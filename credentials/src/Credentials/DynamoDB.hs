{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Credentials.DynamoDB
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Provides the implementation for storage and retrieval of encrypted credentials
-- in DynamoDB. The encryption and decryption is handled by "Credentials.KMS".
--
-- See the "Credentials" module for usage information.
module Credentials.DynamoDB
    ( DynamoTable (..)
    , defaultTable

    -- * Operations
    , setup
    , teardown
    , insert
    , select
    , delete
    , revisions
    ) where

import Control.Exception.Lens
import Control.Lens           hiding (Context)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry

import Credentials.DynamoDB.Item
import Credentials.KMS           as KMS
import Credentials.Types

import Crypto.Hash   (Digest, SHA1)
import Crypto.Random (MonadRandom (..))

import Data.ByteArray.Encoding
import Data.ByteString         (ByteString)
import Data.Conduit            hiding (await)
import Data.List.NonEmpty      (NonEmpty (..))
import Data.Maybe
import Data.Monoid             ((<>))
import Data.Ord
import Data.Text               (Text)
import Data.Time.Clock.POSIX
import Data.Typeable

import Network.AWS
import Network.AWS.Data
import Network.AWS.DynamoDB

import qualified Crypto.Hash         as Crypto
import qualified Data.ByteString     as BS
import qualified Data.Conduit        as C
import qualified Data.Conduit.List   as CL
import qualified Data.HashMap.Strict as Map
import qualified Data.List.NonEmpty  as NE

-- | A DynamoDB table reference.
newtype DynamoTable = DynamoTable { tableName :: Text }
    deriving (Eq, Ord, Show, FromText, ToText, ToByteString, ToLog)

-- | The default DynamoDB table used to store credentials.
defaultTable :: DynamoTable
defaultTable = DynamoTable "credentials"

-- |
insert :: (MonadMask m, MonadRandom m, MonadAWS m, Typeable m)
       => KeyId
       -> Context
       -> Name
       -> ByteString
       -> DynamoTable
       -> m Revision
insert key ctx name plaintext table = do
    ciphertext <- encrypt key ctx name plaintext
    catchResourceNotFound table (insertEncrypted name ciphertext table)

-- |
select :: MonadAWS m
       => Context
       -> Name
       -> Maybe Revision
       -> DynamoTable
       -> m (ByteString, Revision)
select ctx name rev table = do
    (_, (ciphertext, rev')) <-
        catchResourceNotFound table (selectEncrypted name rev table)
    (,rev') <$> decrypt ctx name ciphertext

-- |
delete :: MonadAWS m
       => Name
       -> Maybe Revision
       -> DynamoTable
       -> m ()
delete name rev table@DynamoTable{..} =
    catchResourceNotFound table (maybe deleteAll deleteOne rev)
  where
    deleteOne r = do
        (ver, _) <- selectEncrypted name (Just r) table
        void . send $ deleteItem tableName & diKey .~ encode name <> encode ver

    deleteAll = paginate qry $$ CL.mapM_ (del . view qrsItems)

    qry = queryByName name table
        & qAttributesToGet  ?~ nameField :| [versionField]
        & qScanIndexForward ?~ True
        & qLimit            ?~ 25

    del []     = pure ()
    del (x:ys) = void . send $ batchWriteItem
        & bwiRequestItems .~ [(tableName, f x :| map f xs)]
      where
        f k = writeRequest & wrDeleteRequest ?~ (deleteRequest & drKey .~ k)

        xs | i < 24    = take (i - 1) ys
           | otherwise = ys
          where
            i = length ys

-- |
revisions :: MonadAWS m
          => DynamoTable
          -> Source m (Name, NonEmpty Revision)
revisions table = catchResourceNotFound table $
        paginate (scanTable table)
    =$= CL.concatMapM (traverse decode . view srsItems)
    =$= CL.groupOn1 fst
    =$= CL.map group
  where
    group ((name, rev), revs) =
        ( name
        , desc (rev :| map snd revs)
        )

    desc :: NonEmpty (Version, Revision) -> NonEmpty Revision
    desc = NE.map snd . NE.sortWith (Down . fst)

-- |
setup :: MonadAWS m => DynamoTable -> m Setup
setup table@DynamoTable{..} = do
    p <- exists table
    unless p $ do
        let iops = provisionedThroughput 1 1
            keys = keySchemaElement nameField    Hash
               :| [keySchemaElement versionField Range]
            attr = ctAttributeDefinitions .~
                [ attributeDefinition nameField     S
                , attributeDefinition versionField  N
                , attributeDefinition revisionField B
                ]
            secn = ctLocalSecondaryIndexes .~
                [ localSecondaryIndex revisionIndex
                    (keySchemaElement nameField Hash
                        :| [keySchemaElement revisionField Range])
                    (projection & pProjectionType ?~ All)
                ]
        void $ send (createTable tableName keys iops & attr & secn)
        void $ await tableExists (describeTable tableName)
    pure $
       if p
           then Exists
           else Created

-- |
teardown :: MonadAWS m => DynamoTable -> m ()
teardown table@DynamoTable{..} = do
    p <- exists table
    when p $ do
        void $ send (deleteTable tableName)
        void $ await tableNotExists (describeTable tableName)

insertEncrypted :: (MonadMask m, MonadAWS m, Typeable m)
                => Name
                -> Encrypted
                -> DynamoTable
                -> m Revision
insertEncrypted name encrypted table@DynamoTable{..} =
    recovering policy [const cond] write
  where
    write = const $ do
        ver <- maybe 1 (+1) <$> latest name table
        rev <- genRevision ver
        void . send $ putItem tableName
            & piItem     .~ encode name <> encode ver <> encode rev <> encode encrypted
            & piExpected .~ Map.map (const expect) (encode ver <> encode rev)
        pure rev

    cond = handler_ _ConditionalCheckFailedException (pure True)

    expect = expectedAttributeValue & eavExists ?~ False

    policy = constantDelay 1000 <> limitRetries 5

selectEncrypted :: (MonadThrow m, MonadAWS m)
                => Name
                -> Maybe Revision
                -> DynamoTable
                -> m (Version, (Encrypted, Revision))
selectEncrypted name rev table@DynamoTable{..} =
    send (queryByName name table & revision rev) >>= result
  where
    result = maybe missing decode . listToMaybe . view qrsItems

    missing = throwM $ SecretMissing name rev tableName

    -- If revision is specified, the revisionIndex is used and
    -- a consistent read is done.
    revision Nothing  = id
    revision (Just r) =
          (qIndexName      ?~ revisionIndex)
        . (qKeyConditions  <>~ equals r)
        . (qConsistentRead ?~ True)

latest :: (MonadThrow m, MonadAWS m)
       => Name
       -> DynamoTable
       -> m (Maybe Version)
latest name table = do
    rs <- send (queryByName name table & qConsistentRead ?~ True)
    case listToMaybe (rs ^. qrsItems) of
        Nothing -> pure Nothing
        Just  m -> Just <$> decode m

exists :: MonadAWS m => DynamoTable -> m Bool
exists DynamoTable{..} = paginate listTables
    =$= CL.concatMap (view ltrsTableNames)
     $$ (isJust <$> findC (== tableName))

scanTable :: DynamoTable -> Scan
scanTable DynamoTable{..} =
    scan tableName
        & sAttributesToGet ?~ nameField :| [versionField, revisionField]

queryByName :: Name -> DynamoTable -> Query
queryByName name DynamoTable{..} =
    query tableName
        & qLimit            ?~ 1
        & qScanIndexForward ?~ False
        & qConsistentRead   ?~ False
        & qKeyConditions    .~ equals name

genRevision :: MonadIO m => Version -> m Revision
genRevision (Version ver) = do
    ts <- liftIO getPOSIXTime
    let d = Crypto.hash (toBS (show ts) <> toBS ver) :: Digest SHA1
        r = BS.take 7 (convertToBase Base16 d)
    pure $! Revision r

findC :: Monad m => (a -> Bool) -> Consumer a m (Maybe a)
findC f = loop
  where
    loop = C.await >>= maybe (pure Nothing) go

    go x | f x       = pure (Just x)
         | otherwise = loop

-- FIXME: Over specified due to the coarseness of _ResourceNotFound.
catchResourceNotFound :: MonadCatch m => DynamoTable -> m b -> m b
catchResourceNotFound DynamoTable{..} =
    handling_ _ResourceNotFoundException $
        throwM $ StorageMissing ("Table " <> tableName <> " doesn't exist.")
