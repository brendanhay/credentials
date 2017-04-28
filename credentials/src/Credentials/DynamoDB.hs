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
    (
    -- * Table
      DynamoTable (..)
    , defaultTable

    -- * Operations
    , insert
    , select
    , delete
    , truncate
    , revisions
    , setup
    , teardown
    ) where

import Prelude hiding (truncate)

import Control.Exception.Lens
import Control.Lens           hiding (Context)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry

import Credentials.DynamoDB.Item
import Credentials.KMS           as KMS
import Credentials.Types

import Crypto.Hash (Digest, SHA1)

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
--
-- /Value:/ @credentials@
defaultTable :: DynamoTable
defaultTable = DynamoTable "credentials"

-- | Encrypt and insert a new credential revision with the specified name.
--
-- The newly inserted revision is returned.
insert :: (MonadMask m, MonadAWS m, Typeable m)
       => KeyId       -- ^ The KMS master key ARN or alias.
       -> Context     -- ^ The KMS encryption context.
       -> Name        -- ^ The credential name.
       -> ByteString  -- ^ The unencrypted plaintext.
       -> DynamoTable -- ^ The DynamoDB table.
       -> m Revision
insert key ctx name plaintext table = do
    ciphertext <- encrypt key ctx name plaintext
    catchResourceNotFound table (insertEncrypted name ciphertext table)

-- | Select an existing credential, optionally specifying the revision.
--
-- The decrypted plaintext and selected revision are returned.
select :: MonadAWS m
       => Context        -- ^ The KMS encryption context that was used during insertion.
       -> Name           -- ^ The credential name.
       -> Maybe Revision -- ^ A revision. If 'Nothing', the latest will be selected.
       -> DynamoTable    -- ^ The DynamoDB table.
       -> m (ByteString, Revision)
select ctx name rev table = do
    (_, (ciphertext, rev')) <-
        catchResourceNotFound table (selectEncrypted name rev table)
    (,rev') <$> decrypt ctx name ciphertext

-- | Delete the specific credential revision.
delete :: MonadAWS m
       => Name        -- ^ The credential name.
       -> Revision    -- ^ The revision to delete.
       -> DynamoTable -- ^ The DynamoDB table.
       -> m ()
delete name rev table@DynamoTable{..} =
    catchResourceNotFound table $ do
        (ver, _) <- selectEncrypted name (Just rev) table
        void . send $
            deleteItem tableName
                & diKey .~ toItem name <> toItem ver

-- | Truncate all of a credential's revisions, so that only
-- the latest revision remains.
truncate :: MonadAWS m
         => Name        -- ^ The credential name.
         -> DynamoTable -- ^ The DynamoDB table.
         -> m ()
truncate name table@DynamoTable{..} = catchResourceNotFound table $
    queryAll $$ CL.mapM_ (deleteMany . view qrsItems)
  where
    queryAll =
        paginate $
              queryByName name table
            & qAttributesToGet  ?~ nameField :| [versionField]
            & qScanIndexForward ?~ True
            & qLimit            ?~ batchSize

    deleteMany []     = pure ()
    deleteMany (x:xs) = void . send $
        batchWriteItem
            & bwiRequestItems .~
                [ (tableName, deleteKey x :| map deleteKey (batchInit xs))
                ]

    deleteKey k =
        writeRequest
            & wrDeleteRequest ?~ (deleteRequest & drKey .~ k)

    batchInit xs
        | i < n     = take (i - 1) xs
        | otherwise = xs
      where
        n = fromIntegral (batchSize - 1)
        i = length xs

    batchSize = 50

-- | Scan the entire credential database, grouping pages of results into
-- unique credential names and their corresponding revisions.
revisions :: MonadAWS m
          => DynamoTable -- ^ The DynamoDB table.
          -> Source m (Name, NonEmpty Revision)
revisions table = catchResourceNotFound table $
        paginate (scanTable table)
    =$= CL.concatMapM (traverse fromItem . view srsItems)
    =$= CL.groupOn1 fst
    =$= CL.map group
  where
    group ((name, rev), revs) = (name, desc (rev :| map snd revs))

    desc :: NonEmpty (Version, Revision) -> NonEmpty Revision
    desc = NE.map snd . NE.sortWith (Down . fst)

-- | Create the credentials database table.
--
-- The returned idempotency flag can be used to notify configuration
-- management tools such as ansible whether about system state.
setup :: MonadAWS m
      => DynamoTable -- ^ The DynamoDB table.
      -> m Setup
setup table@DynamoTable{..} = do
    p <- exists table
    unless p $ do
        let iops = provisionedThroughput 1 1
            keys = keySchemaElement nameField    Hash
               :| [keySchemaElement versionField Range]
            attr = ctAttributeDefinitions .~
                [ attributeDefinition nameField     S
                , attributeDefinition versionField  S
                , attributeDefinition revisionField B
                ]
            -- FIXME: Only non-key attributes need to be specified
            -- in the non-key attributes .. duh.
            secn = ctLocalSecondaryIndexes .~
                [ localSecondaryIndex revisionField
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

-- | Delete the credentials database table and all data.
--
-- /Note:/ Unless you have DynamoDB backups running, this is a completely
-- irrevocable action.
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
            & piExpected .~ Map.map (const expect) (toItem ver <> toItem rev)
            & piItem     .~
                   toItem name
                <> toItem ver
                <> toItem rev
                <> toItem encrypted
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
    result = maybe missing fromItem . listToMaybe . view qrsItems

    missing = throwM $ SecretMissing name rev tableName

    -- If revision is specified, the revision index is used and
    -- a consistent read is done.
    revision Nothing  = id
    revision (Just r) =
          (qIndexName      ?~ revisionField)
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
        Just  m -> Just <$> fromItem m

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
