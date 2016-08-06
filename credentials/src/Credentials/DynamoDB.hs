{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Credentials.DynamoDB
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.DynamoDB
    ( DynamoDB
    , Ref (DynamoTable)
    , defaultTable
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

import Crypto.Hash
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

import qualified Data.ByteString     as BS
import qualified Data.Conduit        as C
import qualified Data.Conduit.List   as CL
import qualified Data.HashMap.Strict as Map
import qualified Data.List.NonEmpty  as NE

newtype DynamoDB a = DynamoDB { runDynamo :: AWS a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadMask
        )

instance MonadAWS DynamoDB where
    liftAWS = DynamoDB

instance MonadRandom DynamoDB where
    getRandomBytes = liftIO . getRandomBytes

instance Storage DynamoDB where
    type Layer DynamoDB = AWS
    type In    DynamoDB = ByteString
    type Out   DynamoDB = ByteString

    newtype Ref DynamoDB = DynamoTable Text
        deriving (Eq, Ord, Show, FromText, ToText, ToByteString, ToLog)

    layer        = runDynamo
    setup        = setup'
    destroy      = destroy'
    revisions  r = safe r (revisions'  r)
    delete n v r = safe r (delete' n v r)

    insert k c n v r = do
        x <- encrypt k c n v
        safe r (insert' n x r)

    select c n v r = do
        (_, (x, y)) <- safe r (select' n v r)
        (,y) <$> decrypt c n x

defaultTable :: Ref DynamoDB
defaultTable = DynamoTable "credentials"

-- FIXME:
-- This is a bit over specified due to the coarseness of _ResourceNotFound.
safe :: (ToText a, MonadCatch m) => a -> m b -> m b
safe t = handling_ _ResourceNotFoundException (throwM err)
  where
    err = StorageMissing ("Table " <> toText t <> " doesn't exist.")

setup' :: MonadAWS m => Ref DynamoDB -> m Setup
setup' t@(toText -> t') = do
    p <- exists t
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
        void $ send (createTable t' keys iops & attr & secn)
        void $ await tableExists (describeTable t')
    pure $ if p then Exists else Created

destroy' :: MonadAWS m => Ref DynamoDB -> m ()
destroy' t@(toText -> t') = do
    p <- exists t
    when p $ do
        void $ send (deleteTable t')
        void $ await tableNotExists (describeTable t')

revisions' :: (MonadCatch m, MonadAWS m)
           => Ref DynamoDB
           -> Source m (Name, NonEmpty Revision)
revisions' t = paginate (mkScan t)
    =$= CL.concatMapM (traverse decode . view srsItems)
    =$= CL.groupOn1 fst
    =$= CL.map group
  where
    group ((k, r), rs) = (k, desc (r :| map snd rs))

    desc :: NonEmpty (Version, Revision) -> NonEmpty Revision
    desc = NE.map snd . NE.sortWith (Down . fst)

insert' :: forall m. (MonadIO m, MonadMask m, MonadAWS m, Typeable m)
        => Name
        -> Encrypted
        -> Ref DynamoDB
        -> m Revision
insert' n s t = recovering policy [const cond] write
  where
    write = const $ do
        v <- maybe 1 (+1) <$> latest n t
        r <- mkRevision v
        void . send $ putItem (toText t)
            & piItem     .~ encode n <> encode v <> encode r <> encode s
            & piExpected .~ Map.map (const expect) (encode v <> encode r)
        pure r

    cond = handler_ _ConditionalCheckFailedException (pure True)

    expect = expectedAttributeValue & eavExists ?~ False

    policy = constantDelay 1000 <> limitRetries 5

select' :: (MonadThrow m, MonadAWS m)
        => Name
        -> Maybe Revision
        -> Ref DynamoDB
        -> m (Version, (Encrypted, Revision))
select' n mr t = send (mkNamed n t & revision mr) >>= result
  where
    result  = maybe missing decode . listToMaybe . view qrsItems
    missing = throwM $ SecretMissing n mr (toText t)

    -- If revision is specified, the revisionIndex is used and
    -- a consistent read is done.
    revision Nothing  = id
    revision (Just r) =
          (qIndexName      ?~ revisionIndex)
        . (qKeyConditions  <>~ equals r)
        . (qConsistentRead ?~ True)

-- FIXME: revisit.
delete' :: MonadAWS m
        => Name
        -> Maybe Revision
        -> Ref DynamoDB
        -> m ()
delete' n r t = case r of
    Just x  -> do
        (v, _) <- select' n (Just x) t
        void . send $
            deleteItem (toText t) & diKey .~ encode n <> encode v
    Nothing -> paginate qry $$ CL.mapM_ (del . view qrsItems)
  where
    qry = mkNamed n t
        & qAttributesToGet  ?~ nameField :| [versionField]
        & qScanIndexForward ?~ True
        & qLimit            ?~ 25

    del []     = pure ()
    del (x:ys) = void . send $ batchWriteItem
        & bwiRequestItems .~ [(toText t, f x :| map f xs)]
      where
        f k = writeRequest & wrDeleteRequest ?~ (deleteRequest & drKey .~ k)

        xs | i < 24    = take (i - 1) ys
           | otherwise = ys
          where
            i = length ys

latest :: (MonadThrow m, MonadAWS m)
       => Name
       -> Ref DynamoDB
       -> m (Maybe Version)
latest n t = do
    rs <- send (mkNamed n t & qConsistentRead ?~ True)
    case listToMaybe (rs ^. qrsItems) of
        Nothing -> pure Nothing
        Just  m -> Just <$> decode m

exists :: MonadAWS m => Ref DynamoDB -> m Bool
exists t = paginate listTables
    =$= CL.concatMap (view ltrsTableNames)
     $$ (isJust <$> findC (== toText t))

mkScan :: Ref DynamoDB -> Scan
mkScan t = scan (toText t)
    & sAttributesToGet ?~ nameField :| [versionField, revisionField]

mkNamed :: Name -> Ref DynamoDB -> Query
mkNamed n t = query (toText t)
    & qLimit            ?~ 1
    & qScanIndexForward ?~ False
    & qConsistentRead   ?~ False
    & qKeyConditions    .~ equals n

mkRevision :: MonadIO m => Version -> m Revision
mkRevision v = do
    ts <- liftIO getPOSIXTime
    let d = hash (toBS (show ts) <> toBS v) :: Digest SHA1
        r = BS.take 7 (convertToBase Base16 d)
    pure $! Revision r

findC :: Monad m => (a -> Bool) -> Consumer a m (Maybe a)
findC f = loop
  where
    loop = C.await >>= maybe (pure Nothing) go

    go x | f x       = pure (Just x)
         | otherwise = loop
