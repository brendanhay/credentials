{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Credentials.DynamoDB
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.DynamoDB
    ( DynamoDB
    , TableName
    , defaultTable
    ) where

import           Control.Exception.Lens
import           Control.Lens             hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Retry
import           Credentials
import           Credentials.DynamoDB.Val
import           "cryptonite" Crypto.Hash
import           Data.ByteArray.Encoding
import qualified Data.ByteString          as BS
import           Data.Conduit             hiding (await)
import qualified Data.Conduit             as C
import qualified Data.Conduit.List        as CL
import qualified Data.HashMap.Strict      as Map
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Data.Maybe
import           Data.Ord
import           Data.Text                (Text)
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.DynamoDB

newtype DynamoDB a = DynamoDB { runDynamo :: AWS a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadAWS DynamoDB where
    liftAWS = DynamoDB

instance Storage DynamoDB where
    type  Layer DynamoDB = AWS
    newtype Ref DynamoDB = TableName Text
        deriving (Eq, Ord, Show, FromText, ToText, ToByteString, ToLog)

    layer        = runDynamo
    setup        = setup'
    cleanup      = cleanup'
    listAll    r = safe r (listAll'    r)
    insert n s r = safe r (insert' n s r)
    select n v r = safe r (select' n v r <&> snd)
    delete n v r = safe r (delete' n v r)

type TableName = Ref DynamoDB

defaultTable :: TableName
defaultTable = TableName "credential-store"

-- FIXME:
-- This is a bit over specified due to the coarseness of _ResourceNotFound.
safe :: (ToText a, MonadCatch m) => a -> m r -> m r
safe t = handling_ _ResourceNotFoundException (throwM err)
  where
    err = StorageMissing ("Table " <> toText t <> " doesn't exist.")

setup' :: MonadAWS m => TableName -> m Setup
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
    return $ if p then Exists else Created

cleanup' :: MonadAWS m => TableName -> m ()
cleanup' t@(toText -> t') = do
    p <- exists t
    when p $ do
        void $ send (deleteTable t')
        void $ await tableNotExists (describeTable t')

listAll' :: (MonadCatch m, MonadAWS m) => TableName -> m Revisions
listAll' t = paginate (mkScan t)
    =$= CL.concatMapM (traverse fromVal . view srsItems)
    =$= CL.groupOn1 fst
    =$= CL.map group
     $$ CL.consume
  where
    group ((k, r), rs) = (k, desc (r :| map snd rs))

    desc :: NonEmpty (Version, Revision) -> NonEmpty Revision
    desc = NE.map snd . NE.sortOn (Down . fst)

insert' :: (MonadIO m, MonadMask m, MonadAWS m, Typeable m)
        => Name
        -> Secret
        -> TableName
        -> m Revision
insert' n s t = recovering policy [const cond] write
  where
    write = do
        v <- maybe 1 (+1) <$> latest n t
        r <- mkRevision v
        void . send $ putItem (toText t)
            & piItem     .~ toVal n <> toVal v <> toVal r <> toVal s
            & piExpected .~ Map.map (const expect) (toVal v <> toVal r)
        return r

    cond = handler_ _ConditionalCheckFailedException (return True)

    expect = expectedAttributeValue & eavExists ?~ False

    policy = constantDelay 1000 <> limitRetries 5

select' :: (MonadThrow m, MonadAWS m)
        => Name
        -> Maybe Revision
        -> TableName
        -> m (Version, (Secret, Revision))
select' n mr t = send (mkNamed n t & revision mr) >>= result
  where
    result  = maybe missing fromVal . listToMaybe . view qrsItems
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
        -> TableName
        -> m ()
delete' n r t =
    case r of
        Just x  -> do
            (v, _) <- select' n (Just x) t
            void . send $
                deleteItem (toText t) & diKey .~ toVal n <> toVal v

        Nothing -> paginate qry $$ CL.mapM_ (del . view qrsItems)
          where
            qry = mkNamed n t
                & qAttributesToGet  ?~ nameField :| [versionField]
                & qScanIndexForward ?~ True
                & qLimit            ?~ 25

            del []     = return ()
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
       -> TableName
       -> m (Maybe Version)
latest n t = do
    rs <- send (mkNamed n t & qConsistentRead ?~ True)
    case listToMaybe (rs ^. qrsItems) of
        Nothing -> pure Nothing
        Just  m -> Just <$> fromVal m

exists :: MonadAWS m => TableName -> m Bool
exists t = paginate listTables
    =$= CL.concatMap (view ltrsTableNames)
     $$ (isJust <$> findC (== toText t))

mkScan :: TableName -> Scan
mkScan t = scan (toText t)
    & sAttributesToGet ?~ nameField :| [versionField, revisionField]

mkNamed :: Name -> TableName -> Query
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
    return $! Revision r

findC :: Monad m => (a -> Bool) -> Consumer a m (Maybe a)
findC f = loop
  where
    loop = C.await >>= maybe (return Nothing) go

    go x | f x       = return (Just x)
         | otherwise = loop
