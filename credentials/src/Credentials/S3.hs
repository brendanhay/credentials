{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Credentials.S3
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.S3
    ( S3
    , Ref (S3Bucket)
    , defaultPrefix
    ) where

import           Control.Exception.Lens
import           Control.Lens                    hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS         (AWSConstraint)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Credentials
import           Data.Bifunctor
import           Data.ByteString                 (ByteString)
import           Data.Conduit                    hiding (await)
import qualified Data.Conduit.List               as CL
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NE
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           GHC.Exts
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Body
import           Network.AWS.S3
import           Network.AWS.S3.Encryption
import           Network.AWS.S3.Encryption.Body
import           Network.AWS.S3.Encryption.Types

-- FIXME: store similar envelope on dynamodb credentials, namely, add context/description

newtype S3 a = S3 { runS3 :: AWS a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadAWS S3 where liftAWS = S3

-- FIXME: Revist context/materials for s3-encryption.
instance Storage S3 where
    type Layer S3 = AWS
    type In    S3 = RqBody
    type Out   S3 = RsBody

    data Ref S3 = S3Bucket BucketName (Maybe Text)
        deriving (Eq, Ord, Show)

    layer     = runS3
    setup     = setup'
    cleanup   = cleanup'
    revisions = revisions'
    insert    = insert'
    select    = select'
    delete    = delete_

defaultPrefix :: Maybe Text
defaultPrefix = Just "credential-store"

setup' :: MonadAWS m => Ref S3 -> m Setup
setup' (S3Bucket b _) = do
    r <- liftAWS (view envRegion)
    p <- exists b

    -- FIXME: instead of exists, get the bucket versioning status
    -- so it can be checked to be set correctly.

    unless p $ do
        void . send $ createBucket b
            & cbCreateBucketConfiguration ?~
                (createBucketConfiguration & cbcLocationConstraint ?~ r)
        void $ await bucketExists (headBucket b)

    void . send $ putBucketVersioning b
        (versioningConfiguration
            & vcStatus ?~ BVSEnabled)

    return $ if p then Exists else Created

cleanup' :: MonadAWS m => Ref S3 -> m ()
cleanup' ns@(S3Bucket b _) = do
    p <- exists b
    when p (listAll 200 ns $$ deleteAll b (const True))

revisions' :: MonadAWS m => Ref S3 -> Source m (Name, NonEmpty Revision)
revisions' ns = listAll 200 ns
    =$= CL.concat
    =$= CL.map (_1 %~ strip)
    =$= CL.groupOn1 (view _1)
    =$= CL.map group
  where
    group ((n, r, _), rs) = (name n, rev r :| map (rev . view _2) rs)

    strip = over (keyPrefix '/') (const mempty)

    name :: ObjectKey -> Name
    name = Name . toText

    rev :: ObjectVersionId -> Revision
    rev = Revision . toBS

insert' :: MonadAWS m
        => KeyId
        -> Context
        -> Name
        -> In  S3
        -> Ref S3
        -> m Revision
insert' k c n x (S3Bucket b p) = do
    rs <- withKey k c . encrypt $ putObject b (nameToKey n p) (toBody x)
    maybeRevision n (rs ^. porsVersionId)

select' :: (MonadAWS m)
        => Context
        -> Name
        -> Maybe Revision
        -> Ref S3
        -> m (Out S3, Revision)
select' c n mr (S3Bucket b p) =
    withoutKey c (decrypt (getObject b (nameToKey n p) & revision mr))
        >>= result
  where
    result rs = do
        r <- maybeRevision n (rs ^. gorsVersionId)
        return (rs ^. gorsBody, r)

    revision Nothing  = id
    revision (Just r) = goVersionId ?~ ObjectVersionId (toText r)

delete_ :: MonadAWS m
        => Name
        -> Maybe Revision
        -> Ref S3
        -> m ()
delete_ n r (S3Bucket b p) = case r of
    Just x  -> void . send $
        deleteObject b (nameToKey n p)
            & doVersionId ?~ ObjectVersionId (toText x)
    Nothing ->
        listAll 200 (S3Bucket b (Just (toText (nameToKey n p))))
           $$ deleteAll b not

deleteAll :: MonadAWS m
          => BucketName
          -> (a -> Bool)
          -> Consumer [(ObjectKey, ObjectVersionId, a)] m ()
deleteAll b f = CL.mapM_ (del . mapMaybe key)
  where
    del [] = return ()
    del xs = void . send . deleteObjects b $
        delete' & dQuiet   ?~ True
                & dObjects .~ xs

    key (k, v, x)
        | f x       = Just (objectIdentifier k & oiVersionId ?~ v)
        | otherwise = Nothing

listAll :: MonadAWS m
        => Int
        -> Ref S3
        -> Source m [(ObjectKey, ObjectVersionId, Bool)]
listAll n (S3Bucket b k) = paginate rq
    =$= CL.map (mapMaybe mk . view lovrsVersions)
    =$= CL.filter (not . null)
  where
    rq = listObjectVersions b & lovPrefix .~ k & lovMaxKeys ?~ n

    mk x = (,,)
     <$> x ^. ovKey
     <*> x ^. ovVersionId
     <*> pure (fromMaybe False (x ^. ovIsLatest))

maybeRevision n =
    may (SecretMissing n Nothing "Unable to insert new revision.")
        . fmap (Revision . toBS)

may e Nothing  = throwM e
may _ (Just v) = return v

exists :: MonadAWS m => BucketName -> m Bool
exists b =
    catching_ _ServiceError
        (send (headBucket b) >> return True) (return False)

withoutKey :: MonadAWS m => Context -> ReaderT KeyEnv AWS a -> m a
withoutKey = withKey (KeyId "dummy")

withKey :: MonadAWS m => KeyId -> Context -> ReaderT KeyEnv AWS a -> m a
withKey k c m = liftAWS $ do
    e <- view environment
    runReaderT m . KeyEnv e
        $ kmsKey (toText k)
        & description .~ Description (fromContext c)

nameToKey :: Name -> Maybe Text -> ObjectKey
nameToKey n = \case
    Nothing -> ObjectKey name
    Just p  -> ObjectKey (Text.dropWhileEnd (== '/') p <> "/" <> name)
  where
    name = Text.dropWhile (== '/') (toText n)
