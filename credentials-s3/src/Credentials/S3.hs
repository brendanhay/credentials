{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Credentials.S3
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.S3 where
    -- ( S3
    -- , BucketStore
    -- , BucketName
    -- ) where

import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Credentials
import           Data.Maybe
import           Data.Ord
import           Data.Text              (Text)
import           GHC.Exts
-- import           "cryptonite" Crypto.Hash
-- import           Data.ByteArray.Encoding
-- import qualified Data.ByteString           as BS
import           Data.Conduit           hiding (await)
-- import qualified Data.Conduit              as C
import qualified Data.Conduit.List      as CL
-- import qualified Data.HashMap.Strict       as Map
import           Data.List.NonEmpty     (NonEmpty (..))
import qualified Data.List.NonEmpty     as NE
-- import           Data.Maybe
-- import           Data.Ord
-- import           Data.Text                 (Text)
-- import           Data.Time.Clock.POSIX
-- import           Data.Typeable
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.S3

newtype S3 a = S3 { runS3 :: AWS a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadAWS S3 where
    liftAWS = S3

instance Storage S3 where
    type Layer S3 = AWS
    data Ref   S3 = S3Bucket BucketName (Maybe Text)
        deriving (Eq, Ord, Show)

    layer   = runS3
    setup   = setup'
    cleanup = cleanup'
    listAll = listAll'

--    insert n s r = safe r (insert' n s r)
--     select n v r = safe r (select' n v r <&> snd)
--     delete n v r = safe r (delete' n v r)

type S3Bucket = Ref S3

setup' :: MonadAWS m => S3Bucket -> m Setup
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

cleanup' :: MonadAWS m => S3Bucket -> m ()
cleanup' ns@(S3Bucket b _) = do
    p <- exists b
    when p $ revisions 200 ns $$ CL.mapM_ (void . send . del)
  where
    del xs = deleteObjects b $
        delete' & dQuiet   ?~ True
                & dObjects .~ map key xs

    key (k, v) = objectIdentifier k & oiVersionId ?~ v

listAll' :: MonadAWS m => S3Bucket -> Source m (Name, NonEmpty Revision)
listAll' ns = revisions 200 ns
    =$= CL.concat
    =$= CL.groupOn1 fst
    =$= CL.map group
  where
    group ((n, r), rs) = (name n, rev r :| map (rev . snd) rs)

    name = Name     . toText
    rev  = Revision . toBS

insert' :: MonadAWS m
        => Name
        -> Secret
        -> S3Bucket
        -> m Revision
insert' n s ns = undefined -- do
--    putObject

  -- where
  --   write = do
  --       r <- mkRevision v
  --       void . send $ putItem (toText t)
  --           & piItem     .~ encode n <> encode v <> encode r <> encode s
  --           & piExpected .~ Map.map (const expect) (encode v <> encode r)
  --       return r

  --   cond = handler_ _ConditionalCheckFailedException (return True)

  --   expect = expectedAttributeValue & eavExists ?~ False

  --   policy = constantDelay 1000 <> limitRetries 5

-- ovKey
-- ovVersionId
-- ovIsLatest

revisions :: MonadAWS m
          => Int
          -> S3Bucket
          -> Source m [(ObjectKey, ObjectVersionId)]
revisions n (S3Bucket b k) = paginate rq
    =$= CL.map (mapMaybe mk . view lovrsVersions)
    =$= CL.filter (not . null)
  where
    rq = listObjectVersions b & lovPrefix .~ k & lovMaxKeys ?~ n

    mk x = (,) <$> x ^. ovKey <*> x ^. ovVersionId

exists :: MonadAWS m => BucketName -> m Bool
exists b =
    catching_ _ServiceError
        (send (headBucket b) >> return True) (return False)
