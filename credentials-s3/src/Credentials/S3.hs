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
import           Data.Text              (Text)
-- import           "cryptonite" Crypto.Hash
-- import           Data.ByteArray.Encoding
-- import qualified Data.ByteString           as BS
-- import           Data.Conduit              hiding (await)
-- import qualified Data.Conduit              as C
-- import qualified Data.Conduit.List         as CL
-- import qualified Data.HashMap.Strict       as Map
-- import           Data.List.NonEmpty        (NonEmpty (..))
-- import qualified Data.List.NonEmpty        as NE
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
    data Ref   S3 = BucketNS BucketName (Maybe Text)
        deriving (Eq, Ord, Show)

    layer        = runS3
    setup        = setup'
--     cleanup      = cleanup'
--     listAll    r = safe r (listAll'    r)
--     insert n s r = safe r (insert' n s r)
--     select n v r = safe r (select' n v r <&> snd)
--     delete n v r = safe r (delete' n v r)

type BucketNS = Ref S3

setup' :: MonadAWS m => BucketNS -> m Setup
setup' (BucketNS b _) = do
    r <- liftAWS (view envRegion)
    p <- catching_ _ServiceError (send (headBucket b) >> return True) (return False)
    unless p $ do
        void . send $ createBucket b
            & cbCreateBucketConfiguration ?~
                (createBucketConfiguration & cbcLocationConstraint ?~ r)
        void $ await bucketExists (headBucket b)
    return $ if p then Exists else Created
