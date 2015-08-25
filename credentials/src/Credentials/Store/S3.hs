{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Credentials.Store.S3
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.Store.S3 where

import           Conduit                   hiding (await)
import           Control.Exception.Lens
import           Control.Lens              hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Data.ByteString           (ByteString)
import           Data.HashMap.Strict       (HashMap)
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Maybe
import qualified Data.Text                 as Text
import           Network.AWS               (Region)
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.DynamoDB
import           Network.AWS.S3
import           Network.AWS.S3            (BucketName, ObjectVersionId)
import           Credentials.Types
import           Numeric.Natural

-- instance SecretStore m (BucketName, Maybe Text) where

setup' :: (MonadCatch m, MonadAWS m) => BucketName -> Maybe Text -> m Setup
setup' bkt pre = do
    x <- catching_ _NoSuchBucket
        (send (headBucket bkt) >> return Exists)
        (return Created)

    when (x == Created) $ do
        void $ send (createBucket bkt)
        void $ await bucketExists (headBucket bkt)

    return x