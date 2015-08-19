{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

-- |
-- Module      : Network.Credentials
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.Credentials
    (
    -- * Generalised Interface
      setup
    , cleanup

    -- * Re-exported Types
    , module Network.Credentials.Types
    ) where

import           Conduit                          hiding (await)
import           Control.Exception.Lens
import           Control.Lens                     hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Free.Class
import           Control.Monad.Trans.AWS
import           Data.ByteString                  (ByteString)
import           Data.HashMap.Strict              (HashMap)
import           Data.List.NonEmpty               (NonEmpty (..))
import           Data.Maybe
import qualified Data.Text                        as Text
import           Network.AWS                      (Region)
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.DynamoDB
import           Network.AWS.S3
import           Network.AWS.S3                   (BucketName)
import qualified Network.Credentials.Store.Dynamo as DB
import qualified Network.Credentials.Store.S3     as S3
import           Network.Credentials.Types
import           Numeric.Natural

-- data['name'] = name
-- data['version'] = version if version != "" else "1"
-- data['key'] = b64encode(wrapped_key).decode('utf-8')
-- data['contents'] = b64encode(c_text).decode('utf-8')
-- data['hmac'] = b64hmac
--
-- put_item(data=data)

-- Exceptions, Logging, etc?

setup :: (MonadCatch m, MonadFree Command m) => Store -> m Setup
setup = \case
--    Bucket b p -> S3.setup b p
    Table  t   -> DB.setup t
    m -> error (show m)

cleanup :: MonadFree Command m => Store -> m ()
cleanup = \case
--    Bucket b p -> S3.cleanup b p
    Table  t   -> DB.cleanup t
    m -> error (show m)

    -- create kms

    -- secrets = Table.create(table, schema=[
    --     HashKey('name', data_type=STRING),
    --     RangeKey('version', data_type=STRING)
    --     ], throughput={
    --         'read': 1,
    --         'write': 1
    --     }, connection=d_conn)

    -- timeout = 1
    -- while secrets.describe()['Table']['TableStatus'] != "ACTIVE":
    --     print("Waiting for table to be created...")
    --     time.sleep(timeout)
    --     timeout = timeout * 2 if timeout < 8 else timeout
