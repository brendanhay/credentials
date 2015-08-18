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
module Network.Credentials where

import           Conduit                  hiding (await)
import           Control.Lens             hiding (Context)
import           Control.Monad.Free.Class
import           Control.Monad.Trans.AWS
import           Data.ByteString          (ByteString)
import           Data.HashMap.Strict      (HashMap)
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Maybe
import qualified Data.Text                as Text
import           Network.AWS              (Region)
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.DynamoDB
import           Network.AWS.Free
import           Network.AWS.S3           (BucketName, ObjectVersionId)
import           Numeric.Natural

newtype Key = Key Text
    deriving (Eq, Show, FromText)

data Value
    = Crypt !ByteString
    | Raw   !Text

instance Show Value where
    show = const "Value *****"

instance FromText Value where
    parser = Raw <$> Network.AWS.Data.parser

newtype Context = Context (HashMap Text Text)
    deriving (Show)

newtype Version = Version Natural
    deriving (Show, FromText)

data Store
    = Bucket BucketName (Maybe Text)
    | Table  Text
      deriving (Show)

-- Maybe monadlogger or something?

defaultStore :: Store
defaultStore = Table "credential-store"

schema :: NonEmpty KeySchemaElement
schema = keySchemaElement "name" Hash :| [keySchemaElement "version" Range]

attrs :: [AttributeDefinition]
attrs = [attributeDefinition "name" S, attributeDefinition "version" N]

iops :: ProvisionedThroughput
iops = provisionedThroughput 1 1

-- Exceptions, Logging, etc?

data Setup
    = Created
    | Exists
      deriving (Show)

setup :: MonadFree Command m => Store -> m Setup
setup = \case
    Bucket b p -> s3 b p
    Table  t   -> db t
  where
    s3 bkt pre = do
        -- check if bucket exists

        -- create bucket
        return ()

    db name = do
        -- check if table exists
        x <- paginate listTables
            =$= concatMapC (view ltrsTableNames)
             $$ findC (== name)

        if isJust x
            then return Exists
            else do
                -- create table
                -- _ <- send $ createTable name schema iops
                --     & ctAttributeDefinitions .~ attrs

                -- wait for table
                _ <- await tableExists (describeTable name)

                return Created

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
