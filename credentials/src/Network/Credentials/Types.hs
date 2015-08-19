{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Module      : Network.Credentials.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.Credentials.Types where

import           Conduit                  hiding (await)
import           Control.Exception.Lens
import           Control.Lens             hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Free.Class
import           Control.Monad.Trans.AWS
import           Data.ByteString          (ByteString)
import           Data.HashMap.Strict      (HashMap)
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                as Text
import           Network.AWS              (Region)
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.DynamoDB
import           Network.AWS.S3
import           Network.AWS.S3           (BucketName, ObjectVersionId)
import           Numeric.Natural

data Setup
    = Created
    | Exists
      deriving (Eq, Show)

instance ToLog Setup where
    build Created = "created"
    build Exists  = "exists"

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

instance ToLog Store where
    build (Bucket b p) = "s3://"       <> build b <> "/" <> maybe mempty build p
    build (Table  t)   = "dynamodb://" <> build t

defaultStore :: Store
defaultStore = Table "credential-store"
