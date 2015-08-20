{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

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
import qualified Data.ByteString.Char8    as BS8
import           Data.HashMap.Strict      (HashMap)
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                as Text
import           Data.Typeable
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

newtype Name = Name Text
    deriving (Eq, Ord, Show, FromText, ToLog)

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
    deriving (Eq, Ord, Show, FromText)

instance ToLog Version where
    build (Version n) = build (toInteger n)

data Store
    = Bucket BucketName (Maybe Text)
    | Table  Text
      deriving (Eq)

instance ToLog Store where
    build = \case
        Bucket b p -> "s3://"     <> build b <> "/" <> maybe mempty build p
        Table  t   -> "dynamo://" <> build t

instance Show Store where
    show = BS8.unpack . toBS . build

defaultStore :: Store
defaultStore = Table "credential-store"

data SetupError
    = NotSetup Store
      deriving (Eq, Typeable)

makeClassyPrisms ''SetupError

instance Show SetupError where
    show (NotSetup s) = "Store " ++ show s ++ " doesn't exist."

instance Exception SetupError

instance AsSetupError SomeException where
    _SetupError = exception

data ItemError
    = Invalid Text String
    | Missing Text
      deriving (Eq, Typeable)

makeClassyPrisms ''ItemError

instance Show ItemError where
    show = \case
        Invalid k e -> "Invalid key " ++ Text.unpack k ++ ": " ++ e ++ "."
        Missing k   -> "Missing key " ++ Text.unpack k ++ "."

instance Exception ItemError

instance AsItemError SomeException where
    _ItemError = exception
