{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Network.Credentials.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.Credentials.Types where

import           Conduit                 hiding (await)
import           Control.Exception.Lens
import           Control.Lens            hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.AWS
import           Crypto.Hash             (SHA256)
import           Crypto.MAC.HMAC         (HMAC)
import           Data.ByteArray.Encoding
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS8
import           Data.HashMap.Strict     (HashMap)
import           Data.List.NonEmpty      (NonEmpty (..))
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text               as Text
import           Data.Typeable
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.DynamoDB
import           Network.AWS.S3
import           Network.AWS.S3          (BucketName, ObjectVersionId)
import           Numeric.Natural

data Setup
    = Created
    | Exists
      deriving (Eq, Show)

instance ToLog Setup where
    build Created = "created"
    build Exists  = "exists"

newtype Name = Name Text
    deriving (Eq, Ord, Show, FromText, ToText, ToLog)

newtype Value = Value Text
    deriving (FromText)

instance Show Value where
    show = const "Value *****"

newtype KeyId = KeyId Text
    deriving (Eq, ToText, FromText)

instance Show KeyId where
    show = Text.unpack . toText

defaultKeyId :: KeyId
defaultKeyId = KeyId "alias/credential-store"

newtype Key = Key ByteString
    deriving (ToByteString)

newtype Ciphertext = Ciphertext ByteString
    deriving (ToByteString)

newtype HMAC' = HMAC' (HMAC SHA256)

instance ToText HMAC' where
    toText (HMAC' h) = toText (convertToBase Base16 h :: ByteString)

data Secret = Secret Key Ciphertext HMAC'

instance Show Secret where
    show = const "Secret *****"

newtype Context = Context (HashMap Text Text)
    deriving (Show, Monoid)

newtype Version = Version Natural
    deriving (Eq, Ord, Num, Show, FromText, ToText)

instance ToLog Version where
    build (Version n) = build (toInteger n)

-- newtype Table = Table Text
--     deriving (Eq, Ord, Show, IsString, FromText, ToText, ToLog)

data Bucket = Bucket BucketName (Maybe Text)
    deriving (Eq)

data SetupError
    = NotSetup Text
      deriving (Eq, Typeable)

makeClassyPrisms ''SetupError

instance Show SetupError where
    show (NotSetup s) = "Store " ++ show s ++ " doesn't exist."

instance Exception SetupError

instance AsSetupError SomeException where
    _SetupError = exception

data ItemError
    = NotFound Name
    | Invalid  Text String
    | Missing  Text
      deriving (Eq, Typeable)

makeClassyPrisms ''ItemError

instance Show ItemError where
    show = \case
        NotFound n   -> "Not found: "   ++ Text.unpack (toText n) ++ "."
        Invalid  k e -> "Invalid key: " ++ Text.unpack k ++ ", " ++ e ++ "."
        Missing  k   -> "Missing key: " ++ Text.unpack k ++ "."

instance Exception ItemError

instance AsItemError SomeException where
    _ItemError = exception
