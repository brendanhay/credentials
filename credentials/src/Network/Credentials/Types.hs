{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
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
import           Data.ByteArray
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

newtype KeyId = KeyId Text
    deriving (Eq, Ord, Show, FromText, ToText, ToLog)

newtype Name = Name Text
    deriving (Eq, Ord, Show, FromText, ToText, ToLog)

newtype Value = Value ByteString
    deriving (Eq, Ord, FromText, ToByteString)

defaultKeyId :: KeyId
defaultKeyId = KeyId "alias/credential-store"

-- | Wrapped Key.
newtype Key = Key ByteString deriving (ToByteString)

-- | Encrypted Ciphertext.
newtype Cipher = Cipher ByteString deriving (ToByteString)

-- | HMAC SHA256 of the Ciphertext.
data HMAC256
    = Hex    ByteString
    | Digest (HMAC SHA256)

instance Eq HMAC256 where
    a == b = toBS a == toBS b

instance ToByteString HMAC256 where
    toBS = \case
        Hex    h -> h
        Digest d -> convertToBase Base16 d

data Secret = Secret Key Cipher HMAC256

newtype Context = Context (HashMap Text Text)
    deriving (Eq, Show, Monoid)

newtype Version = Version Natural
    deriving (Eq, Ord, Num, Show, FromText, ToText)

instance ToLog Version where
    build (Version n) = build (toInteger n)

data Bucket = Bucket BucketName (Maybe Text)
    deriving (Eq)

defaultTable :: Table
defaultTable = Table "credential-store"

newtype Table = Table Text
    deriving (Eq, Ord, Show, FromText, ToText, ToLog)

data CredentialError
-- if context is None:
--     msg = ("Could not decrypt hmac key with KMS. The credential may "
--            "require that an encryption context be provided to decrypt "
--            "it.")
-- else:
--     msg = ("Could not decrypt hmac key with KMS. The encryption "
--            "context provided may not match the one used when the "
--            "credential was stored.")

    = IntegrityFailure Name (HMAC SHA256) (HMAC SHA256)
      -- ^ The computed HMAC doesn't matched the stored HMAC.

-- if hmac.hexdigest() != material['hmac']:
--         raise IntegrityError("Computed HMAC on %s does not match stored HMAC"

    | EncryptFailure Name Context Text
      -- ^ Failure occured during local encryption.

    | DecryptFailure Name Context Text
      -- ^ Failure occured during local decryption.

    | StorageMissing Text
      -- ^ Storage doesn't exist, or has gone on holiday.

    | FieldMissing Text [Text]
      -- ^ Missing field from the storage engine.

    | FieldInvalid Text Text
      -- ^ Unable to parse field from the storage engine.

    | SecretMissing Name Text
      -- ^ Secret with the specified name cannot found.

    | OptimisticLockFailure Name Version Text
      -- ^ Attempting to insert a version that (already, or now) exists.

      deriving (Eq)

instance Show CredentialError where
    show = const "foo"

--  except ConditionalCheckFailedException:
--                 latestVersion = getHighestVersion(args.credential, region,
--                                                   args.table)
--                 printStdErr("%s version %s is already in the credential store."
--                             "Use the -v flag to specify a new version" %
--                             (args.credential, latestVersion))

-- try:
--         kms_response = kms.generate_data_key(KeyId=kms_key, EncryptionContext=context, NumberOfBytes=64)
--     except:
--         raise KmsError("Could not generate key using KMS key %s" % kms_key)

instance Exception CredentialError

-- data SetupError
--     = NotSetup Text
--       deriving (Eq, Typeable)

-- makeClassyPrisms ''SetupError

-- instance Show SetupError where
--     show (NotSetup s) = "Store " ++ show s ++ " doesn't exist."

-- instance Exception SetupError

-- instance AsSetupError SomeException where
--     _SetupError = exception

-- data ItemError
--     = NotFound Name
--     | Invalid  Text String
--     | Missing  Text
--       deriving (Eq, Typeable)

-- makeClassyPrisms ''ItemError

-- instance Show ItemError where
--     show = \case
--         NotFound n   -> "Not found: "   ++ Text.unpack (toText n) ++ "."
--         Invalid  k e -> "Invalid key: " ++ Text.unpack k ++ ", " ++ e ++ "."
--         Missing  k   -> "Missing key: " ++ Text.unpack k ++ "."

-- instance Exception ItemError

-- instance AsItemError SomeException where
--     _ItemError = exception

-- consider splitting into separate libraries:
--     credentials-dynamodb
--     credentials-s3
--     credentials-sqlite
--     credentials-file
--     credentials-redis ! with tw's redis-io
