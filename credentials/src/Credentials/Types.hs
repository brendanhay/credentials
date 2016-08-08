{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Credentials.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.Types where

import Control.Exception.Lens (exception)
import Control.Lens           hiding (Context)
import Control.Monad.Catch    (Exception, SomeException)

import Crypto.Cipher.AES   (AES256)
import Crypto.Cipher.Types (IV)
import Crypto.Hash         (Digest, SHA256)
import Crypto.MAC.HMAC     (HMAC)

import Data.ByteString     (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text           (Text)
import Data.Typeable       (Typeable)

import Network.AWS.Data

-- | The KMS master key identifier.
newtype KeyId = KeyId Text
    deriving (Eq, Ord, Show, FromText, ToText, ToByteString, ToLog)

-- | The default KMS master key alias.
--
-- /Value:/ @alias\/credentials@
defaultKeyId :: KeyId
defaultKeyId = KeyId "alias/credentials"

-- | A shared/readable name for a secret.
newtype Name = Name Text
    deriving (Eq, Ord, Show, FromText, ToText, ToByteString, ToLog)

-- | An opaque, non-monotonic revision number.
newtype Revision = Revision ByteString
    deriving (Eq, Ord, Show, FromText, ToText, ToByteString, ToLog)

-- | A KMS encryption context.
--
-- /See:/ KMS <http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html Encryption Context>
-- documentation for more information.
newtype Context = Context { fromContext :: HashMap Text Text }
    deriving (Eq, Show, Monoid)

-- | The encryption parameters required to perform decryption.
data Encrypted = Encrypted
    { iv         :: !(IV AES256)   -- ^ The block cipher initialisation vector.
    , wrappedKey :: !ByteString    -- ^ The wrapped (encrypted) data encryption key.
    , ciphertext :: !ByteString    -- ^ The encrypted ciphertext.
    , digest     :: !(HMAC SHA256) -- ^ HMAC SHA256 digest of the ciphertext.
    }

-- | Denotes idempotency of an action. That is, whether an action resulted
-- in any setup being performed.
data Setup
    = Created
    | Exists
      deriving (Eq, Show)

instance ToText Setup where
    toText = \case
        Created -> "created"
        Exists  -> "exists"

instance ToLog Setup where
    build = build . toText

data CredentialError
    = MasterKeyMissing KeyId (Maybe Text)
      -- ^ The specified master key id doesn't exist.

    | IntegrityFailure Name ByteString ByteString
      -- ^ The computed HMAC doesn't matched the stored HMAC.

    | EncryptFailure Context Name Text
      -- ^ Failure occured during local encryption.

    | DecryptFailure Context Name Text
      -- ^ Failure occured during local decryption.

    | StorageMissing Text
      -- ^ Storage doesn't exist, or has gone on holiday.

    | StorageFailure Text
      -- ^ Some storage pre-condition wasn't met.
      -- For example: DynamoDB column size exceeded.

    | FieldMissing Text [Text]
      -- ^ Missing field from the storage engine.

    | FieldInvalid Text String
      -- ^ Unable to parse field from the storage engine.

    | SecretMissing Name (Maybe Revision) Text
      -- ^ Secret with the specified name cannot found.

    | OptimisticLockFailure Name Revision Text
      -- ^ Attempting to insert a revision that already exists.

      deriving (Eq, Show, Typeable)

instance Exception CredentialError

makeClassyPrisms ''CredentialError

instance AsCredentialError SomeException where
    _CredentialError = exception
