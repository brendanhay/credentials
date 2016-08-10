{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
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
import Control.Lens           (Prism', prism)
import Control.Monad.Catch    (Exception, SomeException)

import Crypto.Hash     (SHA256)
import Crypto.MAC.HMAC (HMAC)

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
    { wrappedKey :: !ByteString    -- ^ The wrapped (encrypted) data encryption key.
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

class AsCredentialError a where
    _CredentialError       :: Prism' a CredentialError
    _MasterKeyMissing      :: Prism' a (KeyId, Maybe Text)
    _IntegrityFailure      :: Prism' a (Name, ByteString, ByteString)
    _EncryptFailure        :: Prism' a (Context, Name, Text)
    _DecryptFailure        :: Prism' a (Context, Name, Text)
    _StorageMissing        :: Prism' a Text
    _StorageFailure        :: Prism' a Text
    _FieldMissing          :: Prism' a (Text, [Text])
    _FieldInvalid          :: Prism' a (Text, String)
    _SecretMissing         :: Prism' a (Name, Maybe Revision, Text)
    _OptimisticLockFailure :: Prism' a (Name, Revision, Text)

    _MasterKeyMissing      = (.) _CredentialError _MasterKeyMissing
    _IntegrityFailure      = (.) _CredentialError _IntegrityFailure
    _EncryptFailure        = (.) _CredentialError _EncryptFailure
    _DecryptFailure        = (.) _CredentialError _DecryptFailure
    _StorageMissing        = (.) _CredentialError _StorageMissing
    _StorageFailure        = (.) _CredentialError _StorageFailure
    _FieldMissing          = (.) _CredentialError _FieldMissing
    _FieldInvalid          = (.) _CredentialError _FieldInvalid
    _SecretMissing         = (.) _CredentialError _SecretMissing
    _OptimisticLockFailure = (.) _CredentialError _OptimisticLockFailure

instance AsCredentialError CredentialError where
    _CredentialError = id

    _MasterKeyMissing = prism
        (\(key, msg) -> MasterKeyMissing key msg)
        (\case
            MasterKeyMissing key msg -> Right (key, msg)
            x                        -> Left x)

    _IntegrityFailure = prism
        (\(name, a, b) -> IntegrityFailure name a b)
        (\case
            IntegrityFailure name a b -> Right (name, a, b)
            x                         -> Left x)

    _EncryptFailure = prism
        (\(ctx, name, msg) -> EncryptFailure ctx name msg)
        (\case
            EncryptFailure ctx name msg -> Right (ctx, name, msg)
            x                           -> Left x)

    _DecryptFailure = prism
        (\(ctx, name, msg) -> DecryptFailure ctx name msg)
        (\case
            DecryptFailure ctx name msg -> Right (ctx, name, msg)
            x                           -> Left x)

    _StorageMissing = prism
        StorageMissing
        (\case
            StorageMissing msg -> Right msg
            x                  -> Left x)

    _StorageFailure = prism
        StorageFailure
        (\case
            StorageFailure msg -> Right msg
            x                  -> Left x)

    _FieldMissing = prism
        (\(field, found) -> FieldMissing field found)
        (\case
            FieldMissing field found -> Right (field, found)
            x                        -> Left x)

    _FieldInvalid = prism
        (\(field, msg) -> FieldInvalid field msg)
        (\case
            FieldInvalid field msg -> Right (field, msg)
            x                      -> Left x)

    _SecretMissing = prism
        (\(name, rev, msg) -> SecretMissing name rev msg)
        (\case
            SecretMissing name rev msg -> Right (name, rev, msg)
            x                          -> Left x)

    _OptimisticLockFailure = prism
        (\(name, rev, msg) -> OptimisticLockFailure name rev msg)
        (\case
            OptimisticLockFailure name rev msg -> Right (name, rev, msg)
            x                                  -> Left x)

instance AsCredentialError SomeException where
    _CredentialError = exception
