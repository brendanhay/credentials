{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Credentials.Types
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.Types where

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
import qualified Data.HashMap.Strict     as Map
import           Data.List.NonEmpty      (NonEmpty (..))
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text               as Text
import           Data.Typeable
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Numeric.Natural

-- | The KMS master key identifier.
newtype KeyId = KeyId Text
    deriving (Eq, Ord, Show, FromText, ToText, ToByteString, ToLog)

defaultKeyId :: KeyId
defaultKeyId = KeyId "alias/credential-store"

-- | A shared/readable name for a secret.
newtype Name = Name Text
    deriving (Eq, Ord, Show, FromText, ToText, ToByteString, ToLog)

-- | An opaque (non-monotonic, potential gaps) revision number.
newtype Revision = Revision ByteString
    deriving (Eq, Ord, Show, FromText, ToText, ToByteString, ToLog)

-- | An unencrypted secret value.
newtype Value = Value ByteString
    deriving (Eq, Ord, FromText, ToByteString)

instance Show Value where
    show = const "Value *****"

-- | An encryption context.
newtype Context = Context { context :: HashMap Text Text }
    deriving (Eq, Show, Monoid)

blank :: Context -> Bool
blank = Map.null . context

-- | Wrapped key.
newtype Key = Key ByteString deriving (ToByteString)

-- | Encrypted ciphertext.
newtype Cipher = Cipher ByteString deriving (ToByteString)

-- | HMAC SHA256 of the Ciphertext, possibly hex-encoded.
data HMAC256
    = Hex    ByteString
    | Digest (HMAC SHA256)

instance Eq HMAC256 where
    a == b = toBS a == toBS b

instance ToByteString HMAC256 where
    toBS = \case
        Hex    h -> h
        Digest d -> convertToBase Base16 d

instance Show HMAC256 where
    show = BS8.unpack . toBS

-- | An encrypted secret.
data Secret = Secret Key Cipher HMAC256

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

type Revisions = [(Name, NonEmpty Revision)]

class Storage m where
    type Layer m :: * -> *
    data Ref   m :: *

    layer   :: m a -> Layer m a

    setup   ::                           Ref m -> m Setup
    cleanup ::                           Ref m -> m ()
    listAll ::                           Ref m -> m Revisions
    insert  :: Name -> Secret         -> Ref m -> m Revision
    select  :: Name -> Maybe Revision -> Ref m -> m (Secret, Revision)
    delete  :: Name -> Maybe Revision -> Ref m -> m ()

data CredentialError
    = MasterKeyMissing KeyId (Maybe Text)
      -- ^ The specified master key id doesn't exist.

    | IntegrityFailure Name HMAC256 HMAC256
      -- ^ The computed HMAC doesn't matched the stored HMAC.

    | EncryptFailure Context Name Text
      -- ^ Failure occured during local encryption.

    | DecryptFailure Context Name Text
      -- ^ Failure occured during local decryption.

    | StorageMissing Text
      -- ^ Storage doesn't exist, or has gone on holiday.

    | FieldMissing Text [Text]
      -- ^ Missing field from the storage engine.

    | FieldInvalid Text Text
      -- ^ Unable to parse field from the storage engine.

    | SecretMissing Name (Maybe Revision) Text
      -- ^ Secret with the specified name cannot found.

    | OptimisticLockFailure Name Revision Text
      -- ^ Attempting to insert a revision that (already, or now) exists.

      deriving (Eq, Show, Typeable)

instance Exception CredentialError

makeClassyPrisms ''CredentialError

instance AsCredentialError SomeException where
    _CredentialError = exception
