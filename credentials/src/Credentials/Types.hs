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

import Crypto.Hash     (SHA256)
import Crypto.MAC.HMAC (HMAC)

import Data.ByteArray          (Bytes)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString         (ByteString)
import Data.Conduit            (Source)
import Data.HashMap.Strict     (HashMap)
import Data.List.NonEmpty      (NonEmpty (..))
import Data.Text               (Text)
import Data.Typeable           (Typeable)

import Network.AWS.Data

import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict   as Map

authTagLength, nonceLength :: Int
authTagLength = 16
nonceLength   = 16

-- FIXME: encode as base 16/64 in dynamodb.

-- | AES128 CTR mode block cipher initialisation vector.
newtype Nonce = Nonce ByteString
    deriving (ToByteString)

-- | The KMS master key identifier.
newtype KeyId = KeyId Text
    deriving (Eq, Ord, Show, FromText, ToText, ToByteString, ToLog)

defaultKeyId :: KeyId
defaultKeyId = KeyId "alias/credentials"

-- | A shared/readable name for a secret.
newtype Name = Name Text
    deriving (Eq, Ord, Show, FromText, ToText, ToByteString, ToLog)

-- | An opaque (non-monotonic, potential gaps) revision number.
newtype Revision = Revision ByteString
    deriving (Eq, Ord, Show, FromText, ToText, ToByteString, ToLog)

-- | A KMS encryption context.
newtype Context = Context { fromContext :: HashMap Text Text }
    deriving (Eq, Show, Monoid)

blankContext :: Context -> Bool
blankContext = Map.null . fromContext

-- | HMAC SHA256, possibly hex-encoded.
data HMAC256
    = Hex    !ByteString
    | Digest !(HMAC SHA256)

instance Eq HMAC256 where
    a == b = toBS a == toBS b

instance ToByteString HMAC256 where
    toBS = \case
        Hex    h -> h
        Digest d -> convertToBase Base16 d

instance Show HMAC256 where
    show = BS8.unpack . toBS

-- | Wrapped key.
newtype Key = Key ByteString deriving (ToByteString)

-- | Encrypted ciphertext.
newtype Cipher = Cipher ByteString deriving (ToByteString)

-- | An encrypted secret.
data Encrypted = Encrypted !Nonce !Key !Cipher !HMAC256

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

class Monad m => Storage m where
    -- | The underlying storage layer.
    type Layer m :: * -> *

    -- | A reference to the storage engine, such as a table or bucket name.
    data Ref m :: *

    type In  m :: *
    type Out m :: *

    layer     :: m a -> Layer m a

    setup     :: Ref m -> m Setup
    cleanup   :: Ref m -> m ()
    revisions :: Ref m -> Source m (Name, NonEmpty Revision)

    delete :: Name  -> Maybe Revision                    -> Ref m -> m ()
    insert :: KeyId -> Context -> Name -> In m           -> Ref m -> m Revision
    select ::          Context -> Name -> Maybe Revision -> Ref m -> m (Out m, Revision)

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

    | StorageFailure Text
      -- ^ Some storage pre-condition wasn't met.
      -- For example: DynamoDB column size exceeded.

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
