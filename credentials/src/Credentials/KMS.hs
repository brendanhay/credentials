{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Credentials.KMS
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Encryption and decryption of local data, by using a wrapped key mechanism
-- and master keys stored in KMS.
--
-- See the "Credentials" module for usage information.
module Credentials.KMS
    ( encrypt
    , decrypt
    ) where

import Control.Exception.Lens (catching_, handler)
import Control.Lens           hiding (Context)
import Control.Monad
import Control.Monad.Catch    (Exception, MonadThrow (..), catches)

import Credentials.Types

import Crypto.Cipher.AES   (AES256)
import Crypto.Cipher.Types (nullIV)
import Crypto.Error
import Crypto.MAC.HMAC     (HMAC (..), hmac)

import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString         (ByteString)
import Data.Text               (Text)
import Data.Typeable           (Typeable)

import Network.AWS
import Network.AWS.Data
import Network.AWS.Error (hasCode, hasStatus)
import Network.AWS.KMS   hiding (decrypt, encrypt)

import Numeric.Natural (Natural)

import qualified Crypto.Cipher.Types as Cipher
import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as Text
import qualified Network.AWS.KMS     as KMS

-- | Encrypt a plaintext 'ByteString' with the given master key and
-- encryption context. The 'Name' is used to annotate error messages.
--
-- The wrapped data encryption key, ciphertext, and HMAC SHA256 are returned
-- if no error occurs.
encrypt :: (MonadAWS m, Typeable m)
        => KeyId
        -> Context
        -> Name
        -> ByteString
        -> m Encrypted
encrypt key ctx name plaintext = do
    let rq = generateDataKey (toText key)
           & gdkNumberOfBytes     ?~ keyLength
           & gdkEncryptionContext .~ fromContext ctx

    rs <- catches (send rq)
        [ handler (_ServiceError . hasStatus 400 . hasCode "NotFound") $
            throwM . MasterKeyMissing key . fmap toText . _serviceMessage
        , handler _NotFoundException $
            throwM . MasterKeyMissing key . fmap toText . _serviceMessage
        ]

    let (dataKey, hmacKey) = splitKey (rs ^. gdkrsPlaintext)
        failure            = EncryptFailure ctx name

    aes :: AES256 <- cryptoError failure (Cipher.cipherInit dataKey)

    let !wrappedKey = rs ^. gdkrsCiphertextBlob
        !ciphertext = Cipher.ctrCombine aes nullIV plaintext
        !digest     = hmac hmacKey ciphertext

    pure $! Encrypted{..}

-- | Decrypt ciphertext using the given encryption context, and wrapped
-- data encryption key. The HMAC SHA256 is recalculated and compared for
-- message integrity. The 'Name' is used to annotate error messages.
--
-- The resulting unencrypted plaintext 'ByteString' is returned if no error occurs.
decrypt :: MonadAWS m
        => Context
        -> Name
        -> Encrypted
        -> m ByteString
decrypt ctx name Encrypted{..} = do
    let rq = KMS.decrypt wrappedKey
           & decEncryptionContext .~ fromContext ctx

    rs <- catching_ _InvalidCiphertextException (send rq) $
        throwM . DecryptFailure ctx name $
            if Map.null (fromContext ctx)
                then "Could not decrypt stored key using KMS. \
                     \The credential may require an ecryption context."
                else  "Could not decrypt stored key using KMS. \
                     \The provided encryption context may not match the one \
                     \used when the credential was stored."

    -- Decrypted plaintext data. This value may not be returned if the customer
    -- master key is not available or if you didn't have permission to use it.
    plaintextKey <-
        case rs ^. drsPlaintext of
            Nothing -> throwM $
                DecryptFailure ctx name
                    "Decrypted plaintext data not available from KMS."
            Just  t -> pure t

    let (dataKey, hmacKey) = splitKey plaintextKey
        expect             = hmac hmacKey (toBS ciphertext)
        failure            = DecryptFailure ctx name

    unless (expect == digest) $
        throwM (IntegrityFailure name (encodeHex expect) (encodeHex digest))

    aes :: AES256 <- cryptoError failure (Cipher.cipherInit dataKey)

    pure $! Cipher.ctrCombine aes nullIV (toBS ciphertext)

splitKey :: ByteString -> (ByteString, ByteString)
splitKey = BS.splitAt 32

keyLength :: Natural
keyLength = 64

cryptoError :: (MonadThrow m, Exception e)
            => (Text -> e)
            -> CryptoFailable a
            -> m a
cryptoError f = onCryptoFailure (throwM . f . Text.pack . show) pure

encodeHex :: HMAC a -> ByteString
encodeHex = convertToBase Base16
