{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- |
-- Module      : Credentials.KMS
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
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
import Crypto.Cipher.Types (IV)
import Crypto.Error
import Crypto.MAC.HMAC     (hmac)
import Crypto.Random       (MonadRandom (..))

import Data.ByteString (ByteString)
import Data.Text       (Text)
import Data.Typeable   (Typeable)

import Network.AWS
import Network.AWS.Data
import Network.AWS.Error (hasCode, hasStatus)
import Network.AWS.KMS   hiding (decrypt, encrypt)

import Numeric.Natural (Natural)

import qualified Crypto.Cipher.Types as Cipher
import qualified Data.ByteString     as BS
import qualified Data.Text           as Text
import qualified Network.AWS.KMS     as KMS

-- |
encrypt :: (MonadRandom m, MonadAWS m, Typeable m)
        => KeyId
        -> Context
        -> Name
        -> ByteString
        -> m Encrypted
encrypt key ctx name plaintext = do
    -- Generate a key. First half for data encryption, last for HMAC.
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

    nonce <- getRandomBytes nonceLength
    iv    <- parseIV failure nonce
    aes   <- cryptoError failure (Cipher.cipherInit dataKey)

    let !ciphertext = Cipher.ctrCombine aes iv plaintext

    -- Compute an HMAC using the hmac key and the cipher text.
    pure $! Encrypted
        (Nonce  nonce)
        (Key    (rs ^. gdkrsCiphertextBlob))
        (Cipher ciphertext)
        (Digest (hmac hmacKey ciphertext))

-- |
decrypt :: MonadAWS m
        => Context
        -> Name
        -> Encrypted
        -> m ByteString
decrypt ctx name encrypted = do
    let Encrypted (Nonce nonce) (toBS -> key) (toBS -> ciphertext) actual = encrypted
        rq = KMS.decrypt key & decEncryptionContext .~ fromContext ctx

    rs <- catching_ _InvalidCiphertextException (send rq) $
        throwM . DecryptFailure ctx name $
            if blankContext ctx
                then "Could not decrypt stored key using KMS. \
                     \The credential may require an ecryption context."
                else  "Could not decrypt stored key using KMS. \
                     \The provided encryption context may not match the one \
                     \used when the credential was stored."

    plaintext <- getPlaintext (DecryptFailure ctx name) rs

    let (dataKey, hmacKey) = splitKey plaintext
        expect             = Digest (hmac hmacKey ciphertext)
        failure            = DecryptFailure ctx name

    unless (expect == actual) $
        throwM (IntegrityFailure name expect actual)

    iv  <- parseIV failure nonce
    aes <- cryptoError failure (Cipher.cipherInit dataKey)

    pure $! Cipher.ctrCombine aes iv ciphertext

splitKey :: ByteString -> (ByteString, ByteString)
splitKey = BS.splitAt 32

nonceLength :: Int
nonceLength = 16

keyLength :: Natural
keyLength = 64

parseIV :: (MonadThrow m, Exception e)
       => (Text -> e)
       -> ByteString
       -> m (IV AES256)
parseIV f bs =
    cryptoError f $
        maybe (CryptoFailed CryptoError_IvSizeInvalid) CryptoPassed
              (Cipher.makeIV bs)

cryptoError :: (MonadThrow m, Exception e)
            => (Text -> e)
            -> CryptoFailable a
            -> m a
cryptoError f = onCryptoFailure (throwM . f . Text.pack . show) pure

-- Decrypted plaintext data. This value may not be returned if the customer
-- master key is not available or if you didn't have permission to use it.
getPlaintext :: (MonadThrow m, Exception e)
             => (Text -> e)
             -> DecryptResponse
             -> m ByteString
getPlaintext e rs =
    maybe (throwM (e "Decrypted plaintext data not available from KMS."))
          pure
          (rs ^. drsPlaintext)
