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

encrypt :: (MonadRandom m, MonadAWS m, Typeable m)
        => KeyId
        -> Context
        -> Name
        -> ByteString
        -> m Encrypted
encrypt k c n x = do
    -- Generate a key. First half for data encryption, last for HMAC.
    let rq = generateDataKey (toText k)
           & gdkNumberOfBytes     ?~ keyLength
           & gdkEncryptionContext .~ fromContext c

    rs    <- catches (send rq)
        [ handler (_ServiceError . hasStatus 400 . hasCode "NotFound") $
            throwM . MasterKeyMissing k . fmap toText . _serviceMessage
        , handler _NotFoundException $
            throwM . MasterKeyMissing k . fmap toText . _serviceMessage
        ]

    let (dataKey, hmacKey) = splitKey (rs ^. gdkrsPlaintext)
        failure            = EncryptFailure c n

    nonce <- getRandomBytes nonceLength
    iv    <- parseIV failure nonce
    aes   <- cryptoError failure (Cipher.cipherInit dataKey)

    let !ctext = Cipher.ctrCombine aes iv x

    -- Compute an HMAC using the hmac key and the cipher text.
    pure $! Encrypted
        (Nonce  nonce)
        (Key    (rs ^. gdkrsCiphertextBlob))
        (Cipher ctext)
        (Digest (hmac hmacKey ctext))

decrypt :: MonadAWS m
        => Context
        -> Name
        -> Encrypted
        -> m ByteString
decrypt c n (Encrypted (Nonce nonce) (toBS -> key) (toBS -> ctext) actual) = do
    let rq = KMS.decrypt key & decEncryptionContext .~ fromContext c

    rs <- catching_ _InvalidCiphertextException (send rq) $
        throwM . DecryptFailure c n $
            if blankContext c
                then "Could not decrypt stored key using KMS. \
                     \The credential may require an ecryption context."
                else  "Could not decrypt stored key using KMS. \
                     \The provided encryption context may not match the one \
                     \used when the credential was stored."

    p  <- plaintext (DecryptFailure c n) rs

    let (dataKey, hmacKey) = splitKey p
        expect             = Digest (hmac hmacKey ctext)
        failure            = DecryptFailure c n

    unless (expect == actual) $
        throwM (IntegrityFailure n expect actual)

    iv  <- parseIV failure nonce
    aes <- cryptoError failure (Cipher.cipherInit dataKey)

    pure $! Cipher.ctrCombine aes iv ctext

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
plaintext :: (MonadThrow m, Exception e)
          => (Text -> e)
          -> DecryptResponse
          -> m ByteString
plaintext e rs =
    maybe (throwM (e "Decrypted plaintext data not available from KMS."))
          pure
          (rs ^. drsPlaintext)
