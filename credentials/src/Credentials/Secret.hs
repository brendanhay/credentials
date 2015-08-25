{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Credentials.Secret
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.Secret
    ( Credentials.Secret.encrypt
    , Credentials.Secret.decrypt
    ) where

import           Control.Exception.Lens
import           Control.Lens            hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Credentials.Types
import           "cryptonite" Crypto.Cipher.AES
import           "cryptonite" Crypto.Cipher.Types
import           "cryptonite" Crypto.Error
import           "cryptonite" Crypto.Hash             hiding (Context)
import           "cryptonite" Crypto.MAC.HMAC         hiding (Context)
import           Data.Bifunctor
import           Data.ByteArray
import           Data.ByteArray.Encoding
import           Data.ByteString         (ByteString)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BS8
import           Data.HashMap.Strict     (HashMap)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text               as Text
import           Data.Typeable
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.KMS         as KMS
import           Numeric.Natural

encrypt :: (MonadThrow m, MonadAWS m)
        => KeyId
        -> Context
        -> Name
        -> Value
        -> m Secret
encrypt k c n (Value x) = do
    -- Generate a 64 byte key.
    -- First 32 bytes for data encryption, last 32 bytes for HMAC.
    rs <- send $ generateDataKey (toText k)
        & gdkNumberOfBytes     ?~ bytes
        & gdkEncryptionContext .~ context c

    let (dataKey, hmacKey) = splitKey (rs ^. gdkrsPlaintext)

    -- TODO: consider scrubbing dataKey, plainText?
    ctext <- counter x <$> cipher (EncryptFailure c n) dataKey

    -- Compute an HMAC using the hmac key and the cipher text.
    return $! Secret
        (Key    (rs ^. gdkrsCiphertextBlob))
        (Cipher ctext)
        (Digest (hmac hmacKey ctext))

decrypt :: (MonadThrow m, MonadAWS m)
        => Context
        -> Name
        -> Secret
        -> m Value
decrypt c n (Secret (toBS -> key) (toBS -> ctext) actual) = do
    e  <- trying _InvalidCiphertextException .
        send $ KMS.decrypt key & dEncryptionContext .~ context c

    rs <- case e of
        Left _ | blank c ->
            throwM $ DecryptFailure c n
                "Could not decrypt stored key using KMS. \
                \The credential may require an ecryption context."
        Left _           ->
            throwM $ DecryptFailure c n
                "Could not decrypt stored key using KMS. \
                \The provided encryption context may not match the one \
                \used when the credential was stored."
        Right x          ->
            pure x

    p  <- plaintext (DecryptFailure c n) rs

    let (dataKey, hmacKey) = splitKey p
        expect             = Digest (hmac hmacKey ctext)

    unless (expect == actual) $
        throwM (IntegrityFailure n expect actual)

    Value . counter ctext
        <$> cipher (DecryptFailure c n) dataKey

-- Decrypted plaintext data. This value may not be returned if the customer
-- master key is not available or if you didn\'t have permission to use it.
plaintext :: (MonadThrow m, Exception e)
          => (Text -> e)
          -> DecryptResponse
          -> m ByteString
plaintext e = maybe (throwM (e msg)) pure . (^. drsPlaintext)
  where
    msg = "Decrypted plaintext data not available from KMS."

cipher :: (MonadThrow m, Exception e) => (Text -> e) -> ByteString -> m AES128
cipher e = onCryptoFailure (throwM . e . Text.pack . show) pure . cipherInit

counter :: ToByteString a => a -> AES128 -> ByteString
counter x aes = ctrCombine aes (ivAdd nullIV 128) (toBS x)

splitKey :: ByteString -> (ByteString, ByteString)
splitKey = BS.splitAt 32

bytes :: Natural
bytes = 64
