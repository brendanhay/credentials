{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Network.Credentials.Secret
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.Credentials.Secret where

import           Conduit                   hiding (await)
import           Control.Exception.Lens
import           Control.Lens              hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           "cryptonite" Crypto.Cipher.AES
import           "cryptonite" Crypto.Cipher.Types
import           "cryptonite" Crypto.Error
import           "cryptonite" Crypto.Hash               hiding (Context)
import           "cryptonite" Crypto.MAC.HMAC           hiding (Context)
import           Data.ByteArray
import           Data.ByteArray.Encoding
import           Data.ByteString           (ByteString)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BS8
import           Data.HashMap.Strict       (HashMap)
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as Text
import           Data.Typeable
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.KMS           as KMS
import           Network.Credentials.Types
import           Numeric.Natural

encrypt :: (MonadThrow m, MonadAWS m)
        => KeyId
        -> Context
        -> Value
        -> m Secret
encrypt k (Context c) (Value x) = do
    -- Generate a 64 byte key.
    -- First 32 bytes for data encryption, last 32 bytes for HMAC.
    rs <- send $ generateDataKey (toText k)
        & gdkNumberOfBytes     ?~ 64
        & gdkEncryptionContext .~ c

    let (dataKey, hmacKey) = splitKey (rs ^. gdkrsPlaintext)

    --consider scrubbing dataKey, plainText?

    ctext <- either throwM pure . eitherCryptoError $
        ctrCombine
            <$> cipherInit dataKey
            <*> pure (ivAdd nullIV 128 :: IV AES128)
            <*> pure (toBS x)

    -- Compute an HMAC using the hmac key and the cipher text.
    return $! Secret
        (Key    (rs ^. gdkrsCiphertextBlob))
        (Cipher ctext)
        (Digest (hmac hmacKey ctext))

decrypt :: (MonadThrow m, MonadAWS m)
        => Context
        -> Secret
        -> m Value
decrypt (Context c) (Secret (toBS -> key) (toBS -> ctext) h) = do
    rs <- send $ KMS.decrypt key & dEncryptionContext .~ c

    -- Check the HMAC before we decrypt to verify ciphertext integrity
    -- try:
    --     kms_response = kms.decrypt(CiphertextBlob=b64decode(material['key']), EncryptionContext=context)
    -- except InvalidCiphertextException:
    --     if context is None:
    --         msg = ("Could not decrypt hmac key with KMS. The credential may "
    --                "require that an encryption context be provided to decrypt "
    --                "it.")
    --     else:
    --         msg = ("Could not decrypt hmac key with KMS. The encryption "
    --                "context provided may not match the one used when the "
    --                "credential was stored.")

    -- Decrypted plaintext data. This value may not be returned if the customer
    -- master key is not available or if you didn\'t have permission to use it.

    let (dataKey, hmacKey) = splitKey $ fromMaybe mempty (rs ^. drsPlaintext)

    unless (Digest (hmac hmacKey ctext) == h) $
        throwM (userError "IntegrityError")

    -- -- hmac = HMAC(hmac_key, msg=b64decode(material['contents']),
    -- --             digestmod=SHA256)

    -- if hmac.hexdigest() != material['hmac']:
    --     raise IntegrityError("Computed HMAC on %s does not match stored HMAC" --
    --                          % name)

    plain <- either throwM pure . eitherCryptoError $
        ctrCombine
            <$> cipherInit dataKey
            <*> pure (ivAdd nullIV 128 :: IV AES128)
            <*> pure ctext

    return $! Value plain

splitKey :: ByteString -> (ByteString, ByteString)
splitKey = BS.splitAt 32
