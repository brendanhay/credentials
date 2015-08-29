{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE ViewPatterns      #-}

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
import           Control.Lens           hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Credentials.Types
import           "cryptonite" Crypto.Cipher.AES
import           "cryptonite" Crypto.Cipher.Types
import           "cryptonite" Crypto.Error
import           "cryptonite" Crypto.MAC.HMAC        hiding (Context)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import           Data.Typeable
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.Error      (hasCode, hasStatus)
import           Network.AWS.KMS        as KMS
import           Numeric.Natural

encrypt :: (MonadThrow m, MonadAWS m, Typeable m)
        => KeyId
        -> Context
        -> Name
        -> Value
        -> m Secret
encrypt k c n (Value x) = do
    -- Generate a key. First half for data encryption, last for HMAC.
    let rq = generateDataKey (toText k)
           & gdkNumberOfBytes     ?~ bytes
           & gdkEncryptionContext .~ context c

    rs <- catches (send rq)
        [ handler (_ServiceError . hasStatus 400 . hasCode "NotFound") $
            throwM . MasterKeyMissing k . fmap toText . _serviceMessage
        , handler _NotFoundException $
            throwM . MasterKeyMissing k . fmap toText . _serviceMessage
        ]

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
    let rq = KMS.decrypt key & dEncryptionContext .~ context c

    rs <- catching_ _InvalidCiphertextException (send rq) $
        throwM . DecryptFailure c n $
            if blank c
                then "Could not decrypt stored key using KMS. \
                     \The credential may require an ecryption context."
                else  "Could not decrypt stored key using KMS. \
                     \The provided encryption context may not match the one \
                     \used when the credential was stored."

    p  <- plaintext (DecryptFailure c n) rs

    let (dataKey, hmacKey) = splitKey p
        expect             = Digest (hmac hmacKey ctext)

    unless (expect == actual) $
        throwM (IntegrityFailure n expect actual)

    Value . counter ctext
        <$> cipher (DecryptFailure c n) dataKey

-- Decrypted plaintext data. This value may not be returned if the customer
-- master key is not available or if you didn't have permission to use it.
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
splitKey = BS.splitAt (2 * bytes)

bytes :: Natural
bytes = 64
