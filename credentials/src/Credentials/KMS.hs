{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

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

import           Control.Exception.Lens
import           Control.Lens           hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Credentials.Types
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.MAC.HMAC        hiding (Context)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import           Data.Typeable
import           Network.AWS            hiding (await)
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.Error      (hasCode, hasStatus)
import           Network.AWS.KMS        hiding (decrypt, encrypt)
import qualified Network.AWS.KMS        as KMS
import           Numeric.Natural

encrypt :: (MonadAWS m, Typeable m)
        => KeyId
        -> Context
        -> Name
        -> ByteString
        -> m Encrypted
encrypt k c n x = do
    -- Generate a key. First half for data encryption, last for HMAC.
    let rq = generateDataKey (toText k)
           & gdkNumberOfBytes     ?~ bytes
           & gdkEncryptionContext .~ fromContext c

    rs <- catches (send rq)
        [ handler (_ServiceError . hasStatus 400 . hasCode "NotFound") $
            throwM . MasterKeyMissing k . fmap toText . _serviceMessage
        , handler _NotFoundException $
            throwM . MasterKeyMissing k . fmap toText . _serviceMessage
        ]

    let (dataKey, hmacKey) = splitKey (rs ^. gdkrsPlaintext)

    -- TODO: consider scrubbing dataKey, plainText?
    ctext <- combine x <$> cipher (EncryptFailure c n) dataKey

    -- Compute an HMAC using the hmac key and the cipher text.
    return $! Encrypted
        (Key    (rs ^. gdkrsCiphertextBlob))
        (Cipher ctext)
        (Digest (hmac hmacKey ctext))

decrypt :: MonadAWS m
        => Context
        -> Name
        -> Encrypted
        -> m ByteString
decrypt c n (Encrypted (toBS -> key) (toBS -> ctext) actual) = do
    let rq = KMS.decrypt key & dEncryptionContext .~ fromContext c

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

    unless (expect == actual) $
        throwM (IntegrityFailure n expect actual)

    combine ctext
        <$> cipher (DecryptFailure c n) dataKey

cipher :: (MonadThrow m, Exception e) => (Text -> e) -> ByteString -> m AES128
cipher e = onCryptoFailure (throwM . e . Text.pack . show) pure . cipherInit

-- FIXME: revisit IV use/initialisation.
combine :: ByteString -> AES128 -> ByteString
combine x aes = ctrCombine aes nullIV x

splitKey :: ByteString -> (ByteString, ByteString)
splitKey = BS.splitAt 32

bytes :: Natural
bytes = 64

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
