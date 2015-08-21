{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE TemplateHaskell            #-}

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
import           Network.AWS.KMS
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

    let (dataKey, hmacKey) = BS.splitAt 32 (rs ^. gdkrsPlaintext)

    ctext <- either throwM pure . eitherCryptoError $
        ctrCombine
            <$> cipherInit dataKey
            <*> pure (ivAdd nullIV 128 :: IV AES128)
            <*> pure (toBS x)

    -- Compute an HMAC using the hmac key and the cipher text.
    return $! Secret
        (Key (rs ^. gdkrsCiphertextBlob))
        (Ciphertext ctext)
        (HMAC' (hmac hmacKey ctext))
