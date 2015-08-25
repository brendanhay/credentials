{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLists        #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE ViewPatterns           #-}

-- |
-- Module      : Credentials.DynamoDB.Val
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.DynamoDB.Val where

import           Control.Lens         hiding (Context)
import           Control.Monad.Catch
import           Credentials
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Network.AWS.Data
import           Network.AWS.DynamoDB

equals :: Val a => a -> HashMap Text Condition
equals = Map.map (\x -> condition EQ' & cAttributeValueList .~ [x]) . toVal

nameField, versionField :: Text
nameField    = name (Proxy :: Proxy Name)
versionField = name (Proxy :: Proxy Version)

class Val a where
    toVal   :: a -> HashMap Text AttributeValue
    fromVal :: MonadThrow m => HashMap Text AttributeValue -> m a

    default toVal :: IsField a b => a -> HashMap Text AttributeValue
    toVal = toField

    default fromVal :: (MonadThrow m, Monoid b, ToText b, IsField a b)
                    => HashMap Text AttributeValue
                    -> m a
    fromVal = fromField

instance (Val a, Val b) => Val (a, b) where
    toVal (x, y) = toVal x <> toVal y
    fromVal m    = (,) <$> fromVal m <*> fromVal m

instance Val Secret where
    toVal (Secret k h c) = toVal k <> toVal h <> toVal c
    fromVal m = Secret <$> fromVal m <*> fromVal m <*> fromVal m

instance Val Name
instance Val Version
instance Val Key
instance Val Cipher
instance Val HMAC256

class IsField a b | a -> b where
    field :: Field a b

instance IsField Name    Text       where field = Field "name"     avS text
instance IsField Version Text       where field = Field "version"  avN text
instance IsField Key     ByteString where field = Field "key"      avB (bytes Key)
instance IsField Cipher  ByteString where field = Field "contents" avB (bytes Cipher)
instance IsField HMAC256 ByteString where field = Field "hmac"     avB (bytes Hex)

data Field a b = Field Text (Lens' AttributeValue (Maybe b)) (Prism' b a)

toField :: IsField a b => a -> HashMap Text AttributeValue
toField x = let Field k l p = field in [(k, attributeValue & l ?~ review p x)]

fromField :: (MonadThrow m, Monoid b, ToText b, IsField a b)
          => HashMap Text AttributeValue
          -> m a
fromField m = require >>= parse
  where
    Field k l p = field

    require = maybe missing pure (Map.lookup k m)

    parse (attr -> x) = maybe (invalid x) pure (preview p x)

    attr = fromMaybe mempty . view l

    missing = throwM (FieldMissing k (Map.keys m))
    invalid = throwM . FieldInvalid k . toText

name :: forall a b. IsField a b => Proxy a -> Text
name _ = let (Field k _ _) = field :: Field a b in k

text :: (FromText a, ToText a) => Prism' Text a
text = prism' toText go
  where
    go x | Text.null x = Nothing
         | otherwise   = either (const Nothing) Just (fromText x)

bytes :: ToByteString a => (ByteString -> a) -> Prism' ByteString a
bytes w = prism' toBS (Just . w)
