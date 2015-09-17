{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Credentials.DynamoDB.Item
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.DynamoDB.Item where

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

-- | The DynamoDB field used for optimistic locking.
newtype Version = Version Integer
    deriving (Eq, Ord, Num, FromText, ToText, ToByteString)

equals :: Item a => a -> HashMap Text Condition
equals = Map.map (\x -> condition EQ' & cAttributeValueList .~ [x]) . encode

revisionIndex :: Text
revisionIndex = revisionField

nameField, revisionField, versionField :: Text
nameField     = name (Proxy :: Proxy Name)
revisionField = name (Proxy :: Proxy Revision)
versionField  = name (Proxy :: Proxy Version)

class Item a where
    encode :: a -> HashMap Text AttributeValue
    decode :: MonadThrow m => HashMap Text AttributeValue -> m a

    default encode :: Attr a b => a -> HashMap Text AttributeValue
    encode = toAttr

    default decode :: (MonadThrow m, Monoid b, ToText b, Attr a b)
                   => HashMap Text AttributeValue
                   -> m a
    decode = fromAttr

instance (Item a, Item b) => Item (a, b) where
    encode (x, y) = encode x <> encode y
    decode m      = (,) <$> decode m <*> decode m

instance Item Secret where
    encode (Secret k h c) = encode k <> encode h <> encode c
    decode m              = Secret <$> decode m <*> decode m <*> decode m

instance Item Name
instance Item Version
instance Item Revision
instance Item Key
instance Item Cipher
instance Item HMAC256

data Meta a b = Meta Text (Traversal' AttributeValue b) (Prism' b a)

class Attr a b | a -> b where
    meta :: Meta a b

instance Attr Name Text where
    meta = Meta "name" (avS . traverse) text

instance Attr Version Text where
    meta = Meta "version" (avN . traverse) text

instance Attr Revision ByteString where
    meta = Meta "revision" (avB . traverse) (iso Revision toBS)

instance Attr Key ByteString where
    meta = Meta "key" (avB . traverse) (iso Key toBS)

instance Attr Cipher ByteString where
    meta = Meta "contents" (avB . traverse) (iso Cipher toBS)

instance Attr HMAC256 ByteString where
    meta = Meta "hmac" (avB . traverse) (iso Hex toBS)

instance Attr Context (HashMap Text Text) where
     meta = Meta "matdesc" (lens f g) (iso Context fromContext)
       where
         f = Map.fromList . mapMaybe q . Map.toList . view avM
           where
             q (k, v) = (k,) <$> v ^. avS

         g s x = s & avM .~ Map.map q x
           where
             q a = attributeValue & avS ?~ a

toAttr :: Attr a b => a -> HashMap Text AttributeValue
toAttr x = [(k, attributeValue & l .~ review p x)]
  where
    Meta k l p = meta

fromAttr :: (MonadThrow m, Monoid b, ToText b, Attr a b)
          => HashMap Text AttributeValue
          -> m a
fromAttr m = require >>= parse
  where
    Meta k l p = meta

    require = maybe missing pure (Map.lookup k m)

    parse (view l -> x) = maybe (invalid x) pure (preview p x)

    missing = throwM (FieldMissing k (Map.keys m))
    invalid = throwM . FieldInvalid k . toText

name :: forall a b. Attr a b => Proxy a -> Text
name _ = let (Meta k _ _) = meta :: Meta a b in k

text :: (FromText a, ToText a) => Prism' Text a
text = prism' toText go
  where
    go x | Text.null x = Nothing
         | otherwise   = either (const Nothing) Just (fromText x)
