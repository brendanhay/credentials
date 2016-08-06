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

-- |
-- Module      : Credentials.DynamoDB.Item
-- Copyright   : (c) 2013-2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.DynamoDB.Item where

import Control.Lens        (Lens', Prism')
import Control.Lens        (iso, lens, preview, prism', review, view)
import Control.Lens        ((&), (.~), (?~), (^.))
import Control.Monad.Catch (MonadThrow (..))

import Credentials.Types

import Data.ByteString     (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Maybe          (fromMaybe, mapMaybe)
import Data.Monoid         ((<>))
import Data.Proxy          (Proxy (..))
import Data.Text           (Text)

import Network.AWS.Data
import Network.AWS.DynamoDB

import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as Text

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

instance Item Encrypted where
    encode (Encrypted n k h c) =
            encode n
         <> encode k
         <> encode h
         <> encode c
    decode m = Encrypted
        <$> decode m
        <*> decode m
        <*> decode m
        <*> decode m

instance Item Name
instance Item Version
instance Item Revision
instance Item Nonce
instance Item Key
instance Item Cipher
instance Item HMAC256

data Meta a b = Meta Text (Lens' AttributeValue (Maybe b)) (Prism' b a)

class Attr a b | a -> b where
    meta :: Meta a b

instance Attr Name Text where
    meta = Meta "name" avS text

instance Attr Version Text where
    meta = Meta "version" avN text

instance Attr Revision ByteString where
    meta = Meta "revision" avB (iso Revision toBS)

instance Attr Key ByteString where
    meta = Meta "key" avB (iso Key toBS)

instance Attr Cipher ByteString where
    meta = Meta "contents" avB (iso Cipher toBS)

instance Attr Nonce ByteString where
    meta = Meta "iv" avB (iso Nonce toBS)

instance Attr HMAC256 ByteString where
    meta = Meta "hmac" avB (iso Hex toBS)

instance Attr Context (HashMap Text Text) where
     meta = Meta "matdesc" (lens f g) (iso Context fromContext)
       where
         f = Just . Map.fromList . mapMaybe q . Map.toList . view avM
           where
             q (k, v) = (k,) <$> v ^. avS

         g s x = s & avM .~ Map.map q (fromMaybe mempty x)
           where
             q a = attributeValue & avS ?~ a

toAttr :: Attr a b => a -> HashMap Text AttributeValue
toAttr x = [(k, attributeValue & l ?~ review p x)]
  where
    Meta k l p = meta

fromAttr :: (MonadThrow m, Monoid b, ToText b, Attr a b)
          => HashMap Text AttributeValue
          -> m a
fromAttr m = require >>= parse
  where
    Meta k l p = meta

    require = maybe missing pure (Map.lookup k m)
    parse x =
        case view l x of
            Nothing -> missing
            Just y  -> maybe (invalid y) pure (preview p y)

    missing = throwM (FieldMissing k (Map.keys m))
    invalid = throwM . FieldInvalid k . toText

name :: forall a b. Attr a b => Proxy a -> Text
name _ = let (Meta k _ _) = meta :: Meta a b in k

text :: (FromText a, ToText a) => Prism' Text a
text = prism' toText go
  where
    go x | Text.null x = Nothing
         | otherwise   = either (const Nothing) Just (fromText x)
