{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |
-- Module      : Credentials.DynamoDB.Item
-- Copyright   : (c) 2013-2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- This module contains the schema that is used by "Credentials.DynamoDB" to
-- serialise encryption parameters to DynamoDB items.
module Credentials.DynamoDB.Item where

import Control.Lens        (set, view, (&), (.~))
import Control.Monad       ((>=>))
import Control.Monad.Catch (MonadThrow (..))

import Credentials.Types

import Crypto.Hash     (SHA256, digestFromByteString)
import Crypto.MAC.HMAC (HMAC (..))

import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import Data.ByteString         (ByteString)
import Data.HashMap.Strict     (HashMap)
import Data.Monoid             ((<>))
import Data.Text               (Text)

import Network.AWS.Data
import Network.AWS.DynamoDB

import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text

padding :: Text
padding = Text.replicate 19 "0"

-- | The DynamoDB field used for optimistic locking.
--
-- Serialisation of 'Version' handles left-padding to support
-- consistent lexicographic ordering when used as a range in DynamoDB.
newtype Version = Version Integer
    deriving (Eq, Ord, Num, FromText, ToText)

equals :: Item a => a -> HashMap Text Condition
equals = Map.map (\x -> condition EQ' & cAttributeValueList .~ [x]) . toItem

nameField, revisionField, versionField, wrappedKeyField,
 ciphertextField, digestField :: Text
nameField       = "name"
revisionField   = "revision"
versionField    = "version"
wrappedKeyField = "key"
ciphertextField = "contents"
digestField     = "hmac"

class Item a where
    -- | Encode an item as a set of attributes including their schema.
    toItem :: a -> HashMap Text AttributeValue

    -- | Decode an item from a set of attributes.
    parseItem :: HashMap Text AttributeValue -> Either CredentialError a

-- | Decode an item by throwing a 'CredentialError' exception when an
-- error is encountered.
fromItem :: (MonadThrow m, Item a) => HashMap Text AttributeValue -> m a
fromItem = either throwM pure . parseItem

instance (Item a, Item b) => Item (a, b) where
    toItem (x, y) = toItem x <> toItem y
    parseItem   m = (,) <$> parseItem m <*> parseItem m

instance Item Name where
    toItem    = Map.singleton nameField . toAttr
    parseItem = parse nameField

instance Item Revision where
    toItem    = Map.singleton revisionField . toAttr
    parseItem = parse revisionField

instance Item Version where
    toItem    = Map.singleton versionField . toAttr
    parseItem = parse versionField

instance Item Encrypted where
    toItem Encrypted{..} =
        Map.fromList
            [ (wrappedKeyField, toAttr wrappedKey)
            , (ciphertextField, toAttr ciphertext)
            , (digestField,     toAttr digest)
            ]

    parseItem m =
        Encrypted
            <$> parse wrappedKeyField m
            <*> parse ciphertextField m
            <*> parse digestField m

parse :: Attribute a
      => Text
      -> HashMap Text AttributeValue
      -> Either CredentialError a
parse k m =
    case Map.lookup k m of
        Nothing -> Left $ FieldMissing k (Map.keys m)
        Just v  ->
           case parseAttr v of
               Nothing -> Left $ FieldInvalid k (show v)
               Just x  -> Right x

class Attribute a where
    -- | Encode an attribute value.
    toAttr :: a -> AttributeValue

    -- | Decode an attribute value.
    parseAttr :: AttributeValue -> Maybe a

instance Attribute Text where
    toAttr  t = set avS (Just t) attributeValue
    parseAttr = view avS

instance Attribute ByteString where
    toAttr bs = set avB (Just bs) attributeValue
    parseAttr = view avB

instance Attribute Name where
    toAttr    = toAttr . toText
    parseAttr = fmap Name . parseAttr

instance Attribute Revision where
    toAttr    = toAttr . toBS
    parseAttr = fmap Revision . parseAttr

instance Attribute Integer where
    toAttr    = toAttr . toText
    parseAttr = parseAttr >=> either (const Nothing) Just . fromText

instance Attribute Version where
    toAttr (Version n) =
        let x = toText n
            y = Text.drop (Text.length x) padding <> x
         in toAttr y
    parseAttr = fmap Version . parseAttr

instance Attribute (HMAC SHA256) where
    toAttr = toAttr . Text.decodeUtf8 . convertToBase Base16 . hmacGetDigest
    parseAttr v = do
        t :: Text <- parseAttr v
        case convertFromBase Base16 (Text.encodeUtf8 t) of
            Left  _  -> Nothing
            Right bs -> HMAC <$> digestFromByteString (bs :: ByteString)
