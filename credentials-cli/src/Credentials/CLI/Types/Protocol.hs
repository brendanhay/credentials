{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Credentials.CLI.Types.Protocol
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.CLI.Types.Protocol where

import           Control.Lens          (preview, _Just)

import           Credentials           (DynamoDB, Ref)
import           Credentials.DynamoDB  ()

import           Data.Attoparsec.Text  (Parser)
import qualified Data.Attoparsec.Text  as A
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe
import           Data.Text             (Text)
import qualified Data.Text             as Text

import           Network.AWS.Data      (FromText, fromText, toBS, toText)

import           URI.ByteString

uriParser :: FromURI a => Parser a
uriParser = uri >>= either fail pure . fromURI
  where
    uri = A.takeText >>= either (fail . show) pure . f . toBS
    f   = parseURI strictURIParserOptions

class FromURI a where
    fromURI :: URI -> Either String a

-- dynamo:/table-name
instance FromURI (Ref DynamoDB) where
    fromURI u = do
        scheme "dynamo" u
        ensure "Table name cannot be empty." (path u)

ensure :: FromText a => String -> Text -> Either String a
ensure m x
    | Text.null x = Left m
    | otherwise   = fromText x

scheme :: ByteString -> URI -> Either String ()
scheme e u
    | a == e    = Right ()
    | otherwise = Left $ "Protocol '" ++ BS8.unpack a ++ "' unrecognized."
  where
    a = schemeBS (uriScheme u)

path :: URI -> Text
path = toText . BS8.dropWhile (== '/') . uriPath

host :: URI -> Maybe ByteString
host = preview (uriAuthorityL . _Just . authorityHostL . hostBSL)

secure :: URI -> Bool
secure = (== 443) . port

port :: URI -> Int
port = fromMaybe 443 . preview
    (uriAuthorityL . _Just . authorityPortL . _Just . portNumberL)
