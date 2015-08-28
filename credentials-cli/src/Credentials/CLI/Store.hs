{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Credentials.CLI.Store
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.CLI.Store where

import           Control.Applicative
import           Control.Lens
import           Credentials
import           Credentials.CLI.Types
import           Credentials.DynamoDB
import qualified Data.Attoparsec.Text            as A
import           Data.Bifunctor
import           Data.ByteString                 (ByteString)
import           Data.ByteString.Builder         (Builder)
import qualified Data.ByteString.Builder         as Build
import qualified Data.ByteString.Char8           as BS8
import qualified Data.ByteString.Char8           as BS8
import           Data.Char
import           Data.Data
import           Data.Foldable                   (foldMap)
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.DynamoDB            (dynamoDB)
import           Network.AWS.S3                  (BucketName (..), s3)
import           Options.Applicative.Help.Pretty (Pretty (..), text)
import           URI.ByteString

defaultStore :: Store
defaultStore = Table uri defaultTable
  where
    uri  = URI dyn (Just auth) ("/" <> toBS defaultTable) mempty Nothing
    dyn  = Scheme "dynamo"
    auth = Authority Nothing (Host "localhost") (Just (Port 8000))

setStore :: HasEnv a => Options -> a -> a
setStore c = configure f
  where
    f = case store c of
        Table  u _   -> endpoint u dynamoDB
        Bucket u _ _ -> endpoint u s3

instance FromText Store where
    parser = uri >>= either fail pure . fromURI
      where
        uri = A.takeText >>= either (fail . show) pure . f . toBS
        f   = parseURI strictURIParserOptions

instance FromURI Store where
    fromURI u = Table u <$> fromURI u <|> uncurry (Bucket u) <$> fromURI u

instance ToText Store where
    toText = toText . serializeURI' . \case
        Table  u _   -> u
        Bucket u _ _ -> u

instance Show   Store where show   = Text.unpack . toText
instance Pretty Store where pretty = text . show
instance ToLog  Store where build  = build . toText

endpoint :: URI -> Service -> Service
endpoint u
    | Just h <- host u = setEndpoint (secure u) h (port u)
    | otherwise        = id

class FromURI a where
    fromURI :: URI -> Either String a

-- dynamo:/table-name
instance FromURI TableName where
    fromURI u = do
        scheme "dynamo" u
        nonEmpty "Table name cannot be empty." (path u)

-- s3:/bucket[/prefix]
instance FromURI (BucketName, Maybe Text) where
    fromURI u = do
        scheme "s3" u
        (,) <$> nonEmpty "Bucket name cannot be empty." b
            <*> pure (prefix p)
      where
        (b, p) = Text.break (== '/') (path u)

        prefix x = listToMaybe [y | not (Text.null y)]
          where
            y = Text.dropWhile (== '/') x

nonEmpty :: FromText a => String -> Text -> Either String a
nonEmpty m x
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
