{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Credentials.CLI.Protocol
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.CLI.Protocol where

import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Credentials                          as Cred
import           Credentials.DynamoDB
import           Credentials.DynamoDB                 (TableName)
import           Data.Aeson                           (object, (.=))
import           Data.Aeson.Encode                    (encodeToByteStringBuilder)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import           Data.Aeson.Types                     (ToJSON (..))
import qualified Data.Attoparsec.Text                 as A
import           Data.Bifunctor
import           Data.ByteString                      (ByteString)
import           Data.ByteString.Builder              (Builder)
import qualified Data.ByteString.Builder              as Build
import qualified Data.ByteString.Char8                as BS8
import qualified Data.ByteString.Char8                as BS8
import           Data.Char
import           Data.Conduit                         (($$))
import qualified Data.Conduit.List                    as CL
import           Data.Data
import           Data.Foldable                        (foldMap)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as Map
import           Data.List                            (intersperse)
import           Data.List.NonEmpty                   (NonEmpty (..))
import           Data.Maybe
import qualified Data.Text                            as Text
import           GHC.Exts                             (toList)
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.DynamoDB                 (dynamoDB)
import           Network.AWS.S3                       (BucketName (..),
                                                       ObjectVersionId, s3)
import           Numeric.Natural
import           Options.Applicative
import           Options.Applicative.Builder.Internal (HasCompleter)
import           System.Exit
import           System.IO
import           URI.ByteString

endpoint :: URI -> Service -> Service
endpoint u
    | Just h <- host u = setEndpoint (secure u) h (port u)
    | otherwise        = id

class FromURI a where
    fromURI :: URI -> Either String a

-- dynamo://aws/table-name

instance FromURI TableName where
    fromURI u = do
        scheme "dynamo" u
        nonEmpty "Table name cannot be empty." (path u)

-- s3://aws/bucket[/prefix]

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
