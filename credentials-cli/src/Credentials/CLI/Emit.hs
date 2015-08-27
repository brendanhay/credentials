{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Credentials.CLI.Emit
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.CLI.Emit where

import qualified Blaze.ByteString.Builder             as BB
import           Control.Exception.Lens
import           Control.Lens                         (view, ( # ), (&), (.~),
                                                       (<&>))
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Credentials                          as Cred
import           Credentials.CLI.Protocol
import           Credentials.CLI.Types
import           Credentials.DynamoDB
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
import           Data.Char
import           Data.Conduit                         (($$))
import qualified Data.Conduit.List                    as CL
import           Data.Data
import           Data.Data
import           Data.Foldable                        (foldMap)
import           Data.HashMap.Strict                  (HashMap)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as Map
import           Data.List                            (foldl', intersperse,
                                                       sort)
import           Data.List.NonEmpty                   (NonEmpty (..))
import           Data.List.NonEmpty                   (NonEmpty (..))
import           Data.Maybe
import           Data.String
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
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
import           Text.PrettyPrint.ANSI.Leijen         (Doc, Pretty)
import           URI.ByteString

-- FIXME: Give types to the output rather the tuple-itis.

type Revisions = [(Name, NonEmpty Revision)]

data Emit a = Emit { multiline :: !Bool, expose :: a }

instance ToText a => ToText (Emit a) where
    toText = toText . expose

instance ToLog (Emit a) => ToLog (Emit (Store, a)) where
    build (Emit p (s, x)) = title % build (Emit p x)
      where
        title | p         = build s % ":\n"
              | otherwise = mempty

instance ToLog (Emit Text)     where build = build . toText
instance ToLog (Emit Setup)    where build = build . toText
instance ToLog (Emit Revision) where build = build . toText

instance ToLog (Emit Revisions) where
    build (Emit p rs) = go rs
      where
        go []     = mempty
        go [x]    = line x
        go (x:xs) = line x <> "\n" <> go xs

        line (n, v :| vs) =
            x % n % ":" % y % v % " # latest" % foldMap (y %) vs

        (x, y) | p         = ("  ", "\n    ")
               | otherwise = ("",   "\n  ")

instance ToLog (Emit (Name, (Value, Revision))) where
    build = mappend "  " . build . toBS . fst . snd . expose

instance ToLog (Emit (Name, Revision, Text)) where
    build (Emit _ (_, r, _)) = "  " <> build r

instance ToLog (Emit (Name, Text)) where
    build = mappend "  " . build . snd . expose

instance ToJSON (Emit a) => ToJSON (Emit (Store, a)) where
   toJSON (Emit p (s, x)) = object [toText s .= Emit p x]

instance ToJSON (Emit Text)     where toJSON = toJSON . toText
instance ToJSON (Emit Setup)    where toJSON = toJSON . toText
instance ToJSON (Emit Revision) where toJSON = toJSON . toText

instance ToJSON (Emit Revisions) where
    toJSON = object . map f . expose
      where
        f (n, vs) = toText n .= map toText (toList vs)

instance ToJSON (Emit (Name, (Value, Revision))) where
    toJSON (Emit _ (n, (v, r))) = object
        [ "name"     .= toText n
        , "revision" .= toText r
        , "secret"   .= Text.decodeUtf8 (toBS v)
        ]

instance ToJSON (Emit (Name, Revision, Text)) where
    toJSON (Emit _ (n, r, s)) = object
        [ "name"     .= toText n
        , "revision" .= toText r
        , "status"   .= s
        ]

instance ToJSON (Emit (Name, Text)) where
    toJSON (Emit _ (n, s)) = object
        [ "name"   .= toText n
        , "status" .= s
        ]
