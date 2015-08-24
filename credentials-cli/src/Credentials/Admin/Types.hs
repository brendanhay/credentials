{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Credentials.Admin.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.Admin.Types where

import           Control.Exception.Lens
import           Control.Lens                         (view, ( # ), (&), (.~),
                                                       (<&>))
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Credentials                          as Cred
import           Data.Aeson                           (object, (.=))
import           Data.Aeson.Encode                    (encodeToByteStringBuilder)
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
import           Data.Foldable                        (foldMap)
import           Data.HashMap.Strict                  (HashMap)
import           Data.List                            (intersperse)
import           Data.List.NonEmpty                   (NonEmpty (..))
import           Data.Maybe
import qualified Data.Text                            as Text
import           GHC.Exts                             (toList)
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.S3                       (BucketName (..),
                                                       ObjectVersionId)
import           Options.Applicative
import           Options.Applicative.Builder.Internal (HasCompleter)
import           System.Exit
import           System.IO

data Force = NoPrompt | Prompt

data Format = JSON | Echo
    deriving (Data, Show)

instance ToText Format where
    toText = \case
        JSON -> "json"
        Echo -> "echo"

instance FromText Format where
    parser = matchCI "json" JSON <|> matchCI "echo" Echo

data Input
    = Raw  Value
    | Path FilePath

newtype App a = App { runApp :: AWS a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadAWS App where
    liftAWS = App

instance Storage App where
    type Layer App = AWS
    data Ref   App
        = Tbl (Ref Dynamo)
        | Bkt BucketName (Maybe Text)

    layer = runApp

    setup = \case
        Tbl t -> App (layer (setup t))

    cleanup = \case
        Tbl t -> wrap (cleanup t)

    list = \case
        Tbl t -> wrap (list t)

    insert n s = \case
        Tbl t -> wrap (insert n s t)

    select n v = \case
        Tbl t -> wrap (select n v t)

wrap :: (Storage m, Layer m ~ Layer App) => m a -> App a
wrap = App . layer

type Store = Ref App

instance Show  Store where show  = Text.unpack . toText
instance ToLog Store where build = build       . toText

instance FromText Store where
    parser = do
        p <- A.takeWhile1 (/= ':') >> A.string "//"
        case p of
            "dynamo" -> Tbl <$> parser
            "s3"     -> Bkt <$> bucket <*> prefix
            _        -> fail $ "Unrecognised protocol '" ++ Text.unpack p ++ "'"
      where
        bucket = BucketName <$> A.takeWhile1 (/= '/') <* A.char '/'
        prefix = optional takeText

instance ToText Store where
    toText = \case
        Tbl t   -> "dynamo://" <> toText t
        Bkt b p -> "s3://"     <> toText b <> maybe mempty (mappend "/") p

data Output where
    Output :: (ToLog a, ToJSON a) => Format -> a -> Output

instance ToLog Output where
    build (Output f x) =
        case f of
            Echo -> build x
            JSON -> encodeToByteStringBuilder (toJSON x)

instance ToLog [(Name, NonEmpty Version)] where
    build = mconcat . intersperse "\n" . map name
      where
        name (toBS -> n, v :| vs) =
               "name: " <> build n <> " -- version " <> build v <> " [latest]"
            <> foldMap f vs
          where
            f x = pad <> " -- version " <> build x
            pad = build $ BS8.replicate (BS8.length n + 6) ' '

instance ToJSON [(Name, NonEmpty Version)] where
    toJSON = object . map (\(n, vs) -> toText n .= map toText (toList vs))

instance ToLog (Name, (Value, Version)) where
    build (_, (Value x, _)) = build x

instance ToJSON (Name, (Value, Version)) where
    toJSON (n, (x, v)) = object
        [ "name"    .= toText n
        , "version" .= toText v
        , "secret"  .= toText (toBS x)
        ]
