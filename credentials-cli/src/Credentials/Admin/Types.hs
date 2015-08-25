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
    parser = takeLowerText >>= \case
        "json" -> pure JSON
        "echo" -> pure Echo
        e      -> fromTextError $ "Failure parsing format from: " <> e

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
        = Tbl (Maybe Host) (Ref Dynamo)
        | Bkt (Maybe Host) BucketName (Maybe Text)
          deriving (Show)

    layer = runApp

    setup = \case
        Tbl _ t -> App (layer (setup t))

    cleanup = \case
        Tbl _ t -> wrap (cleanup t)

    list = \case
        Tbl _ t -> wrap (list t)

    insert n s = \case
        Tbl _ t -> wrap (insert n s t)

    select n v = \case
        Tbl _ t -> wrap (select n v t)

    delete n v = \case
        Tbl _ t -> wrap (delete n v t)

    deleteAll n = \case
        Tbl _ t -> wrap (deleteAll n t)

wrap :: (Storage m, Layer m ~ Layer App) => m a -> App a
wrap = App . layer

type Store = Ref App

storageHost :: Store -> Maybe Host
storageHost = \case
    Tbl h _   -> h
    Bkt h _ _ -> h

defaultStore :: Store
defaultStore = Tbl Nothing defaultTable

instance ToLog Store where
    build = build . toText

instance FromText Store where
    parser = (parser <* A.string scheme) >>= \case
        Dynamo -> Tbl <$> host <*> parser
        S3     -> Bkt <$> host <*> bucket <*> prefix
      where
        host   = optional (parser <* A.char '/')
        bucket = BucketName <$> A.takeWhile1 (A.notInClass ":/")
        prefix = optional (A.char '/' *> takeText)

instance ToText Store where
    toText = \case
        Tbl h t   -> toText Dynamo <> scheme <> host h <> toText t
        Bkt h b p -> toText S3     <> scheme <> host h <> toText b <> prefix p
      where
        host   = maybe mempty ((<> "/") . toText)
        prefix = maybe mempty (mappend "/")

scheme :: Text
scheme = "://"

data Protocol
    = Dynamo
    | S3
      deriving (Eq, Show)

instance FromText Protocol where
    parser = (A.asciiCI "dynamo" >> pure Dynamo)
         <|> (A.asciiCI "s3"     >> pure S3)

instance ToText Protocol where
    toText = \case
        Dynamo -> "dynamo"
        S3     -> "s3"

data Host = Host !Bool !ByteString !Int
    deriving (Eq, Show)

instance FromText Host where
    parser = do
        h <- A.takeWhile1 (/= ':') <* A.char ':'
        p <- A.decimal
        return $! Host (p == 443) (toBS h) p

instance ToText Host where
    toText (Host _ h p) = toText h <> ":" <> toText p

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
