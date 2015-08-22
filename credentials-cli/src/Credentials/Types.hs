{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Credentials.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.Types where

import           Control.Exception.Lens
import           Control.Lens                         (view, ( # ), (&), (.~),
                                                       (<&>))
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.ByteString                      (ByteString)
import           Data.ByteString.Builder              (Builder)
import qualified Data.ByteString.Builder              as Build
import qualified Data.ByteString.Char8                as BS8
import           Data.Char
import           Data.Conduit                         (($$))
import qualified Data.Conduit.List                    as CL
import           Data.Data
import           Data.HashMap.Strict                  (HashMap)
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.List.NonEmpty                   as NE
import           Data.Maybe
import qualified Data.Text                            as Text
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.S3                       (BucketName,
                                                       ObjectVersionId)
import           Network.Credentials                  as Cred
import           Numeric.Natural
import           Options.Applicative
import           Options.Applicative.Builder.Internal (HasCompleter)
import           System.Exit
import           System.IO

data Line
    = Append
    | Ignore
      deriving (Show)

data Force
    = NoPrompt
    | Prompt
      deriving (Show)

data Format
    = JSON
    | YAML
    | CSV
    | Shell
      deriving (Data, Show)

instance ToText Format where
    toText = \case
        JSON  -> "json"
        YAML  -> "yaml"
        CSV   -> "csv"
        Shell -> "shell"

instance FromText Format where
    parser = matchCI "json"  JSON
         <|> matchCI "yaml"  YAML
         <|> matchCI "csv"   CSV
         <|> matchCI "shell" Shell

newtype App a = App { runApp :: AWS a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadAWS App where
    liftAWS = App

instance Storage App where
    type Layer App = AWS
    data Ref   App
        = Tbl (Ref Dynamo)
        | Bkt Bucket

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

instance ToLog Store where
    build = \case
        Tbl t            -> "dynamo://" <> build t
        Bkt (Bucket b p) -> "s3://"     <> build b <> "/" <> maybe mempty build p

instance Show Store where
    show = BS8.unpack . toBS . build
