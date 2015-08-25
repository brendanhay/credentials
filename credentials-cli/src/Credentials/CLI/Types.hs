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
-- Module      : Credentials.CLI.Types
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.CLI.Types where

import           Control.Exception.Lens
import           Control.Lens                         (view, ( # ), (&), (.~),
                                                       (<&>))
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Credentials                          as Cred
import           Credentials.DynamoDB
import           Data.Aeson                           (object, (.=))
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

(%) :: ToLog a => Builder -> a -> Builder
b % x = b <> build x

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

data Pair = Pair Text Text

instance FromText Pair where
    parser = Pair <$> key <*> val
      where
        key = A.skipSpace *> A.takeWhile1 (/= '=')
        val = A.char '='  *> A.takeText

toContext :: Alternative f => f Pair -> f Context
toContext f = Context . Map.fromList . map (\(Pair k v) -> (k, v)) <$> many f

data Mode
    = Setup     !Store
    | Cleanup   !Store !Force
    | List      !Store !Format
    | Put       !Store !KeyId        !Context !Name !Input
    | Get       !Store               !Context !Name !(Maybe Version) !Format
    | Delete    !Store !Name         !Version !Force
    | DeleteAll !Store !(Maybe Name) !Natural !Force

current :: Mode -> Store
current = \case
    Setup     s         -> s
    Cleanup   s _       -> s
    List      s _       -> s
    Put       s _ _ _ _ -> s
    Get       s _ _ _ _ -> s
    Delete    s _ _ _   -> s
    DeleteAll s _ _ _   -> s

newtype App a = App { unApp :: AWS a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadAWS App where
    liftAWS = App

instance Storage App where
    type Layer App = AWS
    data Ref   App
        = Table  (Maybe Host) (Ref DynamoDB)
        | Bucket (Maybe Host) BucketName (Maybe Text)
          deriving (Show)

    layer = unApp

    setup = \case
        Table _ t -> wrap (setup t)

    cleanup = \case
        Table _ t -> wrap (cleanup t)

    list = \case
        Table _ t -> wrap (list t)

    insert n s = \case
        Table _ t -> wrap (insert n s t)

    select n v = \case
        Table _ t -> wrap (select n v t)

    delete n v = \case
        Table _ t -> wrap (delete n v t)

    deleteAll n = \case
        Table _ t -> wrap (deleteAll n t)

wrap :: (Storage m, Layer m ~ Layer App) => m a -> App a
wrap = App . layer

runApp :: Env -> App a -> IO a
runApp e = runResourceT . runAWS e . unApp

type Store = Ref App

defaultStore :: Store
defaultStore = Table (Just (Host False "localhost" 8000)) defaultTable

instance ToLog Store where
    build = build . toText

instance FromText Store where
    parser = (parser <* A.string scheme) >>= \case
        Dynamo -> Table <$> host <*> parser
        S3     -> Bucket <$> host <*> bucket <*> prefix
      where
        host   = optional (parser <* A.char '/')
        bucket = BucketName <$> A.takeWhile1 (A.notInClass ":/")
        prefix = optional (A.char '/' *> takeText)

instance ToText Store where
    toText = \case
        Table  h t   -> toText Dynamo <> scheme <> host h <> toText t
        Bucket h b p -> toText S3     <> scheme <> host h <> toText b <> prefix p
      where
        host   = maybe mempty ((<> "/") . toText)
        prefix = maybe mempty (mappend "/")

storeEndpoint :: HasEnv a => Store -> a -> a
storeEndpoint = configure . \case
    Table  h _   -> host h dynamoDB
    Bucket h _ _ -> host h s3
  where
    host (Just (Host s h p)) = setEndpoint s h p
    host _                   = id

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
            JSON -> build (encodePretty x) <> "\n"

instance ToLog [(Name, NonEmpty Version)] where
    build = foldMap name
      where
        name (toBS -> n, v :| vs) =
            "name: " % n % " -- version " % v % " [latest]\n" <> foldMap f vs
          where
            f x = pad % " -- version " % x % "\n"
            pad = build (BS8.replicate (BS8.length n + 6) ' ')

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
