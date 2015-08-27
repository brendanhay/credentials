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
-- Module      : Credentials.CLI.Types
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.CLI.Types where

import qualified Blaze.ByteString.Builder             as BB
import           Control.Exception.Lens
import           Control.Lens                         (view, ( # ), (&), (.~),
                                                       (<&>))
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Credentials                          as Cred
import           Credentials.CLI.Protocol
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
import           Data.List                            (foldl', sort)
import           Data.List                            (intersperse)
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

data Fact
    = Required
    | Optional
    | Default

data Force
    = NoPrompt
    | Prompt

data Agree
    = Yes
    | No
    | What String

data Input
    = Raw  Value
    | Path FilePath

data Mode
    = Setup
    | Cleanup   !Force
    | List
    | Put       !KeyId        !Context  !Name             !Input
    | Get       !Context      !Name     !(Maybe Revision)
    | Delete    !Name         !Revision !Force
    | DeleteAll !(Maybe Name) !Natural  !Force

data Format
    = Pretty
    | JSON
    | Echo
    | Print
      deriving (Eq, Show)

instance ToText Format where
    toText = \case
        Pretty -> "json-pretty"
        JSON   -> "json"
        Echo   -> "echo"
        Print  -> "print"

instance FromText Format where
    parser = takeLowerText >>= \case
        "json-pretty" -> pure Pretty
        "json"        -> pure JSON
        "echo"        -> pure Echo
        e             -> fromTextError $ "Failure parsing format from: " <> e

data Common = Common
    { region :: !Region
    , store  :: !Store
    , format :: !Format
    , level  :: !LogLevel
    }

newtype App a = App { unApp :: ReaderT Common AWS a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadReader Common)

instance MonadAWS App where
    liftAWS = App . lift

instance Storage App where
    type Layer App = ReaderT Common AWS
    data Ref   App
        = Table  URI (Ref DynamoDB)
        | Bucket URI BucketName (Maybe Text)

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

--wrap :: (Storage m, Layer m ~ Layer App) => m a -> App a
wrap = App . lift . layer

runApp :: Env -> Common -> App a -> IO a
runApp e c = runResourceT . runAWS e . (`runReaderT` c) . unApp

type Store = Ref App

defaultStore :: Store
defaultStore = Table uri defaultTable
  where
    uri    = URI scheme (Just auth) ("/" <> toBS defaultTable) mempty Nothing
    scheme = Scheme "dynamo"
    auth   = Authority Nothing (Host "localhost") (Just (Port 8000))

setStore :: HasEnv a => Common -> a -> a
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

instance Show Store where
    show = Text.unpack . toText

instance ToLog Store where
    build = build . BB.toLazyByteString . serializeURI . \case
        Table  u _   -> u
        Bucket u _ _ -> u

instance ToText Store where
    toText = toText . BB.toByteString . serializeURI . \case
        Table  u _   -> u
        Bucket u _ _ -> u

data Pair = Pair Text Text

instance FromText Pair where
    parser = Pair <$> key <*> val
      where
        key = A.skipSpace *> A.takeWhile1 (/= '=')
        val = A.char '='  *> A.takeText

ctx :: Alternative f => f Pair -> f Context
ctx f = Context . Map.fromList . map (\(Pair k v) -> (k, v)) <$> many f

(%) :: ToLog a => Builder -> a -> Builder
b % x = b <> build x

unsafeEnum :: forall a. (Ord a, Data a, ToText a) => [a]
unsafeEnum = sort . map fromConstr . dataTypeConstrs $ dataTypeOf val
  where
    val :: a
    val = undefined

string :: ToText a => a -> String
string = Text.unpack . toText
