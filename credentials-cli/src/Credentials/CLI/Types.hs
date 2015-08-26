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

import qualified Blaze.ByteString.Builder             as BB
import           Control.Exception.Lens
import           Control.Lens                         (view, ( # ), (&), (.~),
                                                       (<&>))
import           Control.Monad.Catch
import           Control.Monad.IO.Class
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

data Format
    = Pretty
    | JSON
    | Shell
      deriving (Data, Show)

instance ToText Format where
    toText = \case
        Pretty -> "json-pretty"
        JSON   -> "json"
        Shell  -> "echo"

instance FromText Format where
    parser = takeLowerText >>= \case
        "json-pretty" -> pure Pretty
        "json"        -> pure JSON
        "shell"       -> pure Shell
        e             -> fromTextError $ "Failure parsing format from: " <> e

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

wrap :: (Storage m, Layer m ~ Layer App) => m a -> App a
wrap = App . layer

runApp :: Env -> App a -> IO a
runApp e = runResourceT . runAWS e . unApp

type Store = Ref App

defaultStore :: Store
defaultStore = Table uri defaultTable
  where
    uri    = URI scheme (Just auth) ("/" <> toBS defaultTable) mempty Nothing
    scheme = Scheme "dynamo"
    auth   = Authority Nothing (Host "localhost") (Just (Port 8000))

setStore :: HasEnv a => Store -> a -> a
setStore = configure . \case
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
    toText = toText .BB.toByteString . serializeURI . \case
        Table  u _   -> u
        Bucket u _ _ -> u

data Input
    = Raw  Value
    | Path FilePath

data Output where
    Output :: (ToLog a, ToJSON a) => Format -> a -> Output

instance ToLog Output where
    build (Output f x) =
        case f of
            Pretty -> build (encodePretty x) <> "\n"
            JSON   -> encodeToByteStringBuilder (toJSON x)
            Shell  -> build x

instance ToLog (Store, [(Name, NonEmpty Version)]) where
    build (s, vs) = build s % ":\n" % build vs

instance ToLog [(Name, NonEmpty Version)] where
    build = foldMap name
      where
        name (toBS -> n, v :| vs) =
            "  " % n % ":\n" % f v % " [latest]\n" % foldMap g vs
          where
            g x = f x % "\n"
            f x = "    version: " % x

            pad = build (BS8.replicate (BS8.length n + 6) ' ')

instance ToJSON (Store, [(Name, NonEmpty Version)]) where
    toJSON (s, vs) = object [toText s .= vs]

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

data Pair = Pair Text Text

instance FromText Pair where
    parser = Pair <$> key <*> val
      where
        key = A.skipSpace *> A.takeWhile1 (/= '=')
        val = A.char '='  *> A.takeText

toContext :: Alternative f => f Pair -> f Context
toContext f = Context . Map.fromList . map (\(Pair k v) -> (k, v)) <$> many f

(%) :: ToLog a => Builder -> a -> Builder
b % x = b <> build x

-- Orphan.
instance FromText LogLevel where
    parser = takeLowerText >>= \case
        "error" -> pure Error
        "debug" -> pure Debug
        "trace" -> pure Trace
        e       -> fromTextError $ "Failure parsing log level from: " <> e

instance ToText LogLevel where
    toText = Text.toLower . Text.pack . show

-- class Help a where
--     initial  :: a
--     document :: Proxy a -> [(Text, Maybe Doc)]

-- instance Help Region where
--     initial  = Frankfurt
--     document = map ((,Nothing) . toText) . unsafeEnum

-- instance Help Format where
--     initial  = Shell
--     document = values
--         [ (Pretty, Just "foo")
--         , (JSON,   Just "bar")
--         , (Shell,  Just "quz")
--         ]

-- instance Help LogLevel where
--     initial  = Info
--     document = values
--         [ (Error, Nothing)
--         , (Debug, Nothing)
--         , (Trace, Nothing)
--         ]

-- instance Help Store where
--     initial  = defaultStore
--     document = const
--         [ ("dynamo://[host[:port]]/table-name",       Nothing)
--         , ("s3://[host[:port]]/bucket-name[/prefix]", Nothing)
--         ]

-- instance Help KeyId where
--     intiial  = defaultKeyId
--     document = const []

-- annotated :: forall a. Help a => Proxy a -> [(Doc, Maybe Doc)]
-- annotated = map (first (fromString . string)) . document

-- values :: ToText a => [(a, b)] -> c -> [(Text, b)]
-- values = const . map (first toText)
          --

unsafeEnum :: forall a. (Ord a, Data a, ToText a) => [a]
unsafeEnum = sort . map fromConstr . dataTypeConstrs $ dataTypeOf val
  where
    val :: a
    val = undefined

string :: ToText a => a -> String
string = Text.unpack . toText
