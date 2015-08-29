{-# LANGUAGE FlexibleInstances          #-}
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

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Credentials
import           Credentials.CLI.Types.Protocol
import           Credentials.DynamoDB
import           Credentials.S3
import qualified Data.Attoparsec.Text            as A
import           Data.ByteString.Builder         (Builder)
import           Data.Data
import qualified Data.HashMap.Strict             as Map
import           Data.List                       (sort)
import qualified Data.Text                       as Text
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.DynamoDB            (dynamoDB)
import           Network.AWS.S3                  (BucketName (..), s3)
import           Options.Applicative
import           Options.Applicative.Help.Pretty (Pretty (..), text)
import           URI.ByteString                  hiding (uriParser)

data Force
    = NoPrompt
    | Prompt

data Input
    = Raw  Value
    | Path FilePath

data Mode
    = Setup
    | Cleanup  !Force
    | List
    | Put      !KeyId   !Context  !Name !Input
    | Get      !Context !Name     !(Maybe Revision)
    | Delete   !Name    !Revision !Force
    | Truncate !Name    !Force

data Format
    = Pretty
    | JSON
    | Echo
    | Print
      deriving (Eq, Show)

instance ToText Format where
    toText = \case
        Pretty -> "pretty"
        JSON   -> "json"
        Echo   -> "echo"
        Print  -> "print"

instance FromText Format where
    parser = takeLowerText >>= \case
        "pretty" -> pure Pretty
        "json"   -> pure JSON
        "echo"   -> pure Echo
        e        -> fromTextError $ "Failure parsing format from: " <> e

data Options = Options
    { region :: !Region
    , store  :: !Store
    , format :: !Format
    , level  :: !LogLevel
    }

newtype App a = App { unApp :: ReaderT Options AWS a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadReader Options)

runApp :: Env -> Options -> App a -> IO a
runApp e c = runResourceT . runAWS e . (`runReaderT` c) . unApp

instance MonadAWS App where
    liftAWS = App . lift

instance Storage App where
    type Layer App = ReaderT Options AWS
    data Ref   App
        = Table  URI TableName
        | Bucket URI BucketNS

    layer = unApp

    setup = \case
        Table  _ t -> runStore (setup t)
        Bucket _ b -> runStore (setup b)

    cleanup = \case
        Table  _ t -> runStore (cleanup t)
        Bucket _ b -> runStore (cleanup b)

    listAll = \case
        Table  _ t -> runStore (listAll t)
        Bucket _ b -> runStore (listAll b)

    insert n s = \case
        Table  _ t -> runStore (insert n s t)
        Bucket _ b -> runStore (insert n s b)

    select n v = \case
        Table  _ t -> runStore (select n v t)
        Bucket _ b -> runStore (select n v b)

    delete n v = \case
        Table  _ t -> runStore (delete n v t)
        Bucket _ b -> runStore (delete n v b)

runStore :: (Storage m, Layer m ~ AWS) => m a -> App a
runStore = App . lift . layer

type Store = Ref App

defaultStore :: Store
defaultStore = Bucket u (BucketNS "sekkinen" (Just "credential-store"))
  where
    u = URI d Nothing ("/sekkinen/credential-store") mempty Nothing
    d = Scheme "s3"

-- defaultStore :: Store
-- defaultStore = Table u defaultTable
--   where
--     u = URI d (Just a) ("/" <> toBS defaultTable) mempty Nothing
--     d = Scheme "dynamo"
--     a = Authority Nothing (Host "localhost") (Just (Port 8000))

setStore :: HasEnv a => Options -> a -> a
setStore c = configure f
  where
    f = case store c of
        Table  u _ -> g u dynamoDB
        Bucket u _ -> g u s3

    g u | Just h <- host u = setEndpoint (secure u) h (port u)
        | otherwise        = id

instance FromText Store where
    parser = uriParser

instance FromURI Store where
    fromURI u = Table u <$> fromURI u <|> Bucket u <$> fromURI u

instance ToText Store where
    toText = toText . serializeURI' . \case
        Table  u _ -> u
        Bucket u _ -> u

instance Show   Store where show   = Text.unpack . toText
instance Pretty Store where pretty = text . show
instance ToLog  Store where build  = build . toText

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
