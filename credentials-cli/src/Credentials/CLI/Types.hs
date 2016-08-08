{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Credentials.CLI.Types
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.CLI.Types where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Morph          (hoist)
import Control.Monad.Reader
import Control.Monad.Trans.Resource

import Credentials
import Credentials.CLI.Types.Protocol

import Crypto.Random (MonadRandom (..))

import Data.ByteString         (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Conduit
import Data.Conduit.Lazy
import Data.Data
import Data.List               (sort)
import Data.Text               (Text)

import Network.AWS
import Network.AWS.Data
import Network.AWS.DynamoDB (dynamoDB)
import Network.AWS.Endpoint

import Options.Applicative
import Options.Applicative.Help.Pretty (Pretty (..), text)

import URI.ByteString (Authority (..), Host (..), Port (..), Scheme (..), URI,
                       URIRef (..))

import qualified Data.Attoparsec.Text as A
import qualified Data.HashMap.Strict  as Map
import qualified Data.Text            as Text
import qualified URI.ByteString       as URI

data Force
    = NoPrompt
    | Prompt

data Input
    = Value !ByteString
    | Path  !FilePath

data Mode
    = Setup
    | Teardown  !Force
    | List
    | Insert   !KeyId   !Context  !Name !Input
    | Select   !Context !Name     !(Maybe Revision)
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
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadReader Options
        , MonadBase IO
        )

--     select c n mr (Table _ s) = do
--         (x, r) <- embed (select c n mr s)
--         return (newResumableSource (CL.sourceList [x]), r)

instance MonadAWS App where
    liftAWS = App . lift

instance MonadResource App where
    liftResourceT = App . liftResourceT

instance MonadRandom App where
    getRandomBytes = liftIO . getRandomBytes

runApp :: Env -> Options -> App a -> IO a
runApp e c = runResourceT . runAWS e . (`runReaderT` c) . unApp

runLazy :: Source App a -> App [a]
runLazy = App . lazyConsume . hoist unApp

data Store = Table URI DynamoTable

instance FromText Store where
    parser = uriParser

instance FromURI Store where
    fromURI u = Table u <$> fromURI u

instance ToText Store where
    toText = toText . URI.serializeURI' . \case
        Table u _ -> u

instance Show   Store where show   = Text.unpack . toText
instance Pretty Store where pretty = text . show
instance ToLog  Store where build  = build . toText

defaultRegion :: Region
defaultRegion = Frankfurt

defaultStore :: Region -> Store
defaultStore r = Table u defaultTable
  where
    u = URI s (Just a) ("/" <> toBS defaultTable) mempty Nothing
    s = Scheme "dynamo"
    a = Authority Nothing h (Just p)
    h = Host (_endpointHost e)
    p = Port (_endpointPort e)
    e = defaultEndpoint dynamoDB r

setStore :: HasEnv a => Options -> a -> a
setStore c = configure f
  where
    f = case store c of
        Table u _ -> g u dynamoDB

    g u | Just h <- host u = setEndpoint (secure u) h (port u)
        | otherwise        = id

(%) :: ToLog a => Builder -> a -> Builder
b % x = b <> build x

unsafeEnum :: forall a. (Ord a, Data a, ToText a) => [a]
unsafeEnum = sort . map fromConstr . dataTypeConstrs $ dataTypeOf val
  where
    val :: a
    val = undefined

string :: ToText a => a -> String
string = Text.unpack . toText

data Pair = Pair Text Text

instance FromText Pair where
    parser = Pair <$> key <*> val
      where
        key = A.skipSpace *> A.takeWhile1 (/= '=')
        val = A.char '='  *> A.takeText

ctx :: Alternative f => f Pair -> f Context
ctx f = Context . Map.fromList . map (\(Pair k v) -> (k, v)) <$> many f
