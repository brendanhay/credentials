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
import           Credentials.DynamoDB
import qualified Data.Attoparsec.Text    as A
import           Data.ByteString.Builder (Builder)
import           Data.Data
import qualified Data.HashMap.Strict     as Map
import           Data.List               (sort)
import qualified Data.Text               as Text
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.DynamoDB    (dynamoDB)
import           Network.AWS.S3          (BucketName (..), s3)
import           Options.Applicative
import           URI.ByteString

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

instance MonadAWS App where
    liftAWS = App . lift

type Store = Ref App

instance Storage App where
    type Layer App = ReaderT Options AWS
    data Ref   App
        = Table  URI (Ref DynamoDB)
        | Bucket URI BucketName (Maybe Text)

    layer = unApp

    setup = \case
        Table _ t -> run (setup t)

    cleanup = \case
        Table _ t -> run (cleanup t)

    listAll = \case
        Table _ t -> run (listAll t)

    insert n s = \case
        Table _ t -> run (insert n s t)

    select n v = \case
        Table _ t -> run (select n v t)

    delete n v = \case
        Table _ t -> run (delete n v t)

run :: DynamoDB a -> App a
run = App . lift . layer

runApp :: Env -> Options -> App a -> IO a
runApp e c = runResourceT . runAWS e . (`runReaderT` c) . unApp

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
