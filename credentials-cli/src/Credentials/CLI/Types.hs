{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
import           Control.Monad.Morph             (hoist)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Credentials
import           Credentials.CLI.Types.Protocol
import           Credentials.DynamoDB
import           Credentials.S3
import qualified Data.Attoparsec.Text            as A
import           Data.ByteString                 (ByteString)
import           Data.ByteString.Builder         (Builder)
import qualified Data.ByteString.Lazy            as LBS
import           Data.Conduit
import           Data.Conduit
import           Data.Conduit.Lazy
import qualified Data.Conduit.List               as CL
import           Data.Data
import qualified Data.HashMap.Strict             as Map
import           Data.List                       (sort)
import qualified Data.Text                       as Text
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Body           (RsBody (..))
import           Network.AWS.Data.Text
import           Network.AWS.DynamoDB            (dynamoDB)
import           Network.AWS.S3                  (BucketName (..), s3)
import           Network.AWS.S3.Encryption.Body
import           Options.Applicative
import           Options.Applicative.Help.Pretty (Pretty (..), text)
import           URI.ByteString                  hiding (uriParser)

data Force
    = NoPrompt
    | Prompt

data Input
    = Value  ByteString
    | Stream !Integer (Source (ResourceT IO) ByteString)

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
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadReader Options
        )

runApp :: Env -> Options -> App a -> IO a
runApp e c = runResourceT . runAWS e . (`runReaderT` c) . unApp

-- newtype AppT a = AppT { unAppT :: (Storage m, Layer m ~ AWS) => App m a }

-- runAppT e c = runApp e c . unAppT

-- runLazy :: Source App a -> App [a]
-- runLazy = App . lazyConsume . hoist unApp

instance MonadAWS App where
    liftAWS = App . lift

-- instance Storage m => Storage (App m) where
--     type  Layer (App m) = ReaderT Options m

--     newtype Ref (App m) = Ref { ref :: Ref m }

--     type    In  (App m) = In  m
--     type    Out (App m) = Out m

--     layer = unApp

--     setup     = lift . setup           . ref
--     cleanup   = lift . cleanup         . ref
--     revisions = hoist lift . revisions . ref

--     delete     n mr = lift . delete n mr    . ref
--     insert k c n i  = lift . insert k c n i . ref
--     select   c n mr = lift . select c n mr  . ref

instance Storage App where
    type Layer App = ReaderT Options AWS

    data Ref App
        = Table  (Ref DynamoDB)
        | Bucket (Ref S3)

    type In  App = Input
    type Out App = ResumableSource (ResourceT IO) ByteString

    layer = unApp

    setup = \case
        Table  s -> embed (setup s)
        Bucket s -> embed (setup s)

    cleanup = \case
        Table  s -> embed (cleanup s)
        Bucket s -> embed (cleanup s)

    revisions = \case
        Table  s -> hoist embed (revisions s)
        Bucket s -> hoist embed (revisions s)

    delete n mr = \case
        Table  s -> embed (delete n mr s)
        Bucket s -> embed (delete n mr s)

    insert k c n i = \case
        Table  s -> embed $ do
            case i of
                Value     v -> insert k c n v s
                Stream sz v
                    | sz > 190 * 1024 -> throwM (StorageFailure "Some size error goes here.")
                    | otherwise       -> do
                        cs <- liftIO . runResourceT $ v $$ CL.consume
                        let x = LBS.toStrict (LBS.fromChunks cs)
                        insert k c n x s

        Bucket s -> embed . flip (insert k c n) s $
            case i of
                Value     v -> toBody v
                Stream sz v -> toBody (enforceChunks sz v)

    select c n mr = \case
        Table  s -> do
            (x, r) <- embed (select c n mr s)
            return (newResumableSource (CL.sourceList [x]), r)

        Bucket s -> do
            (x, r) <- embed (select c n mr s)
            return (_streamBody x, r)

embed :: (Storage m, Layer m ~ AWS) => m a -> App a
embed = App . lift . layer

type Store = Ref App

instance FromText Store where
    parser = uriParser

instance FromURI Store where
    fromURI u = Table <$> fromURI u <|> Bucket <$> fromURI u

instance ToText Store where
    toText = const "" --toText . serializeURI' . \case
        -- Table  u _ -> u
        -- Bucket u _ -> u

defaultStore = undefined

--defaultStore :: Store
-- defaultStore = Bucket u (S3Bucket "sekkinen" defaultPrefix)
--   where
--     u = URI d Nothing ("/sekkinen/credential-store") mempty Nothing
--     d = Scheme "s3"

-- defaultStore :: Store
-- defaultStore = Table u defaultTable
--   where
--     u = URI d (Just a) ("/" <> toBS defaultTable) mempty Nothing
--     d = Scheme "dynamo"
--     a = Authority Nothing (Host "localhost") (Just (Port 8000))

-- setStore :: HasEnv a => Options -> a -> a
-- setStore c = configure f
--   where
--     f = case store c of
--         Table  u _ -> g u dynamoDB
--         Bucket u _ -> g u s3

--     g u | Just h <- host u = setEndpoint (secure u) h (port u)
--         | otherwise        = id

-- instance FromText Store where
--     parser = uriParser

-- instance FromURI Store where
--     fromURI u = Table u <$> fromURI u <|> Bucket u <$> fromURI u

-- instance ToText Store where
--     toText = toText . serializeURI' . \case
--         Table  u _ -> u
--         Bucket u _ -> u

-- instance Show   Store where show   = Text.unpack . toText
-- instance Pretty Store where pretty = text . show
-- instance ToLog  Store where build  = build . toText

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
