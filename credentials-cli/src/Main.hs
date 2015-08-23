{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Main (main) where

import           Control.Exception.Lens
import           Control.Lens                         (view, ( # ), (&), (.~),
                                                       (<&>))
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Credentials                          as Cred hiding (context)
import           Credentials.Admin.IO
import           Credentials.Admin.Types
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as BS
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
import qualified Data.Text.IO                         as Text
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.S3                       (BucketName,
                                                       ObjectVersionId)
import           Numeric.Natural
import           Options.Applicative
import           Options.Applicative.Builder.Internal (HasCompleter)
import           System.Exit
import           System.IO

default (Builder)

main :: IO ()
main = do
    (v, (r, m)) <-
        customExecParser (prefs (showHelpOnError <> columns 100)) options

    l <- newLogger v stdout
    e <- newEnv r Discover <&> envLogger .~ l

    runResourceT . runAWS e . runApp $ program r m
--    catches (runResourceT . runAWS e . runApp $ program r m)
        -- [ handler _NotSetup $ \s ->
        --     quit 2 ("Credential store " <> build s <> " doesn't exist. Please run setup.")

        -- -- , hd 3 _Invalid
        -- -- , hd 4 _Missing
        -- ]

program :: Region -> Mode -> App ()
program r = \case
    Setup s -> do
        say $ "Setting up " <> build s <> " in " <> build r <> "."
        x <- Cred.setup s
        say $ build s <> " " <> build x <> "."

    Cleanup s f -> do
        say $ "This will delete " <> build s <> " from " <> build r <> "!"
        prompt f $ do
            Cred.cleanup s
            say $ build s <> " deleted."

    List s f -> do
        xs <- list s
        liftIO . BS.putStr $ emit xs f

    Put s k c n i -> do
        say $ "PUT " <> build n <> " to " <> build s <> "."

        x <- case i of
            Raw  v -> pure v
            Path p -> do
                say $ "Reading " <> build p <> "..."
                Value <$> liftIO (BS.readFile p)

        v <- Cred.put k c n x s
        say $ "Wrote version " <> build v <> " of " <> build n <> "."

    Get s c n v f -> do
        x <- Cred.get c n v s
        liftIO . BS.putStr $ emit (n, x) f

data Mode
    = Setup     !Store
    | Cleanup   !Store !Force
    | List      !Store !Format
    | Put       !Store !KeyId        !Context !Name !Input
    | Get       !Store               !Context !Name !(Maybe Version) !Format
    | Delete    !Store !Name         !Version !Force
    | DeleteAll !Store !(Maybe Name) !Natural !Force

options :: ParserInfo (LogLevel, (Region, Mode))
options = info (helper <*> ((,) <$> level <*> sub)) desc
  where
    desc = fullDesc
        <> progDesc "Administration CLI for credential and secret storage."

    sub = subparser $ mconcat
        [ mode "setup"
            (Setup <$> store)
            "Setup a new credential store."

        , mode "cleanup"
            (Cleanup <$> store <*> force)
            "Remove the credential store entirely."

        , mode "list"
            (List <$> store <*> format)
            "List credential names and the respective versions\
            \ in the specified store."

        , mode "get"
            (Get <$> store <*> context <*> name <*> optional version <*> format)
            "Fetch and decrypt a specific version of a credential from the store. \
            \[default: latest]"

        , mode "put"
            (Put <$> store <*> key <*> context <*> name <*> input)
            "Write and encrypt a new version of a credential to the store."

        , mode "delete"
            (Delete <$> store <*> name <*> version <*> force)
            "Remove a specific version of a credential from the store."

        , mode "truncate"
            (DeleteAll <$> store <*> optional name <*> retain <*> force)
            "Remove multiple versions of credentials from the store. \
            \If no credential name is specified, it will operate on all \
            \credentials."
        ]

mode :: String -> Parser a -> String -> Mod CommandFields (Region, a)
mode m p h = command m $ info ((,) <$> region <*> p) (progDesc h)

region :: Parser Region
region = option text
     ( short 'r'
    <> long "region"
    <> metavar "REGION"
    <> help "The AWS Region in which to operate."
    <> complete
     )

level :: Parser LogLevel
level = option (eitherReader r)
     ( short 'l'
    <> long "level"
    <> metavar "LEVEL"
    <> help "Log message level to emit. (trace|debug|info|error) [default: info]"
    <> value Info
    <> completeWith ["info", "error", "debug", "trace"]
     )
  where
    r "debug" = Right Debug
    r "trace" = Right Trace
    r "info"  = Right Info
    r "error" = Right Error
    r e       = Left $ "Unrecognised log level: " ++ e

store :: Parser Store
store = option text
     ( short 's'
    <> long "store"
    <> metavar "ADDRESS"
    <> help
        ("Protocol address for the storage system. \
         \(s3://<bucket>[/<prefix>] | dynamo://<table>) \
         \[default: " ++ string (Tbl defaultTable) ++ "].")
    <> value (Tbl defaultTable)
     )

key :: Parser KeyId
key = option text
    ( short 'k'
   <> long "key"
   <> metavar "STRING"
   <> help
       ("The KMS master key id to use. \
       \[default: " ++ string defaultKeyId ++ "]")
   <> value defaultKeyId
    )

context :: Parser Context
context = pure (Context mempty)

name :: Parser Name
name = option text
     ( short 'n'
    <> long "name"
    <> metavar "STRING"
    <> help "The unique name of the credential."
     )

version :: Parser Version
version = option text
     ( short 'v'
    <> long "version"
    <> metavar "NUMBER"
    <> help "A specific credential version."
    <> completeWith ["1", "2"]
     )

force :: Parser Force
force = flag Prompt NoPrompt
     ( short 'f'
    <> long "force"
    <> help "Always overwrite or remove, without an interactive prompt."
     )

format :: Parser Format
format = option text
     ( long "format"
    <> metavar "FORMAT"
    <> help "Output format. (json|echo) [default: echo]"
    <> value Echo
    <> complete
     )

retain :: Parser Natural
retain = option text
     ( short 'k'
    <> long "keep"
    <> metavar "NUMBER"
    <> help "Number of versions to retain. [default: latest]"
    <> value 1
     )

input :: Parser Input
input = textual <|> filepath
  where
    textual = Raw
        <$> option text
             ( long "secret"
            <> metavar "STRING"
            <> help "The raw unencrypted value of the credential."
             )

    filepath = Path
        <$> option str
             ( long "path"
            <> metavar "PATH"
            <> help "A path to read as the raw unencrypted value of the credential."
            <> action "file"
             )

text :: FromText a => ReadM a
text = eitherReader (fromText . Text.pack)

complete :: forall a f. (Data a, ToText a, HasCompleter f) => Mod f a
complete = completeWith $
    map (str . fromConstr) . dataTypeConstrs $ dataTypeOf val
  where
    val :: a
    val = undefined

    str :: a -> String
    str = string

string :: ToText a => a -> String
string = Text.unpack . toText
