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
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Credentials.IO
import           Credentials.Types
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
import           Network.Credentials                  as Cred
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
        say $ "LIST " <> build s <> " in " <> build r <> ":"

        xs <- list s

        forM_ xs $ \(n, v :| vs) -> do
            say $ "  "   <> build n <> ":"
            say $ "    " <> build v <> " [latest] "
            mapM_ (say . mappend "    " . build) vs
        say "Done."

    Put s k c n x -> do
        say $ "PUT " <> build n <> " to " <> build s <> "."
        v <- Cred.put k c n x s
        say $ "Wrote version " <> build v <> " of " <> build n <> "."

    Get s c n v l -> do
        say $ "GET " <> build n <> " secret."
        x <- Cred.get c n v s
        case l of
            Append -> liftIO $ BS.putStrLn (toBS x)
            Ignore -> liftIO $ BS.putStr   (toBS x)

data Mode
    = Setup   !Store
    | Cleanup !Store !Force
    | List    !Store !Format
    | Put     !Store !KeyId !Context !Name !Value
    | Get     !Store        !Context !Name !(Maybe Version) !Line
    | Del     !Store !Name  !Version !Force
    | DelAll  !Store !Name  !Force
      deriving (Show)

options :: ParserInfo (LogLevel, (Region, Mode))
options = info (helper <*> ((,) <$> level <*> sub)) desc
  where
    desc = fullDesc
        <> progDesc "Administration CLI for credential and secret storage."

    sub = subparser $ mconcat
        [ mode "setup"
            (Setup <$> store)
            "Setup the credential store."

        , mode "cleanup"
            (Cleanup <$> store <*> force)
            "Remove the credential store."

        , mode "list"
            (List <$> store <*> format)
            "List credential names and the respective versions\
            \ in the specified store."

        , mode "get"
            (Get <$> store <*> context <*> name <*> optional version <*> line)
            "Fetch and decrypt a specific version of the credential from the store. \
            \Defaults to latest version."

        , mode "put"
            (Put <$> store <*> key <*> context <*> name <*> secret)
            "Write and encrypt a new version of the credential to the store."

        , mode "delete"
            (Del <$> store <*> name <*> version <*> force)
            "Remove a specific version of the credential from the store."

        , mode "delete-all"
            (DelAll <$> store <*> name <*> force)
            "Remove all versions of the credential from the store."
        ]

mode :: String -> Parser a -> String -> Mod CommandFields (Region, a)
mode m p h = command m $ info ((,) <$> region <*> p) (progDesc h)

region :: Parser Region
region = option text
     ( long "region"
    <> metavar "REGION"
    <> help "The AWS Region in which to operate."
    <> complete
     )

level :: Parser LogLevel
level = option (eitherReader r)
     ( long "level"
    <> metavar "LEVEL"
    <> help "Log message level to emit. One of debug|info|error. [default: info]"
    <> value Info
    <> completeWith ["info", "error", "debug", "trace"]
     )
  where
    r "debug" = Right Debug
    r "trace" = Right Trace
    r "info"  = Right Info
    r "error" = Right Error
    r e       = Left $ "Unrecognised log level: " ++ e

key :: Parser KeyId
key = option text
    ( long "key"
   <> metavar "STRING"
   <> help
       ("The KMS master key id to use. \
       \[default: " ++ show defaultKeyId ++ "]")
   <> value defaultKeyId
    )

context :: Parser Context
context = pure (Context mempty)

name :: Parser Name
name = option text
     ( long "name"
    <> metavar "STRING"
    <> help "The unique name of the credential."
     )

secret :: Parser Value
secret = option text
     ( long "secret"
    <> metavar "STRING"
    <> help "The raw unencrypted value of the credential."
     )

version :: Parser Version
version = option text
     ( long "version"
    <> metavar "NUMBER"
    <> help "A specific credential version."
    <> completeWith ["1", "2"]
     )

format :: Parser Format
format = option text
     ( long "format"
    <> metavar "FORMAT"
    <> help "Output format. One of json|yaml|csv|shell. [default: shell]"
    <> value Shell
    <> complete
     )

line :: Parser Line
line = flag Append Ignore
     ( short 'n'
    <> long "no-line"
    <> help "Don't append a newline to the output. [default: off]"
     )

force :: Parser Force
force = flag Prompt NoPrompt
     ( short 'f'
    <> long "force"
    <> help "Always overwrite or remove, without an interactive prompt."
     )

store :: Parser Store
store = bucket <|> table <|> pure (Tbl defaultTable)
  where
    bucket = fmap Bkt $ Bucket
        <$> option text
             ( long "bucket"
            <> metavar "STRING"
            <> help "S3 bucket name to use for credential storage."
             )

        <*> (optional $ option text
             ( long "prefix"
            <> metavar "STRING"
            <> help "S3 object prefix to namespace credential storage."
             ))

    table = Tbl
        <$> option text
             ( long "table"
            <> metavar "STRING"
            <> help
                ("DynamoDB table name to use for credential storage. \
                \[default: " ++ show defaultTable ++ "]")
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
    str = Text.unpack . toText
