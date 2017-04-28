{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Main (main) where

import Prelude hiding (truncate)

import Control.Exception.Lens
import Control.Lens                 ((.~), (<&>))
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Resource

import Credentials
import Credentials.CLI.Format
import Credentials.CLI.IO
import Credentials.CLI.Options
import Credentials.CLI.Types

import Data.ByteString.Builder (Builder)
import Data.Conduit
import Data.Functor.Identity   (Identity (..))
import Data.Text               (Text)

import Network.AWS

import Options.Applicative             hiding (optional)
import Options.Applicative.Help.Pretty

import System.IO

import qualified Control.Applicative as App
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit.Binary  as CB
import qualified Data.Conduit.List    as CL

default (Builder, Text)

main :: IO ()
main = do
    (defaultOptions -> opt, m) <-
        customExecParser (prefs (showHelpOnError <> columns 90)) options

    lgr <- newLogger (level opt) stderr
    env <- newEnv (runIdentity (region opt)) Discover
        <&> (envLogger .~ lgr) . setStore opt

    catches (runApp env opt (program opt m))
        [ handler _CredentialError (quit 1 . show)
        ]

program :: Options Identity -> Mode -> App ()
program Options{ store  = Identity store@(Table _ table)
               , region = Identity region
               , ..}    = \case
    List -> do
        says ("Listing contents of " % store % " in " % region % "...")
        runLazy (revisions table) >>=
            emit . ListR

    Insert key ctx name input -> do
        says ("Writing new revision of " %
              name % " to " % store % " in " % region % "...")
        rev <- case input of
            Value val  -> insert key ctx name val table
            Path  path -> do
                sz <- getFileSize path
                if sz > 190 * 1024
                    then throwM $ StorageFailure
                        "Secret file is larger than allowable storage size."
                    else do
                        cs <- liftIO . runResourceT $
                            CB.sourceFile path $$ CL.consume
                        let val = LBS.toStrict (LBS.fromChunks cs)
                        insert key ctx name val table
        emit (InsertR name rev)

    Select ctx name rev -> do
        say "Retrieving"
        case rev of
            Nothing -> pure ()
            Just r  -> say (" revision " % r % " of")
        says (" " % name % " from " % store % " in " % region % "...")
        (val, rev') <- select ctx name rev table
        emit (SelectR name rev' val)

    Delete name rev force -> do
        says ("This will delete revision " %
              rev % " of " % name % " from " % store % " in " % region % "!")
        prompt force $ do
            delete name rev table
            emit (DeleteR name rev)

    Truncate name force -> do
        says ("This will delete all but the latest revision of " %
              name % " from " % store % " in " % region % "!")
        prompt force $ do
            truncate name table
            emit (TruncateR name)

    Setup -> do
        says ("Setting up " % store % " in " % region % ".")
        says "Running ..."
        setup table >>= emit . SetupR

    Teardown force -> do
        says ("This will delete " % store % " from " % region % "!")
        prompt force $ do
            teardown table
            emit TeardownR

options :: ParserInfo (Options Maybe, Mode)
options = info (helper <*> modes) (fullDesc <> headerDoc (Just desc))
  where
    desc = bold "credentials"
        <+> "- Administration tool for managing secure, shared credentials."

    modes = hsubparser $ mconcat
        [ mode "list"
            (pure List)
            "List credential names and their respective revisions."
            "This does not perform decryption of any credentials, and can be used \
            \to obtain an overview of the credential names and revisions that \
            \are stored."

        , mode "select"
            (Select <$> contextOption <*> nameOption <*> optional revisionOption)
            "Fetch and decrypt a specific credential revision."
            "Defaults to the latest available revision, if --revision is not specified."

        , mode "insert"
            (Insert <$> keyOption <*> contextOption <*> nameOption <*> inputOption)
            "Write and encrypt a new credential revision."
            "You can supply the secret value as a string with --secret, or as \
            \a file path which contents' will be read by using --path."

        , mode "delete"
            (Delete <$> nameOption <*> require revisionOption <*> forceFlag)
            "Remove a specific credential revision."
            "Please note that if an application is pinned to the revision specified \
            \by --revision, it will no longer be able to decrypt the credential."

        , mode "truncate"
            (Truncate <$> nameOption <*> forceFlag)
            "Truncate a specific credential's revisions."
            "This will remove all but the most recent credential revision. \
            \That is, after running this command you will have exactly _one_ \
            \revision for the given credential name."

        , mode "setup"
            (pure Setup)
            "Setup a new credential store."
            "This will run the necessary actions to create a new credential store. \
            \This action is idempotent and if the store already exists, \
            \the operation will succeed with exit status 0."

        , mode "teardown"
            (Teardown <$> forceFlag)
            "Remove an entire credential store."
            "Warning: This will completely remove the credential store. For some \
            \storage engines this action is irrevocable unless you specifically \
            \perform backups for your data."
        ]

mode :: String
     -> Parser a
     -> Text
     -> Text
     -> Mod CommandFields (Options Maybe, a)
mode name p desc foot =
    command name $ info ((,) <$> commonOptions <*> p)
        ( fullDesc <> progDescDoc (Just $ wrap desc)
                   <> footerDoc   (Just $ indent 2 (wrap foot) <> line)
        )

commonOptions :: Parser (Options Maybe)
commonOptions = Options
    <$> App.optional (textOption
         ( short 'r'
        <> long "region"
        <> metavar "REGION"
        <> completes "The AWS region in which to operate."
             "The following regions are supported:"
                 (map (,mempty) unsafeEnum)
             Nothing
             (Just "Note: this corresponds to both the KMS key region and the \
                   \DynamoDB table region.")
         ))

    <*> App.optional (textOption
         ( short 'u'
        <> long "uri"
        <> metavar "URI"
        <> defaults "URI specifying the storage system to use."
             "The URI format must follow the following protocol:"
                 [ ("dynamo:/[/host[:port]]/table-name", "")
                 , (show (defaultStore defaultRegion),   "")

                 ]
             Nothing
             (Just "If no host is specified (ie. dynamo:/table-name), \
                   \the default AWS endpoint for the given --region will be used. \
                   \If an AWS endpoint is specified the endpoint region MUST \
                   \correspond with --region, or a signature error will occur.")
         ))

    <*> textOption
         ( short 'o'
        <> long "output"
        <> metavar "FORMAT"
        <> completes "Output format for displaying retrieved credentials."
             "The following formats are supported:"
                 [ (Pretty, "Pretty printed JSON.")
                 , (JSON,   "Single-line JSON output.")
                 , (Echo,   "Untitled textual output with no trailing newline.")
                 , (Print,  "Print multi-line user output.")
                 ]
             (Just Print) Nothing
         )

    <*> textOption
         ( short 'l'
        <> long "level"
        <> metavar "LEVEL"
        <> completes "Log level of AWS messages to emit."
             "The following log levels are supported:"
                 [ (Error, "Service errors and exceptions.")
                 , (Debug, "Requests and responses.")
                 , (Trace, "Sensitive signing metadata.")
                 , (Info,  "No logging of library routines.")
                 ]
             (Just Info) Nothing
         )

keyOption :: Parser KeyId
keyOption = textOption
    ( short 'k'
   <> long "key"
   <> metavar "ARN"
   <> defaults "The KMS Master Key Id to use."
       "Examples of KMS aliases or ARNs are:"
           [ ("arn:aws:kms:us-east-1:1234:key/12345678-1234", "")
           , ("arn:aws:kms:us-east-1:1234:alias/MyAliasName", "")
           , ("12345678-1234-1234-12345",                     "")
           , ("alias/MyAliasName",                            "")
           ]
       (Just defaultKeyId)
       (Just "It's recommended to setup a new key using the default alias.")
    )

contextOption :: Parser Context
contextOption = fromPairs $ textOption
    ( short 'c'
   <> long "context"
   <> metavar "KEY=VALUE"
   <> describe "A key/value pair to add to the encryption context. \
               \The same context must be provided during encryption and decryption."
        (Just $ "You can enter multiple key/value pairs. For example:"
      </> indent 2 "-c foo=bar -c something=\"containing spaces\" ..."
        ) Optional
    )

nameOption :: Parser Name
nameOption = textOption
     ( short 'n'
    <> long "name"
    <> metavar "STRING"
    <> describe "The unique name of the credential." Nothing Required
     )

revisionOption :: Fact -> Parser Revision
revisionOption r = textOption
     ( short 'v'
    <> long "revision"
    <> metavar "STRING"
    <> describe "The revision of the credential." Nothing r
     )

forceFlag :: Parser Force
forceFlag = flag Prompt NoPrompt
     ( short 'f'
    <> long "force"
    <> help "Always overwrite or remove, without an interactive prompt."
     )

inputOption :: Parser Input
inputOption = textual <|> filepath
  where
    textual = Value
        <$> textOption
             ( short 's'
            <> long "secret"
            <> metavar "STRING"
            <> help "The unencrypted secret value of the credential."
             )

    filepath = Path
        <$> option str
             ( short 'p'
            <> long "path"
            <> metavar "PATH"
            <> help "A file to read as the contents of the unencrypted credential."
            <> action "file"
             )
