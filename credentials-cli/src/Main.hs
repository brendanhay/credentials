{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Main (main) where

import           Control.Exception.Lens
import           Control.Lens                    ((.~), (<&>))
import           Control.Monad.Catch
import           Control.Monad.Morph             (hoist)
import           Control.Monad.Reader
import           Credentials                     hiding (context)
import           Credentials.CLI.Format
import           Credentials.CLI.IO
import           Credentials.CLI.Options
import           Credentials.CLI.Types
import           Data.ByteString.Builder         (Builder)
import           Data.Conduit
import           Data.Conduit.Lazy
import qualified Data.Conduit.List               as CL
import           Data.List.NonEmpty              (NonEmpty)
import           Data.Text                       (Text)
import           Network.AWS
import           Options.Applicative             hiding (optional)
import           Options.Applicative.Help.Pretty
import           System.IO

default (Builder, Text)

-- Large File Storage:
--   have a pointer to something in S3, or actually store everything there?

-- Streaming:
--   for storage engines that support it, when specifying a filepath
--   via the CLI, how to connect a conduit, what interface?

-- Revision:
--   an optional comment for revisions.

-- Val:
--   rename to Table -> Item -> Attribute

main :: IO ()
main = do
    (c, m) <- customExecParser (prefs showHelpOnError) options
    l      <- newLogger (level c) stderr
    e      <- newEnv (region c) Discover <&> (envLogger .~ l) . setStore c
    catches (runApp e c (program c m))
        [ handler _CredentialError $ \x -> quit 1 (show x)
        ]

program :: Options -> Mode -> App ()
program Options{region = r, store = s} = \case
    Setup -> do
        says ("Setting up " % s % " in " % r % ".")
        says "Running ..."
        setup s >>= emit . SetupR
        says "Done."

    Cleanup f -> do
        says ("This will delete " % s % " from " % r % "!")
        prompt f $ do
            cleanup s
            emit CleanupR
        says "Done."

    List -> do
        says ("Listing contents of " % s % " in " % r % "...")
        runLazy (listAll s) >>= emit . ListR
        says "Done."

    Put k c n i -> do
        says ("Writing new revision of " % n % " to " % s % " in " % r % "...")
        x <- load i
        insert k c n x s >>= emit . PutR n
        says "Done."

    Get c n v -> do
        say "Retrieving"
        case v of
            Nothing -> pure ()
            Just x  -> say (" revision " % x % " of")
        says (" " % n % " from " % s % " in " % r % "...")
        select c n v s >>= emit . uncurry (GetR n)
        says "Done."

    Delete n v f -> do
        says ("This will delete revision " % v % " of " % n % " from " % s % " in " % r % "!")
        prompt f $ do
            delete n (Just v) s
            emit (DeleteR n v)
            says ("Deleted revision " % v % " of " % n % ".")
        says "Done."

    Truncate n f -> do
        says ("This will delete all but the latest revision of " % n % " from " % s % " in " % r % "!")
        prompt f $ do
            delete n Nothing s
            emit (TruncateR n)
            says ("Truncated " % n % ".")
        says "Done."

options :: ParserInfo (Options, Mode)
options = info (helper <*> modes) (fullDesc <> headerDoc (Just desc))
  where
    desc = bold "credentials"
        <+> "- Administration tool for managing secure, shared credentials."

    modes = subparser $ mconcat
        [ mode "setup"
            (pure Setup)
            "Setup a new credential store."
            "This will run the necessary actions to create a new credential store. \
            \This action is idempotent and if the store already exists, \
            \the operation will succeed with exit status 0."

        , mode "cleanup"
            (Cleanup <$> force)
            "Remove a credential store."
            "Warning: This will completely remove the credential store. For some \
            \storage engines this action is irrevocable unless you specifically \
            \perform backups for your data."

        , mode "list"
            (pure List)
            "List credential names and their respective revisions."
            "The -u,--uri option takes a URI conforming to one of the following protocols"

        , mode "get"
            (Get <$> context <*> name <*> optional revision)
            "Fetch and decrypt a specific credential revision."
            "Defaults to the latest available revision, if --revision is not specified."

        , mode "put"
            (Put <$> key <*> context <*> name <*> input)
            "Write and encrypt a new credential revision."
            "You can supply the secret value as a string with --secret, or as \
            \a file path which contents' will be read by using --path."

        , mode "delete"
            (Delete <$> name <*> require revision <*> force)
            "Remove a specific credential revision."
            "Foo"

        , mode "truncate"
            (Truncate <$> name <*> force)
            "Truncate a specific credential's revisions."
            "Bar"
        ]

mode :: String -> Parser a -> Text -> Text -> Mod CommandFields (Options, a)
mode n p h f = command n (info ((,) <$> common <*> p) (fullDesc <> desc <> foot))
  where
    desc = progDescDoc (Just $ wrap h)
    foot = footerDoc   (Just $ indent 2 (wrap f) <> line)

common :: Parser Options
common = Options
    <$> textOption
         ( short 'r'
        <> long "region"
        <> metavar "REGION"
        <> completes "The AWS region in which to operate."
             "The following regions are supported:"
                 (map (,mempty) unsafeEnum)
             Frankfurt Nothing
         )

    <*> textOption
         ( short 'u'
        <> long "uri"
        <> metavar "URI"
        <> defaults "URI specifying the storage system to use."
             "The URI format must be one of the following protocols:"
                 [ ("dynamo:/[/host[:port]]/table-name", "")
                 , ("s3:/[/host[:port]]/bucket-name[/prefix]", "")
                 ]
             defaultStore
             (Just "If no host is specified for AWS services (ie. dynamo:/table-name), \
                   \the AWS endpoints will be used if appropriate.")
         )

    <*> textOption
         ( short 'o'
        <> long "output"
        <> metavar "FORMAT"
        <> completes "Output format for displaying retrieved credentials."
             "The following formats are supported:"
                 [ (Pretty, "Pretty printed JSON.")
                 , (JSON,   "Single-line JSON output.")
                 , (Echo,   "Single-line textual output.")
                 , (Print,  "Print multi-line user output.")
                 ]
             Print Nothing
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
             Info Nothing
         )

key :: Parser KeyId
key = textOption
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
       defaultKeyId
       (Just "It's recommended to setup a new key using the default alias.")
    )

context :: Parser Context
context = ctx $ textOption
    ( short 'c'
   <> long "context"
   <> metavar "KEY=VALUE"
   <> describe "A key/value pair to add to the encryption context."
        (Just $ "You can enter multiple key/value pairs. For example:"
      </> indent 2 "-c foo=bar -c something=\"containing spaces\" ..."
        ) Optional
    )

name :: Parser Name
name = textOption
     ( short 'n'
    <> long "name"
    <> metavar "STRING"
    <> describe "The unique name of the credential." Nothing Required
     )

revision :: Fact -> Parser Revision
revision r = textOption
     ( short 'v'
    <> long "revision"
    <> metavar "STRING"
    <> describe "The revision of the credential." Nothing r
     )

force :: Parser Force
force = flag Prompt NoPrompt
     ( short 'f'
    <> long "force"
    <> help "Always overwrite or remove, without an interactive prompt."
     )

input :: Parser Input
input = textual <|> filepath
  where
    textual = Raw
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
