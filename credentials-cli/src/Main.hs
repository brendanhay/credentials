{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

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
import           Control.Lens                 (view, ( # ), (&), (.~), (<&>))
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Credentials                  as Store hiding (context)
import           Credentials.CLI.IO
import           Credentials.CLI.Options
import           Credentials.CLI.Types
import           Data.Bifunctor
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString.Builder      as Build
import qualified Data.ByteString.Char8        as BS8
import           Data.Char
import           Data.Conduit                 (($$))
import qualified Data.Conduit.List            as CL
import           Data.Data
import           Data.HashMap.Strict          (HashMap)
import           Data.List                    (foldl', sort)
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.List.NonEmpty           as NE
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.S3               (BucketName, ObjectVersionId)
import           Numeric.Natural
import           Options.Applicative          hiding (optional)
import qualified Options.Applicative          as Opt
import           System.Exit
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen (Doc, bold, indent, (<+>), (</>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

default (Builder, Text)

-- Deleting:
--   how about tombstones, or some thought out plan for how the revision number
--   semantics are impacted by deletes.

--   Some opaque revision format returned?
--     sha (revision + timestamp)?

-- Large File Storage:
--   have a pointer to something in S3, or actually store everything there?
--   is S3's revisioning enough?

-- Is Revision a better name than Revision due to the opaqueness?

-- An optional comment for revisions?

-- Val:
--   rename to Table -> Item -> Attribute

-- Do the same formatting/pprint of mode progDesc to layout the main
-- credentials help view.

-- BUG:
-- when printing the store URI:
--  dynamo:///name -> dynamo:/name

-- Add ability to single line shell, or normal verbosity.

main :: IO ()
main = do
    (r, v, m) <- customExecParser settings options

    l <- newLogger v stdout
    e <- newEnv r Discover <&> (envLogger .~ l) . setStore (current m)

    catches (runApp e (program r m))
        [ handler _CredentialError $ \x -> quit 1 (show x)
        ]

program :: Region -> Mode -> App ()
program r = \case
    Setup s -> do
        says ("Setting up " % s <> " in " % r <> ".")
        x <- Store.setup s
        says $ "Created " % s % " " % x % "."

    Cleanup s f -> do
        says ("This will delete " % s % " from " % r % "!")
        prompt f $ do
            Store.cleanup s
            says ("Deleted " % s % ".")

    List s f -> do
        xs <- list s
        say (Output f (s, xs))

    Put s k c n i f -> do
        x <- case i of
            Raw  v -> pure v
            Path p -> do
                when (f == Shell) $
                    says ("Reading secret from " % p % "...")
                Value <$> liftIO (BS.readFile p)

        v <- Store.put k c n x s
        say (Output f (n, v))

    Get s c n v f -> do
        x <- Store.get c n v s
        say (Output f (n, x))

    Delete s n v f -> do
        says ("This will delete revision " % v % " of " % n % " from " % s % " in " % r % "!")
        prompt f $ do
            Store.delete n v s
            says ("Deleted revision " % v % " of " % n % ".")

settings :: ParserPrefs
settings = prefs (showHelpOnError <> columns 90)

options :: ParserInfo (Region, LogLevel, Mode)
options = info (helper <*> modes) (fullDesc <> headerDoc (Just desc))
  where
    desc = bold "credentials"
        <+> "- Provides a unified interface for managing secure, shared credentials."

    modes = subparser $ mconcat
        [ mode "setup"
            (Setup <$> store)
            "Setup a new credential store."
            "Foo "

        , mode "cleanup"
            (Cleanup <$> store <*> force)
            "Remove the credential store entirely."
            "Bar"

        , mode "list"
            (List <$> store <*> format)
            "List all credential names and their respective revisions."
            "The -u,--uri option takes a URI conforming to one of the following protocols"

        , mode "get"
            (Get <$> store <*> context <*> require name <*> optional revision <*> format)
            "Fetch and decrypt a specific revision of a credential."
            "Defaults to the latest available revision, if --revision is not specified."

        , mode "put"
            (Put <$> store <*> key <*> context <*> require name <*> input <*> format)
            "Write and encrypt a new revision of a credential to the store."
            "You can supply the secret as a string with --secret, or as \
            \a file path to the secret's contents using --path."

        , mode "delete"
            (Delete <$> store <*> require name <*> require revision <*> force)
            "Remove a specific revision of a credential from the store."
            "Foo"

        , mode "truncate"
            (DeleteAll <$> store <*> optional name <*> retain <*> force)
            "Remove multiple revisions of a credential from the store."
            "If no credential name is specified, it will operate on all \
            \credentials. Defaults to removing all but the latest revision."
        ]

mode :: String
     -> Parser a
     -> String
     -> Doc
     -> Mod CommandFields (Region, LogLevel, a)
mode n p h foot = command n (info ((,,) <$> region <*> level <*> p) desc)
  where
    desc = fullDesc <> progDesc h <> footerDoc (Just (indent 2 foot))

region :: Parser Region
region = option text
     ( short 'r'
    <> long "region"
    <> metavar "REGION"
    <> completes "The AWS region in which to operate."
         "The following regions are supported:"
             (map (second (PP.text . show) . join (,)) unsafeEnum)
         Frankfurt Nothing
     )

level :: Parser LogLevel
level = option text
     ( short 'l'
    <> long "level"
    <> metavar "LEVEL"
    <> completes "Log level of AWS messages to emit."
         "The following log levels are supported:"
             [ (Error, "Service errors and exceptions.")
             , (Debug, "Requests and responses.")
             , (Trace, "Sensitive signing metadata.")
             ]
         Info Nothing
     )

store :: Parser Store
store = option text
     ( short 'u'
    <> long "uri"
    <> metavar "URI"
    <> defaults "URI specifying the storage system to use."
         "The URI format must be one of the following protocols:"
             [ ("dynamo://[host[:port]]/table-name", "Amazon DynamoDB")
             , ("s3://[host[:port]]/bucket-name[/prefix]", "Amazon S3")
             ]
         defaultStore
         (Just $ "If no host is specified for AWS services (ie. scheme:///path),"
             </> "then the AWS endpoints will be used if appropriate.")
     )

key :: Parser KeyId
key = option text
    ( short 'k'
   <> long "key"
   <> metavar "STRING"
   <> defaults "The KMS Master Key Id to use."
       "Examples of KMS aliases or ARNs are:"
           [ ("arn:aws:kms:us-east-1:1234:key/12345678-1234", "")
           , ("arn:aws:kms:us-east-1:1234:alias/MyAliasName", "")
           , ("12345678-1234-1234-12345", "")
           , ("alias/MyAliasName", "")
           ]
       defaultKeyId
       (Just "It's recommended to setup a new key using the default alias.")
    )

context :: Parser Context
context = toContext $ option text
    ( short 'c'
   <> long "context"
   <> metavar "KEY=VALUE"
   <> describe "A key/value pair to add to the encryption context."
        (Just $ "You can enter multiple key/value pairs. For example:"
      </> indent 2 "-c foo=bar -c something=\"containing spaces\" ..."
        ) Optional
    )

name :: Fact -> Parser Name
name r = option text
     ( long "name"
    <> metavar "STRING"
    <> describe "The unique name of the credential." Nothing r
     )

revision :: Fact -> Parser Revision
revision r = option text
     ( long "revision"
    <> metavar "STRING"
    <> describe "The revision of the secret." Nothing r
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
    <> completes "Output format for displaying retrieved credentials."
         "The following formats are supported:"
             [ (Pretty, "Pretty printed JSON.")
             , (JSON,   "JSON suitable for piping to another process.")
             , (Shell,  "Single or multi-line textual output.")
             ]
         Shell Nothing
     )

retain :: Parser Natural
retain = option text
     ( short 'k'
    <> long "keep"
    <> metavar "NUMBER"
    <> help "Number of revisions to retain when truncating. [default: latest]"
    <> value 1
     )

input :: Parser Input
input = textual <|> filepath
  where
    textual = Raw
        <$> option text
             ( short 's'
            <> long "secret"
            <> metavar "STRING"
            <> help "The unencrypted secret."
             )

    filepath = Path
        <$> option str
             ( short 'p'
            <> long "path"
            <> metavar "PATH"
            <> help "A file to read as the contents of the unencrypted secret."
            <> action "file"
             )
