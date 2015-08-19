{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}

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

import           Control.Lens               (view, (&), (.~), (<&>))
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS
import           Data.ByteString            (ByteString)
import           Data.ByteString.Builder    (Builder)
import           Data.HashMap.Strict        (HashMap)
import           Data.Maybe
import qualified Data.Text                  as Text
import           Network.AWS                (Region)
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.S3             (BucketName, ObjectVersionId)
import           Network.Credentials        as Cred
import           Numeric.Natural
import           Options.Applicative
import           Options.Applicative.Arrows
import           System.IO

default (Builder)

data Line
    = Append
    | Ignore
      deriving (Show)

data Force
    = NoPrompt
    | Prompt
      deriving (Show)

data Format
    = JSON
    | YAML
    | CSV
    | Shell
      deriving (Show)

instance FromText Format where
    parser = matchCI "json"  JSON
         <|> matchCI "yaml"  YAML
         <|> matchCI "csv"   CSV
         <|> matchCI "shell" Shell

data Mode
    = Setup   !Store !Force
    | Cleanup !Store !Force
    | List    !Store !Format
    | Get     !Store !Key !(Maybe Version) !Line
    | GetAll  !Store !Format
    | Put     !Store !Key !Value
    | Del     !Store !Key !Version !Force
    | DelAll  !Store !Key !Force
      deriving (Show)

main :: IO ()
main = do
    (v, r, m) <- customExecParser settings options
    l         <- newLogger v stdout
    e         <- newEnv r Discover <&> envLogger .~ l

    runResourceT . runAWST e $
        case m of
            Setup s f -> do
                say $ "Setting up " <> build s
                x <- Cred.setup s
                say x

            Cleanup s f -> do
                say $ "Initiating cleanup of " <> build s
                Cred.cleanup s
                say "Cleanup completed."

            _         -> say $ show m

say :: (MonadIO m, MonadReader r m, HasEnv r, ToLog a) => a -> m ()
say x = do
    f <- view envLogger
    liftIO (f Info (build x))

settings :: ParserPrefs
settings = prefs (showHelpOnError <> columns 100)

options :: ParserInfo (LogLevel, Region, Mode)
options = info (helper <*> (top <$> level <*> sub)) fullDesc
  where
    top v (r, m) = (v, r, m)

    sub = subparser $ mconcat
        [ mode "setup"
            (Setup <$> store <*> force)
            "setup"

        , mode "cleanup"
            (Cleanup <$> store <*> force)
            "cleanup"

        , mode "list"
            (List <$> store <*> format)
            "list"

        , mode "get"
            (Get <$> store <*> key <*> optional version <*> line)
            "get"

        , mode "get-all"
            (GetAll <$> store <*> format)
            "get-all"

        , mode "put"
            (Put <$> store <*> key <*> val)
            "put"

        , mode "delete"
            (Del <$> store <*> key <*> version <*> force)
            "delete"

        , mode "delete-all"
            (DelAll <$> store <*> key <*> force)
            "delete-all"
        ]

mode :: String -> Parser a -> String -> Mod CommandFields (Region, a)
mode m p h = command m $ info ((,) <$> region <*> p) (progDesc h)

region :: Parser Region
region = option text
     ( long "region"
    <> metavar "REGION"
    <> help "Region to operate in."
     )

level :: Parser LogLevel
level = option (eitherReader r)
     ( long "level"
    <> metavar "LEVEL"
    <> help "Log level to emit. One of debug|info|error. [default: info]"
    <> value Info
     )
  where
    r "debug" = Right Debug
    r "info"  = Right Info
    r "error" = Right Error
    r e       = Left $ "Unrecognised log level: " ++ e

version :: Parser Version
version = option text
     ( long "version"
    <> metavar "VERSION"
    <> help "Version number."
     )

key :: Parser Key
key = option text
     ( long "key"
    <> metavar "KEY"
    <> help "Key."
     )

val :: Parser Value
val = option text
     ( long "value"
    <> metavar "VALUE"
    <> help "Value."
     )

store :: Parser Store
store = bucket <|> table <|> pure defaultStore
  where
    bucket = Bucket
        <$> option text
             ( long "bucket"
            <> metavar "BUCKET"
            <> help "Bucket name."
             )

        <*> (optional $ option text
             ( long "prefix"
            <> metavar "PREFIX"
            <> help "Object prefix."
             ))

    table = Table
        <$> option text
             ( long "table"
            <> metavar "TABLE"
            <> help "Table name."
             )

context :: Parser Context
context = pure (Context mempty)

format :: Parser Format
format = option text
     ( long "format"
    <> metavar "FORMAT"
    <> help "Output format. One of json|yaml|csv|shell. [default: shell]"
    <> value Shell
     )

line :: Parser Line
line = flag Append Ignore
     ( short 'n'
    <> long "no-line"
    <> help "Don't append a newline to the output. [default: off]"
     )

force :: Parser Force
force = flag Prompt NoPrompt
     ( long "force"
    <> help "Always overwrite or remove, without prompting."
     )

text :: FromText a => ReadM a
text = eitherReader (fromText . Text.pack)
