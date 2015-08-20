{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
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

import           Control.Exception.Lens
import           Control.Lens               (view, ( # ), (&), (.~), (<&>))
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Data
-- import           Control.Monad.Trans.AWS
import           Data.ByteString            (ByteString)
import           Data.ByteString.Builder    (Builder)
import qualified Data.ByteString.Builder    as Build
import           Data.Char
import           Data.Conduit               (($$))
import qualified Data.Conduit.List          as CL
import           Data.HashMap.Strict        (HashMap)
import           Data.Maybe
import qualified Data.Text                  as Text
import           Network.AWS
-- import           Network.AWS                (Region)
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.S3             (BucketName, ObjectVersionId)
import           Network.Credentials        as Cred
import           Numeric.Natural
import           Options.Applicative
import           Options.Applicative.Arrows
import           System.Exit
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

data Agree
    = Yes
    | No
    | What String

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
    = Setup   !Store
    | Cleanup !Store !Force
    | List    !Store !Format
    | Put     !Store !Name !Value
    | Get     !Store !Name !(Maybe Version) !Line
    | GetAll  !Store !Format
    | Del     !Store !Name !Version !Force
    | DelAll  !Store !Name !Force
      deriving (Show)

main :: IO ()
main = do
    (v, r, m) <- customExecParser settings options
    l         <- newLogger v stdout
    e         <- newEnv r Discover <&> envLogger .~ l

    exit . runResourceT . runAWS e $ do
        case m of
            Setup s -> do
                say $ "Creating table " <> build s <> " in " <> build r <> "."
                x <- Cred.setup s
                say $ "Table " <> build s <> " " <> build x <> "."

            Cleanup s f -> do
                say $ "This will delete table " <> build s <> " from " <> build r <> "!"
                prompt f $ do
                    Cred.cleanup s
                    say $ "Table " <> build s <> " deleted."

            List s f -> do
                say $ "Listing " <> build s <> " in " <> build r
                xs <- Cred.list s
                forM_ xs $ \(n, vs) ->
                    say $ build n <> " -- versions " <> build (show vs)

settings :: ParserPrefs
settings = prefs (showHelpOnError <> columns 100)

options :: ParserInfo (LogLevel, Region, Mode)
options = info (helper <*> (top <$> level <*> sub)) desc
  where
    desc = mconcat
        [ fullDesc
        , progDesc "Administration CLI for credential and secret storage."
        , footer $
            "    If --table or --bucket isn't specified, "
                ++ show defaultStore ++ " will be used."
        ]

    top v (r, m) = (v, r, m)

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
            (Get <$> store <*> name <*> optional version <*> line)
            "Fetch and decrypt a specific version of the credential from the store. \
            \Defaults to latest version."

        , mode "get-all"
            (GetAll <$> store <*> format)
            "Fetch and decrypt all secrets in the credential store. \
            \Defaults to the latest version per credential."

        , mode "put"
            (Put <$> store <*> name <*> val)
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
    <> completeWith possibleRegions
     )

level :: Parser LogLevel
level = option (eitherReader r)
     ( long "level"
    <> metavar "LEVEL"
    <> help "Log message level to emit. One of debug|info|error. [default: info]"
    <> value Info
     )
  where
    r "debug" = Right Debug
    r "info"  = Right Info
    r "error" = Right Error
    r e       = Left $ "Unrecognised log level: " ++ e

name :: Parser Name
name = option text
     ( long "name"
    <> metavar "STRING"
    <> help "The unique name of the credential."
     )

val :: Parser Value
val = option text
     ( long "value"
    <> metavar "STRING"
    <> help "The raw unencrypted value of the credential."
     )

version :: Parser Version
version = option text
     ( long "version"
    <> metavar "NUMBER"
    <> help "A specific credential version."
     )

store :: Parser Store
store = bucket <|> table <|> pure defaultStore
  where
    bucket = Bucket
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

    table = Table
        <$> option text
             ( long "table"
            <> metavar "STRING"
            <> help "DynamoDB table name to use for credential storage."
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
     ( short 'f'
    <> long "force"
    <> help "Always overwrite or remove, without an interactive prompt."
     )

text :: FromText a => ReadM a
text = eitherReader (fromText . Text.pack)

exit :: IO () -> IO ()
exit io = do
    void $ catches io
        [ handler _NotSetup $ \s ->
            quit 2 ("Credential store " <> build s <> " doesn't exist. Please run setup.")

        -- , hd 3 _Invalid
        -- , hd 4 _Missing
        ]
    exitSuccess
  where
     quit n m = err m >> exitWith (ExitFailure n)

say :: (MonadIO m, ToLog a) => a -> m ()
say x = liftIO $ Build.hPutBuilder stdout (build x <> "\n") >> hFlush stdout

err :: (MonadIO m, ToLog a) => a -> m ()
err x = liftIO $ Build.hPutBuilder stderr ("Error! " <> build x <> "\n")

prompt :: MonadIO m => Force -> m () -> m ()
prompt NoPrompt io = say "Running ..." >> io
prompt Prompt   io = do
    liftIO $ hPutStr stdout " -> Proceed? [y/n]: " >> hFlush stdout
    a <- agree
    case a of
        Yes    -> say "Running ..." >> io
        No     -> say "Cancelling ..."
        What w -> say $ build w <> ", what? Cancelling ..."

agree :: MonadIO m => m Agree
agree = do
    r <- map toLower . filter (not . isSpace) <$> liftIO getLine
    return $! case r of
        "yes" -> Yes
        "y"   -> Yes
        "no"  -> No
        "n"   -> No
        ""    -> No
        x     -> What x

possibleRegions :: [String]
possibleRegions = map (str . fromConstr)
    . dataTypeConstrs
    $ dataTypeOf Frankfurt
  where
    str :: Region -> String
    str = Text.unpack . toText
