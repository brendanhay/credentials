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
                say "Done."

settings :: ParserPrefs
settings = prefs (showHelpOnError <> columns 100)

options :: ParserInfo (LogLevel, Region, Mode)
options = info (helper <*> (top <$> level <*> sub)) fullDesc
  where
    top v (r, m) = (v, r, m)

    sub = subparser $ mconcat
        [ mode "setup"
            (Setup <$> store)
            "setup"

        , mode "cleanup"
            (Cleanup <$> store <*> force)
            "cleanup"

        , mode "list"
            (List <$> store <*> format)
            "list"

        , mode "get"
            (Get <$> store <*> name <*> optional version <*> line)
            "get"

        , mode "get-all"
            (GetAll <$> store <*> format)
            "get-all"

        , mode "put"
            (Put <$> store <*> name <*> val)
            "put"

        , mode "delete"
            (Del <$> store <*> name <*> version <*> force)
            "delete"

        , mode "delete-all"
            (DelAll <$> store <*> name <*> force)
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

name :: Parser Name
name = option text
     ( long "name"
    <> metavar " NAME"
    <> help "Name."
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
     ( short 'f'
    <> long "force"
    <> help "Always overwrite or remove, without prompting."
     )

text :: FromText a => ReadM a
text = eitherReader (fromText . Text.pack)

exit :: IO () -> IO ()
exit io = do
    void $ catches io
        [ handler _NotSetup $ \s ->
            quit 2 ("Credential store " <> build s <> " doesn't exist.")

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
