{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Apache, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Main (main) where

import           Data.ByteString            (ByteString)
import           Data.HashMap.Strict        (HashMap)
import           Data.Maybe
import qualified Data.Text                  as Text
import           Network.AWS                (Region)
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.S3             (BucketName, ObjectVersionId)
import           Network.Credentials
import           Numeric.Natural
import           Options.Applicative        hiding (value)
import           Options.Applicative.Arrows

data Format
    = JSON
    | YAML
    | CSV
      deriving (Show)

instance FromText Format where
    parser = matchCI "json" JSON
         <|> matchCI "yaml" YAML
         <|> matchCI "csv"  CSV

newtype Key = Key Text
    deriving (Eq, Show, FromText)

data Value
    = Crypt !ByteString
    | Raw   !Text

instance Show Value where
    show (Crypt _) = "Crypt *****"
    show (Raw   _) = "Raw *****"

instance FromText Value where
    parser = Raw <$> Network.AWS.Data.parser

newtype Context = Context (HashMap Text Text)
    deriving (Show)

newtype Version = Version Natural
    deriving (Show, FromText)

data Store
    = Bucket BucketName Text
    | Table  Text
      deriving (Show)

data Mode
    = Delete Store         Key
    | Get    Store Version Key
    | Gets   Store Version Key Format
    | Put    Store Version Key Value
    | List   Store
    | Setup  Store
      deriving (Show)

main :: IO ()
main = do
    (r, m) <- customExecParser (prefs showHelpOnError) program
    print r
    print m

program :: ParserInfo (Region, Mode)
program = info (helper <*> parse) fullDesc
  where
    parse = subparser $ mconcat
        [ mode "delete"
            (Delete <$> store <*> key)
            "DELETE"

        , mode "get"
            (Get <$> store <*> version <*> key)
            "GET"

        , mode "gets"
            (Gets <$> store <*> version <*> key <*> format)
            "GETS"

        , mode "put"
            (Put <$> store <*> version <*> key <*> value)
            "PUT"

        , mode "list"
            (List <$> store)
            "LIST"

        , mode "setup"
            (Setup <$> store)
            "SETUP"
        ]

mode :: String -> Parser a -> String -> Mod CommandFields (Region, a)
mode m p h = command m $ info ((,) <$> region <*> p) (progDesc h)

region :: Parser Region
region = option text
     ( long "region"
    <> metavar "REGION"
    <> help "Region to operate in."
     )

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

store :: Parser Store
store = bucket <|> table
  where
    bucket = Bucket
        <$> option text
             ( long "bucket-name"
            <> metavar "BUCKET"
            <> help "Bucket name."
             )

        <*> (def $ option text
             ( long "object-prefix"
            <> metavar "PREFIX"
            <> help "Object prefix."
             ))

    table = Table
        <$> option text
             ( long "table-name"
            <> metavar "TABLE"
            <> help "Table name."
             )

context :: Parser Context
context = pure (Context mempty)

value :: Parser Value
value = option text
     ( long "value"
    <> metavar "VALUE"
    <> help "Value."
     )

format :: Parser Format
format = option text
     ( long "format"
    <> metavar "FORMAT"
    <> help "Output format."
     )

text :: FromText a => ReadM a
text = eitherReader (fromText . Text.pack)

def :: (Alternative f, Monoid a, FromText a) => f a -> f a
def = fmap (fromMaybe mempty) . optional
