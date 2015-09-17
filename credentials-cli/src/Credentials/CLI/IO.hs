{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module      : Credentials.CLI.IO
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.CLI.IO where

import           Control.Arrow
import           Control.Monad.Reader
import           Credentials
import           Credentials.CLI.Format
import           Credentials.CLI.Types
import           Data.Aeson                      (ToJSON (..))
import           Data.Aeson.Encode
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString                 as BS
import           Data.ByteString.Builder         (Builder)
import           Data.ByteString.Builder         (hPutBuilder, stringUtf8)
import qualified Data.ByteString.Builder         as Build
import           Data.Char                       (isSpace, toLower)
import           Data.Monoid
import           Network.AWS.Data
import           Options.Applicative.Help.Pretty
import           System.Exit
import           System.IO

default (Builder)

data Agree
    = Yes
    | No
    | What String

quit :: ToLog a => Int -> a -> IO ()
quit n m = err m >> exitWith (ExitFailure n)

err :: (MonadIO m, ToLog a) => a -> m ()
err x = liftIO $ Build.hPutBuilder stderr ("Error!:\n  " <> build x <> "\n")

says :: ToLog a => a -> App ()
says x = say (build x <> "\n")

say :: ToLog a => a -> App ()
say x = do
    f <- asks format
    when (f == Print) $
        liftIO $ hPutBuilder stderr (build x)

emit :: Result -> App ()
emit r = do
    (f, s) <- asks (format &&& store)
    let e = Emit s r
    liftIO . hPutBuilder stdout $
        case f of
            Pretty -> build (encodePretty e) <> "\n"
            JSON   -> encodeToBuilder (toJSON e)
            Echo   -> build r
            Print  -> stringUtf8
                (displayS (renderPretty 0.4 80 (pretty e)) "") <> "\n"

load :: Input -> App Value
load (Raw  v) = return v
load (Path p) = do
    says ("Reading secret from " % p % "...")
    Value <$> liftIO (BS.readFile p)

prompt :: Force -> App () -> App ()
prompt NoPrompt io = says "Running ..." >> io
prompt Prompt   io = do
    say " -> Proceed? [y/n]: "
    a <- agree
    case a of
        Yes    -> says "Running ..." >> io
        No     -> says "Cancelling ..."
        What w -> says $ build w <> ", what? Cancelling ..."

agree :: App Agree
agree = do
    r <- map toLower . filter (not . isSpace) <$> liftIO getLine
    return $! case r of
        "yes" -> Yes
        "y"   -> Yes
        "no"  -> No
        "n"   -> No
        ""    -> No
        x     -> What x
