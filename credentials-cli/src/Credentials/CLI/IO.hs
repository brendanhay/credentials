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
-- Module      : Credentials.CLI.IO
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.CLI.IO where

import           Control.Monad.Reader
import           Credentials
import           Credentials.CLI.Emit
import           Credentials.CLI.Types
import           Data.Aeson               (ToJSON (..))
import           Data.Aeson.Encode
import           Data.Aeson.Encode.Pretty
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.ByteString.Builder  (Builder)
import           Data.ByteString.Builder  (hPutBuilder)
import qualified Data.ByteString.Builder  as Build
import qualified Data.ByteString.Char8    as BS8
import           Data.Char
import           Data.Conduit             (($$))
import qualified Data.Conduit.List        as CL
import           Data.Data
import           Data.Monoid
import qualified Data.Text                as Text
import           Network.AWS.Data.Log
import           System.Exit
import           System.IO

default (Builder)

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

emit :: (ToJSON (Emit a), ToLog (Emit a)) => Store -> a -> App ()
emit s x = do
    f <- asks format
    let p = f `elem` [Pretty, Print]
        e = Emit p (s, x)
    liftIO . hPutBuilder stdout $
        case f of
            Pretty -> build (encodePretty e) <> "\n"
            JSON   -> encodeToByteStringBuilder (toJSON e)
            Echo   -> build e
            Print  -> build e <> "\n"

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
