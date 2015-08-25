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
-- Module      : Credentials.Admin.IO
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.Admin.IO where

import           Control.Monad.IO.Class
import           Credentials.Admin.Types
import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Char8   as BS8
import           Data.Char
import           Data.Conduit            (($$))
import qualified Data.Conduit.List       as CL
import           Data.Data
import           Data.Monoid
import qualified Data.Text               as Text
import           Network.AWS.Data.Log
import           System.Exit
import           System.IO

default (Builder)

data Agree
    = Yes
    | No
    | What String

quit :: ToLog a => Int -> a -> IO ()
quit n m = err m >> exitWith (ExitFailure n)

says :: (MonadIO m, ToLog a) => a -> m ()
says x = say (build x <> "\n")

say :: (MonadIO m, ToLog a) => a -> m ()
say x = liftIO $ Build.hPutBuilder stdout (build x) >> hFlush stdout

err :: (MonadIO m, ToLog a) => a -> m ()
err x = liftIO $ Build.hPutBuilder stderr ("Error! " <> build x <> "\n")

prompt :: MonadIO m => Force -> m () -> m ()
prompt NoPrompt io = says "Running ..." >> io
prompt Prompt   io = do
    say " -> Proceed? [y/n]: "
    a <- agree
    case a of
        Yes    -> says "Running ..." >> io
        No     -> says "Cancelling ..."
        What w -> says $ build w <> ", what? Cancelling ..."

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
