{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Credentials.IO
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.IO where

import           Control.Exception.Lens
import           Control.Lens                         (view, ( # ), (&), (.~),
                                                       (<&>))
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Credentials.Types
import           Data.ByteString                      (ByteString)
import           Data.ByteString.Builder              (Builder)
import qualified Data.ByteString.Builder              as Build
import qualified Data.ByteString.Char8                as BS8
import           Data.Char
import           Data.Conduit                         (($$))
import qualified Data.Conduit.List                    as CL
import           Data.Data
import           Data.HashMap.Strict                  (HashMap)
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.List.NonEmpty                   as NE
import           Data.Maybe
import qualified Data.Text                            as Text
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.S3                       (BucketName,
                                                       ObjectVersionId)
import           Network.Credentials                  as Cred
import           Numeric.Natural
import           Options.Applicative
import           Options.Applicative.Builder.Internal (HasCompleter)
import           System.Exit
import           System.IO

data Agree
    = Yes
    | No
    | What String

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
