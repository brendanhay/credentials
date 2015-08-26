{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

-- |
-- Module      : Credentials
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials
    -- * Storage Interface
    ( Storage (..)

    -- * Encryption and Decryption
    , put
    , get

    -- * Re-exported Types
    , module Credentials.Types
    ) where

import           Control.Exception.Lens
import           Control.Lens           hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Credentials.Secret
import           Credentials.Types
import           Data.ByteString        (ByteString)
import           Data.Either
import           Data.HashMap.Strict    (HashMap)
import           Data.List.NonEmpty     (NonEmpty (..))
import           Data.Maybe
import qualified Data.Text              as Text
import           Data.Typeable
import           Network.AWS
import           Network.AWS
import           Network.AWS            (Region)
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Numeric.Natural

put :: (MonadThrow m, MonadAWS m, Storage m, Typeable m)
    => KeyId
    -> Context
    -> Name
    -> Value
    -> Ref m
    -> m Version
put k c n x r = do
    s <- encrypt k c n x
    insert n s r

get :: (MonadThrow m, MonadAWS m, Storage m)
    => Context
    -> Name
    -> Maybe Version
    -> Ref m
    -> m (Value, Version)
get c n mv r = do
    (s, v) <- select  n mv r
    x      <- decrypt c n s
    return (x, v)
