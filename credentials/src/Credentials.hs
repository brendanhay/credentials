{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

-- |
-- Module      : Credentials
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials
    ( DB.Dynamo
    , DB.defaultTable

    -- * Storage Interface
    , Storage
    , Layer
    , Ref
    , setup
    , cleanup
    , list
    , put
    , get
    , delete
    , deleteAll

    -- * Re-exported Types
    , module Credentials.Types
    ) where

import           Conduit                  hiding (await)
import           Control.Exception.Lens
import           Control.Lens             hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Credentials.Secret
import qualified Credentials.Store.Dynamo as DB
import qualified Credentials.Store.S3     as S3
import           Credentials.Types
import           Data.ByteString          (ByteString)
import           Data.Either
import           Data.HashMap.Strict      (HashMap)
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Maybe
import qualified Data.Text                as Text
import           Network.AWS
import           Network.AWS
import           Network.AWS              (Region)
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Numeric.Natural

put :: (MonadThrow m, MonadAWS m, Storage m)
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
