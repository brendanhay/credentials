{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Credentials
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials
    -- * Storage Interface
    ( Storage (..)

    -- -- * Encryption and Decryption
    -- , put
    -- , get

    -- * Re-exported Types
    , module Credentials.Types
    ) where

-- import           Control.Monad.Catch
-- import           Credentials.Encryption
import           Credentials.Types
-- import           Data.Typeable
-- import           Network.AWS

-- put :: (MonadThrow m, MonadAWS m, Storage m, Typeable m)
--     => KeyId
--     -> Context
--     -> Name
--     -> Value
--     -> Ref m
--     -> m Revision
-- put k c n x r = do
--     s <- encrypt k c n x
--     insert n s r

-- get :: (MonadThrow m, MonadAWS m, Storage m)
--     => Context
--     -> Name
--     -> Maybe Revision
--     -> Ref m
--     -> m (Value, Revision)
-- get c n mv r = do
--     (s, v) <- select  n mv r
--     x      <- decrypt c n s
--     return (x, v)
