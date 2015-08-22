{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Network.Credentials
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.Credentials.Store where

import           Conduit                    hiding (await)
import           Control.Exception.Lens
import           Control.Lens               hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Data.ByteString            (ByteString)
import           Data.Either
import           Data.HashMap.Strict        (HashMap)
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.Maybe
import qualified Data.Text                  as Text
import           Network.AWS
import           Network.AWS
import           Network.AWS                (Region)
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.Credentials.Secret
import           Network.Credentials.Types
import           Numeric.Natural

class Storage m where
    type Layer m :: * -> *
    type Ref   m :: *

    layer   :: m a -> Layer m a

    setup   ::                              Ref m -> m Setup
    cleanup ::                              Ref m -> m ()
    list    ::                              Ref m -> m [(Name, NonEmpty Version)]
    insert  :: Name -> Version -> Secret -> Ref m -> m ()
    select  :: Name -> Maybe Version     -> Ref m -> m Secret

    -- delete    :: a -> Name -> Version       -> M ()
    -- deleteAll :: a -> Name                  -> M ()

put :: (MonadThrow m, MonadAWS m, Storage m)
    => KeyId
    -> Context
    -> Name
    -> Version
    -> Value
    -> Ref m
    -> m ()
put k c n v x r = do
    s <- encrypt k c x
    insert n v s r

get :: (MonadThrow m, MonadAWS m, Storage m)
    => Context
    -> Name
    -> Maybe Version
    -> Ref m
    -> m Value
get c n v r = select n v r >>= decrypt c
