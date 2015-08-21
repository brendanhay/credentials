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

class Storage s where
    type Engine s :: * -> *
    data Ref    s :: *

    engine  :: s a -> Engine s a

    setup   ::                          Ref s -> s Setup
    cleanup ::                          Ref s -> s ()
    list    ::                          Ref s -> s [(Name, NonEmpty Version)]
    insert  :: Name -> Secret        -> Ref s -> s Version
    select  :: Name -> Maybe Version -> Ref s -> s (Maybe Secret)

    -- delete    :: a -> Name -> Version       -> M ()
    -- deleteAll :: a -> Name                  -> M ()

put :: (MonadThrow s, MonadAWS s, Storage s)
    => KeyId
    -> Context
    -> Name
    -> Value
    -> Ref s
    -> s Version
put k c n v r = do
    s <- encrypt k c v
    insert n s r

--get ::
