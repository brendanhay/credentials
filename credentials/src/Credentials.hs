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
    (
    -- * Storage Interface
      Storage  (..)

    -- ** DynamoDB
    , DynamoDB
    , Ref (DynamoTable)
    , defaultTable

    -- * Re-exported Types
    , module Credentials.Types
    ) where

import Credentials.DynamoDB
import Credentials.Types
