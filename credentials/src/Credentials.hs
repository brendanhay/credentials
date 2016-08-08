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
-- $usage
module Credentials
    (
    -- * Default Storage Backend
      DynamoTable (..)
    , defaultTable

    -- * Storage Operations
    , insert
    , select
    , delete
    , revisions
    , setup
    , teardown

    -- * Re-exported Types
    , module Credentials.Types
    ) where

import Credentials.DynamoDB
import Credentials.Types

{- $usage
Basic usage requires that you have the following prerequisites met:

* You have a master key in KMS. You can create this under Identity and Access
  Management > Encryption Keys, in the AWS developer console.

* Your AWS access credentials are available where
  [amazonka](https://hackage.haskell.org/package/amazonka) can find them.
  Typically in the `~/.aws/credentials` file, or as `AWS_ACCESS_KEY_ID` and
  `AWS_SECRET_ACCESS_KEY` environment variables.

Since all of the credentials operations are within a 'MonadAWS' context,
running them is identical to that of [amazonka](https://hackage.haskell.org/package/amazonka).

@
{-# LANGUAGE OverloadedStrings #-}

import Credentials
import Control.Lens
import Data.ByteString (ByteString)
import Network.AWS
import System.IO

example :: IO (ByteString, Revision)
example = do
    -- A new 'Logger' to replace the default noop logger is created,
    -- which will print AWS debug information and errors to stdout.
    lgr <- newLogger Debug stdout

    -- A new amazonka 'Env' is created, which auto-discovers the
    -- underlying host credentials.
    env <- newEnv Frankfurt Discover

    let table = "dynamo-table-name"
        key   = "kms-key-alias"
        name  = "credential-name"

    -- We now run the 'AWS' computation with the overriden logger,
    -- performing the sequence of credentials operations.
    runResourceT . runAWS (env & envLogger .~ lgr) $ do
        -- Firstly, we create the DynamoDB table.
        -- This is an idempotent operation but since it makes remote API calls,
        -- it's recommended you only run this once via the CLI.
        Credentials.setup table

        -- Then we insert a credential's value, for a given name.
        -- Encryption is handled transparently and the resulting revision
        -- is returned.
        _ <- Credentials.insert key mempty name "a-super-secret-value" table

        -- Selecting the credential by name, and specifying 'Nothing' for the
        -- revision results in the latest credential revision being returned.
        Credentials.select mempty name Nothing table
@

-}
