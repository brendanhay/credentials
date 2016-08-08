{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
-- This module provides a common interface for operating on your shared
-- credentials.
module Credentials
    (
    -- * Usage
    -- $usage

    -- * Operations
      insert
    , select
    , delete
    , truncate
    , revisions
    , setup
    , teardown

    -- * KMS
    , KeyId             (..)
    , defaultKeyId

    -- * DynamoDB
    , DynamoTable       (..)
    , defaultTable

    -- * Errors
    , CredentialError   (..)
    , AsCredentialError (..)

    -- * Types
    , Name              (..)
    , Revision          (..)
    , Context           (..)
    , Setup             (..)
    ) where

import Credentials.DynamoDB
import Credentials.Types

-- $usage
-- To use the library, make sure you have met the following prerequisites:
--
-- * You have a master key in KMS. You can create this under Identity and Access
--   Management > Encryption Keys, in the AWS developer console.
--
-- * Your AWS access credentials are available where
--   <https://hackage.haskell.org/package/amazonka amazonka> can find them. This
--   will be automatic if you are running on an EC2 host, otherwise
--   the <https://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs ~\/.aws\/credentials> file, as @AWS_ACCESS_KEY_ID@ and
--   @AWS_SECRET_ACCESS_KEY@ environment variables need to be configured.
--
-- Since all of the credentials operations are constrained by a 'MonadAWS' context,
-- running them is identical to that of [amazonka](https://hackage.haskell.org/package/amazonka),
-- which you will also need to add to your @build-depends@ section of your project's cabal file.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Credentials
-- > import Control.Lens
-- > import Data.ByteString (ByteString)
-- > import Network.AWS
-- > import System.IO
-- >
-- > example :: IO (ByteString, Revision)
-- > example = do
-- >     -- A new 'Logger' to replace the default noop logger is created,
-- >     -- which will print AWS debug information and errors to stdout.
-- >     lgr <- newLogger Debug stdout
-- >
-- >     -- A new amazonka 'Env' is created, which auto-discovers the
-- >     -- underlying host credentials.
-- >     env <- newEnv Frankfurt Discover
-- >
-- >     let table = "dynamo-table-name"
-- >         key   = "kms-key-alias"
-- >         name  = "credential-name"
-- >
-- >     -- We now run the 'AWS' computation with the overriden logger,
-- >     -- performing the sequence of credentials operations.
-- >     runResourceT . runAWS (env & envLogger .~ lgr) $ do
-- >         -- Firstly, we create the DynamoDB table.
-- >         -- This is an idempotent operation but since it makes remote API calls,
-- >         -- it's recommended you only run this once via the CLI.
-- >         Credentials.setup table
-- >
-- >         -- Then we insert a credential\'s value, for a given name.
-- >         -- Encryption is handled transparently and the resulting revision
-- >         -- is returned.
-- >         _ <- Credentials.insert key mempty name "a-super-secret-value" table
-- >
-- >         -- Selecting the credential by name, and specifying 'Nothing' for the
-- >         -- revision results in the latest credential revision being returned.
-- >         Credentials.select mempty name Nothing table
