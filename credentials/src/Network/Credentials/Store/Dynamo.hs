{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Credentials.Store.Dynamo
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.Credentials.Store.Dynamo where

import           Conduit                   hiding (await)
import           Control.Exception.Lens
import           Control.Lens              hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Free.Class
import           Control.Monad.Trans.AWS
import           Data.ByteString           (ByteString)
import           Data.HashMap.Strict       (HashMap)
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Maybe
import qualified Data.Text                 as Text
import           Network.AWS               (Region)
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.DynamoDB
import           Network.AWS.S3
import           Network.AWS.S3            (BucketName, ObjectVersionId)
import           Network.Credentials.Types
import           Numeric.Natural

fieldName, fieldVersion, fieldKey, fieldContents, fieldHMAC :: Text
fieldName     = "name"
fieldVersion  = "version"
fieldKey      = "key"
fieldContents = "contents"
fieldHMAC     = "hmac"

setup :: (MonadCatch m, MonadFree Command m)
      => Text
      -> m Setup
setup n = do
    p <- tableDefined n
    unless p $ do
        let keys = keySchemaElement fieldName    Hash
               :| [keySchemaElement fieldVersion Range]
            iops = provisionedThroughput 1 1
            attr = ctAttributeDefinitions .~
                [ attributeDefinition fieldName S
                , attributeDefinition fieldVersion N
                ]
        void $ send (createTable n keys iops & attr)
        void $ await tableExists (describeTable n)
    return $
        if p
            then Exists
            else Created

cleanup :: MonadFree Command m => Text -> m ()
cleanup n = do
    p <- tableDefined n
    when p $ do
        void $ send (deleteTable n)
        void $ await tableNotExists (describeTable n)

tableDefined :: MonadFree Command m => Text -> m Bool
tableDefined n =
        paginate listTables
    =$= concatMapC (view ltrsTableNames)
     $$ (isJust <$> findC (== n))

