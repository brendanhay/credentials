{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

-- |
-- Module      : Network.Credentials.Store.Dynamo
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.Credentials.Store.Dynamo where

import           Conduit                   hiding (await, scan)
import           Control.Applicative
import           Control.Exception.Lens
import           Control.Lens              hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Free.Class
import           Control.Monad.Reader      (ask)
import           Data.Bifunctor
import           Data.ByteString           (ByteString)
import qualified Data.Conduit.List         as CL
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as Map
import           Data.List                 (sort)
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as Text
import           Data.Typeable
import           Network.AWS
import           Network.AWS               (runAWS)
import           Network.AWS               (AWS, liftAWS)
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

setup :: MonadAWS m => Text -> m Setup
setup n = do
    p <- exists n
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

cleanup :: MonadAWS m => Text -> m ()
cleanup n = do
    p <- exists n
    when p $ do
        void $ send (deleteTable n)
        void $ await tableNotExists (describeTable n)

exists :: MonadAWS m => Text -> m Bool
exists n = paginate listTables
    =$= concatMapC (view ltrsTableNames)
     $$ (isJust <$> findC (== n))

list :: (MonadCatch m, MonadAWS m) => Text -> m [(Name, NonEmpty Version)]
list n = catching_ _ResourceNotFoundException go $ throwM (NotSetup (Table n))
  where
    go = paginate req
        =$= CL.concatMapM (traverse attrs . view srsItems)
        =$= CL.groupOn1 fst
        =$= CL.map group
         $$ CL.consume

    req = scan n & sAttributesToGet ?~ fieldName :| [fieldVersion]

    group ((k, v), xs) = (k, v :| map snd (sort xs))

    attrs m = (,)
        <$> value fieldName    avS m
        <*> value fieldVersion avN m

    value k l m = require k m >>= parse k l

    parse k l a = maybe (missing k) (either (invalid k) pure . fromText) (a ^. l)
    require k m = maybe (missing k) pure (Map.lookup k m)

    missing k   = throwM (Missing k)
    invalid k e = throwM (Invalid k e)

-- # print list of credential names and versions,
-- # sorted by name and then by version
-- max_len = max([len(x["name"]) for x in credential_list])
-- for cred in sorted(credential_list,
--                    key=operator.itemgetter("name", "version")):
--     print("{0:{1}} -- version {2:>}".format(
--         cred["name"], max_len, cred["version"]))
