{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Credentials.DynamoDB
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.DynamoDB
    ( DynamoDB
    , TableName
    , defaultTable
    ) where

import           Control.Exception.Lens
import           Control.Lens             hiding (Context)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Retry
import           Credentials
import           Credentials.DynamoDB.Val
import           Data.Conduit             hiding (await)
import qualified Data.Conduit             as C
import qualified Data.Conduit.List        as CL
import qualified Data.HashMap.Strict      as Map
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Data.Maybe
import           Data.Ord
import           Data.Text                (Text)
import           Data.Typeable
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.DynamoDB

newtype DynamoDB a = DynamoDB { runDynamo :: AWS a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadAWS DynamoDB where
    liftAWS = DynamoDB

instance Storage DynamoDB where
    type  Layer DynamoDB = AWS
    newtype Ref DynamoDB = TableName Text
        deriving (Eq, Ord, Show, FromText, ToText, ToLog)

    layer     = runDynamo
    setup     = setup'
    cleanup   = cleanup'
    list      = list'
    insert    = insert'
    select    = select'
    delete    = delete'
--    deleteAll = deleteAll

type TableName = Ref DynamoDB

defaultTable :: TableName
defaultTable = TableName "credential-store"

setup' :: MonadAWS m => TableName -> m Setup
setup' t@(toText -> t') = do
    p <- exists t
    unless p $ do
        let keys = keySchemaElement nameField    Hash
               :| [keySchemaElement versionField Range]
            iops = provisionedThroughput 1 1
            attr = ctAttributeDefinitions .~
                [ attributeDefinition nameField    S
                , attributeDefinition versionField N
                ]
        void $ send (createTable t' keys iops & attr)
        void $ await tableExists (describeTable t')
    return $ if p then Exists else Created

cleanup' :: MonadAWS m => TableName -> m ()
cleanup' t@(toText -> t') = do
    p <- exists t
    when p $ do
        void $ send (deleteTable t')
        void $ await tableNotExists (describeTable t')

list' :: (MonadCatch m, MonadAWS m)
      => TableName
      -> m [(Name, NonEmpty Version)]
list' t =
    paginate (scan (toText t) & sAttributesToGet ?~ fields) $$ result
  where
    result = CL.concatMapM (traverse fromVal . view srsItems)
         =$= CL.groupOn1 fst
         =$= CL.map group
         =$= CL.consume

    fields = nameField :| [versionField]

    group ((k, v), map snd -> vs) = (k, desc (v :| vs))

    desc = NE.sortOn Down

insert' :: (MonadIO m, MonadMask m, MonadAWS m, Typeable m)
        => Name
        -> Secret
        -> TableName
        -> m Version
insert' n s t = recovering policy [const cond] write
  where
    write = do
        v <- maybe 1 (+1) <$> latest n t
        let x = toVal v
        void . send $ putItem (toText t)
            & piItem     .~ toVal n <> x <> toVal s
            & piExpected .~ Map.map (const expect) x
        return v

    cond = handler_ _ConditionalCheckFailedException (return True)

    expect = expectedAttributeValue & eavExists ?~ False

    policy = constantDelay 1000 <> limitRetries 5

select' :: (MonadThrow m, MonadAWS m)
        => Name
        -> Maybe Version
        -> TableName
        -> m (Secret, Version)
select' n v t = send (named n t & version v) >>= result
  where
    result  = maybe missing fromVal . listToMaybe . view qrsItems
    missing = throwM $ SecretMissing n (toText t)

    version Nothing  = id
    version (Just x) = qKeyConditions <>~ equals x

delete' :: MonadAWS m
        => Name
        -> Version
        -> TableName
        -> m ()
delete' n v t = void . send $
    deleteItem (toText t) & diKey .~ toVal n <> toVal v

latest :: (MonadThrow m, MonadAWS m)
       => Name
       -> TableName
       -> m (Maybe Version)
latest n t = do
    rs <- send (named n t)
    case listToMaybe (rs ^. qrsItems) of
        Nothing -> pure Nothing
        Just  m -> Just <$> fromVal m

exists :: MonadAWS m => TableName -> m Bool
exists t = paginate listTables
    =$= CL.concatMap (view ltrsTableNames)
     $$ (isJust <$> findC (== toText t))

named :: Name -> TableName -> Query
named n t = query (toText t)
    & qLimit            ?~ 1
    & qScanIndexForward ?~ False
    & qConsistentRead   ?~ True
    & qKeyConditions    .~ equals n

findC :: Monad m => (a -> Bool) -> Consumer a m (Maybe a)
findC f = loop
  where
    loop = C.await >>= maybe (return Nothing) go

    go x | f x       = return (Just x)
         | otherwise = loop
