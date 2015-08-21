{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

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
import           Control.Monad.Reader      (ask)
import           Data.Bifunctor
import           Data.ByteArray.Encoding
import           Data.ByteString           (ByteString)
import qualified Data.Conduit.List         as CL
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as Map
import           Data.List                 (sortBy)
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NE
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text                 as Text
import           Data.Typeable
import           Network.AWS
import           Network.AWS               (runAWS)
import           Network.AWS               (AWS, liftAWS)
import           Network.AWS               (Region)
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.DynamoDB
import           Network.Credentials.Store (Ref, Storage)
import qualified Network.Credentials.Store as Store
import           Network.Credentials.Types
import           Numeric.Natural

fieldName, fieldVersion, fieldKey, fieldContents, fieldHMAC :: Text
fieldName     = "name"
fieldVersion  = "version"
fieldKey      = "key"
fieldContents = "contents"
fieldHMAC     = "hmac"

newtype Dynamo a = Dynamo { runDynamo :: AWS a }
    deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch)

instance MonadAWS Dynamo where
    liftAWS = Dynamo

instance Storage Dynamo where
    type Engine Dynamo = AWS
    newtype Ref Dynamo = Table Text

    engine  = runDynamo
    setup   = setup
    cleanup = cleanup
    list    = list
    insert  = put

type Table = Ref Dynamo

deriving instance Eq       Table
deriving instance Show     Table
deriving instance FromText Table
deriving instance ToText   Table
deriving instance ToLog    Table

defaultTable :: Table
defaultTable = Table "credential-store"

setup :: MonadAWS m => Table -> m Setup
setup t@(toText -> t') = do
    p <- exists t

    unless p $ do
        let keys = keySchemaElement fieldName    Hash
               :| [keySchemaElement fieldVersion Range]
            iops = provisionedThroughput 1 1
            attr = ctAttributeDefinitions .~
                [ attributeDefinition fieldName S
                , attributeDefinition fieldVersion N
                ]
        void $ send (createTable t' keys iops & attr)
        void $ await tableExists (describeTable t')

    return $
        if p
            then Exists
            else Created

cleanup :: MonadAWS m => Table -> m ()
cleanup t@(toText -> t') = do
    p <- exists t
    when p $ do
        void $ send (deleteTable t')
        void $ await tableNotExists (describeTable t')

list :: (MonadCatch m, MonadAWS m)
     => Table
     -> m [(Name, NonEmpty Version)]
list t =
    catching_ _ResourceNotFoundException rq $ throwM (NotSetup (toText t))
  where
    rq = paginate (scan (toText t) & sAttributesToGet ?~ fieldName :| [fieldVersion])
        =$= CL.concatMapM (traverse parse . view srsItems)
        =$= CL.groupOn1 fst
        =$= CL.map group
         $$ CL.consume

    parse m = (,)
        <$> fromAttrs fieldName    avS m
        <*> fromAttrs fieldVersion avN m

    group ((k, v), map snd -> vs) = (k, desc (v :| vs))

    desc = NE.sortOn Down

put :: (MonadThrow m, MonadAWS m)
     => Name
     -> Secret
     -> Table
     -> m Version
put n (Secret k c h) t@(toText -> t') = do
    v <- maybe 1 (+1) <$> latest n t

    void . send $ putItem t' & piItem .~ Map.fromList
        [ (fieldName,     toAttr avS toText n)
        , (fieldVersion,  toAttr avN toText v)
        , (fieldKey,      toAttr avB toBS   k)
        , (fieldContents, toAttr avB toBS   c)
        , (fieldHMAC,     toAttr avS toText h)
        ]

    return v

latest :: (MonadThrow m, MonadAWS m)
       => Name
       -> Table
       -> m (Maybe Version)
latest n t = do
    rs <- send $ query (toText t)
        & qLimit            ?~ 1
        & qScanIndexForward ?~ False
        & qConsistentRead   ?~ True
        & qKeyConditions    .~ Map.fromList
            [ ( fieldName, condition EQ' & cAttributeValueList .~
                  [attributeValue & avS ?~ toText n]
              )
            ]

    case rs ^. qrsItems of
        []  -> return Nothing
        m:_ -> Just <$> fromAttrs fieldVersion avN m

exists :: MonadAWS m => Table -> m Bool
exists t = paginate listTables
    =$= concatMapC (view ltrsTableNames)
     $$ (isJust <$> findC (== toText t))

toAttr :: Setter' AttributeValue (Maybe b)
       -> (a -> b)
       -> a
       -> AttributeValue
toAttr l f v = attributeValue & l ?~ f v

fromAttrs :: (MonadThrow m, FromText a)
          => Text
          -> Getter AttributeValue (Maybe Text)
          -> HashMap Text AttributeValue
          -> m a
fromAttrs k l m = require >>= parse
  where
    parse x = maybe missing (either invalid pure . fromText) (x ^. l)
    require = maybe missing pure (Map.lookup k m)

    missing :: MonadThrow m => m a
    missing = throwM (Missing k)

    invalid :: MonadThrow m => String -> m a
    invalid e = throwM (Invalid k e)
