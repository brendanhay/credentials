{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
import           GHC.Exts
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

newtype Dynamo a = Dynamo { runDynamo :: AWS a }
    deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch)

instance MonadAWS Dynamo where
    liftAWS = Dynamo

instance Storage Dynamo where
    type Layer Dynamo = AWS
    type Ref   Dynamo = Table

    layer   = runDynamo
    setup   = setup
    cleanup = cleanup
    list    = list
    insert  = put
    select  = get

setup :: MonadAWS m => Table -> m Setup
setup t@(toText -> t') = do
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
list t = paginate (scan (toText t) & sAttributesToGet ?~ fields) $$ result
  where
    result = CL.concatMapM (traverse fromVal . view srsItems)
         =$= CL.groupOn1 fst
         =$= CL.map group
         =$= CL.consume

    fields = nameField :| [versionField]

    group ((k, v), map snd -> vs) = (k, desc (v :| vs))

    desc = NE.sortOn Down

put :: (MonadThrow m, MonadAWS m)
     => Name
     -> Version
     -> Secret
     -> Table
     -> m ()
put n v s t = void . send $ putItem (toText t)
    & piItem     .~ toVal n <> toVal v <> toVal s
    & piExpected .~ Map.map (const new) (toVal v)
  where
    new = expectedAttributeValue
        & eavExists             ?~ False
        & eavComparisonOperator ?~ Null

get :: (MonadThrow m, MonadAWS m)
    => Name
    -> Maybe Version
    -> Table
    -> m Secret
get n v t = send (named n t & version v) >>= result
  where
    result  = maybe missing fromVal . listToMaybe . view qrsItems
    missing = throwM $ SecretMissing n (toText t)

    version Nothing  = id
    version (Just x) = qKeyConditions <>~ equals x

latest :: (MonadThrow m, MonadAWS m)
       => Name
       -> Table
       -> m (Maybe Version)
latest n t = do
    rs <- send (named n t)
    case listToMaybe (rs ^. qrsItems) of
        Nothing -> pure Nothing
        Just  m -> Just <$> fromVal m

exists :: MonadAWS m => Table -> m Bool
exists t = paginate listTables
    =$= concatMapC (view ltrsTableNames)
     $$ (isJust <$> findC (== toText t))

named :: Name -> Table -> Query
named n t = query (toText t)
    & qLimit            ?~ 1
    & qScanIndexForward ?~ False
    & qConsistentRead   ?~ True
    & qKeyConditions    .~ equals n

equals :: Val a => a -> HashMap Text Condition
equals = Map.map (\x -> condition EQ' & cAttributeValueList .~ [x]) . toVal

nameField, versionField :: Text
nameField    = name (Proxy :: Proxy Name)
versionField = name (Proxy :: Proxy Version)

class Val a where
    toVal   :: a -> HashMap Text AttributeValue
    fromVal :: MonadThrow m => HashMap Text AttributeValue -> m a

    default toVal :: IsField a b => a -> HashMap Text AttributeValue
    toVal = toField

    default fromVal :: (MonadThrow m, Monoid b, ToText b, IsField a b)
                    => HashMap Text AttributeValue
                    -> m a
    fromVal = fromField

instance (Val a, Val b) => Val (a, b) where
    toVal (x, y) = toVal x <> toVal y
    fromVal m    = (,) <$> fromVal m <*> fromVal m

instance Val Secret where
    toVal (Secret k h c) = toVal k <> toVal h <> toVal c
    fromVal m = Secret <$> fromVal m <*> fromVal m <*> fromVal m

instance Val Name
instance Val Version
instance Val Key
instance Val Cipher
instance Val HMAC256

class IsField a b | a -> b where
    field :: Field a b

instance IsField Name    Text       where field = Field "name"     avS text
instance IsField Version Text       where field = Field "version"  avN text
instance IsField Key     ByteString where field = Field "key"      avB (bytes Key)
instance IsField Cipher  ByteString where field = Field "contents" avB (bytes Cipher)
instance IsField HMAC256 ByteString where field = Field "hmac"     avB (bytes Hex)

data Field a b = Field Text (Lens' AttributeValue (Maybe b)) (Prism' b a)

toField :: IsField a b => a -> HashMap Text AttributeValue
toField x = let Field k l p = field in [(k, attributeValue & l ?~ review p x)]

fromField :: (MonadThrow m, Monoid b, ToText b, IsField a b)
          => HashMap Text AttributeValue
          -> m a
fromField m = require >>= parse
  where
    Field k l p = field

    require = maybe missing pure (Map.lookup k m)

    parse (attr -> x) = maybe (invalid x) pure (preview p x)

    attr = fromMaybe mempty . view l

    missing = throwM (FieldMissing k (Map.keys m))
    invalid = throwM . FieldInvalid k . toText

name :: forall a b. IsField a b => Proxy a -> Text
name _ = let (Field k _ _) = field :: Field a b in k

text :: (FromText a, ToText a) => Prism' Text a
text = prism' toText go
  where
    go x | Text.null x = Nothing
         | otherwise   = either (const Nothing) Just (fromText x)

bytes :: ToByteString a => (ByteString -> a) -> Prism' ByteString a
bytes w = prism' toBS (Just . w)
