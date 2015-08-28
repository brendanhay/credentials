{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Credentials.CLI.Options
-- Copyright   : (c) 2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.CLI.Options where

import           Control.Exception.Lens
import           Control.Lens             (view, ( # ), (&), (.~), (<&>))
import           Control.Monad
import           Control.Monad.Catch
import           Credentials              as Store hiding (context)
import           Credentials.CLI.IO
import           Credentials.CLI.Types
import           Data.Bifunctor
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.ByteString.Builder  (Builder)
import qualified Data.ByteString.Builder  as Build
import qualified Data.ByteString.Char8    as BS8
import           Data.Char
import           Data.Conduit             (($$))
import qualified Data.Conduit.List        as CL
import           Data.Data
import           Data.HashMap.Strict      (HashMap)
import           Data.List                (foldl', sort)
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Data.Tuple               (swap)
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.S3           (BucketName, ObjectVersionId)
import           Numeric.Natural
import           Options.Applicative      hiding (optional)
import qualified Options.Applicative      as Opt
import           Options.Applicative.Help hiding (string)
import           System.Exit
import           System.IO

-- | Setup an option with formatted help text.
describe :: Text    -- ^ The options' description.
         -> Maybe Doc -- ^ The help body (title/footer in tabular).
         -> Fact
         -> Mod OptionFields a
describe title body r = helpDoc . Just $ wrap title <> doc <> line
  where
    doc | Just b <- body = pad (maybe b (b .$.) foot)
        | otherwise      = maybe mempty pad foot

    foot = case r of
        Required -> Just ("This is" <+> bold "required.")
        Optional -> Just ("This is" <+> bold "optional.")
        Default  -> Nothing

    pad = mappend line . indent 2

-- | Setup a tabular list of possible values for an option,
-- a default value, and an auto-completer.
completes :: ToText a
          => Text          -- ^ The options' description.
          -> Text          -- ^ A title for the values.
          -> [(a, String)] -- ^ Possible values and their documentation.
          -> a             -- ^ A default value.
          -> Maybe Text    -- ^ Footer contents.
          -> Mod OptionFields a
completes title note xs x foot = doc <> completeWith (map fst ys)
  where
    doc = defaults title note ys x foot
    ys  = map (first string) xs

-- | Construct a tabular representation displaying the default values,
-- without using ToText for the tabular values.
defaults :: ToText a
         => Text
         -> Text
         -> [(String, String)]
         -> a
         -> Maybe Text
         -> Mod OptionFields a
defaults title note xs x foot = describe title (Just doc) Default <> value x
  where
    doc   = maybe table (table .$.) (wrap <$> foot)
    table = wrap note
        .$. indent 2 rows
        .$. ("Defaults to " <> bold (text (string x)) <> ".")

    len = maximum (map (length . fst) xs)

    rows | [r]  <- xs = sep r
         | r:rs <- xs = foldl' (.$.) (sep r) (map sep rs)
         | otherwise  = mempty
      where
        sep (k, v) = "-"
            <+> bold (text k)
            <+> indent (len - length k) ts
          where
            ts | null v    = mempty
               | otherwise = tupled [text v]

require :: Functor f => (Fact -> f a) -> f a
require f = f Required

optional :: Alternative f => (Fact -> f a) -> f (Maybe a)
optional f = Opt.optional (f Optional)

textOption :: FromText a => Mod OptionFields a -> Parser a
textOption = option (eitherReader (fromText . Text.pack))

wrap :: Text -> Doc
wrap = extractChunk . paragraph . Text.unpack
