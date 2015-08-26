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
import           Control.Lens                 (view, ( # ), (&), (.~), (<&>))
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Credentials                  as Store hiding (context)
import           Credentials.CLI.IO
import           Credentials.CLI.Types
import           Data.Bifunctor
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString.Builder      as Build
import qualified Data.ByteString.Char8        as BS8
import           Data.Char
import           Data.Conduit                 (($$))
import qualified Data.Conduit.List            as CL
import           Data.Data
import           Data.HashMap.Strict          (HashMap)
import           Data.List                    (foldl', sort)
import           Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.List.NonEmpty           as NE
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import           Network.AWS
import           Network.AWS.Data
import           Network.AWS.Data.Text
import           Network.AWS.S3               (BucketName, ObjectVersionId)
import           Numeric.Natural
import           Options.Applicative          hiding (optional)
import qualified Options.Applicative          as Opt
import           System.Exit
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen (Doc, bold, hardline, indent,
                                               (<+>), (</>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

-- | Setup a regular required option with formatted help text.
regular :: Doc       -- ^ The options' description.
        -> Maybe Doc -- ^ The help body (title/footer in tabular).
        -> Fact
        -> Mod OptionFields a
regular title body r = helpDoc . Just $ title <> doc <> hardline
  where
    doc | Just b <- body = pad (maybe b (b PP.<$>) foot)
        | otherwise      = maybe mempty pad foot

    foot = case r of
        Required -> Just ("This is" <+> bold "required.")
        Optional -> Just ("This is" <+> bold "optional.")
        Default  -> Nothing

    pad = mappend hardline . indent 2

-- | Setup a tabular list of possible values for an option,
-- a default value, and an autocompleter.
tabular :: ToText a
        => Doc        -- ^ The options' description.
        -> Doc        -- ^ A title for the values.
        -> [(a, Doc)] -- ^ Possible values and their documentation.
        -> a          -- ^ A default value.
        -> Maybe Doc  -- ^ Footer contents.
        -> Mod OptionFields a
tabular title note xs x foot = body <> value x <> completeWith (map fst ys)
  where
    body = regular title (Just (defaults note ys def foot)) Default

    def = string x
    ys  = map (first string) xs

-- | Construct a tabular representation displaying the default values,
-- without using ToText.
defaults :: Doc
         -> [(String, Doc)]
         -> String
         -> Maybe Doc
         -> Doc
defaults note xs def foot = maybe id (flip (PP.<$>)) foot body
  where
    body = note
        PP.<$> indent 2 rows
        PP.<$> ("Defaults to " <> bold (PP.text def) <> ".")

    rows | [r]  <- xs = sep r
         | r:rs <- xs = foldl' (PP.<$>) (sep r) (map sep rs)
         | otherwise  = mempty
      where
        sep (k, v) = "-" <+> bold (PP.text k) <+> indent (len - length k) v

    len = maximum (map (length . fst) xs)

require :: Functor f => (Fact -> f a) -> f a
require f = f Required

optional :: Alternative f => (Fact -> f a) -> f (Maybe a)
optional f = Opt.optional (f Optional)

text :: FromText a => ReadM a
text = eitherReader (fromText . Text.pack)
