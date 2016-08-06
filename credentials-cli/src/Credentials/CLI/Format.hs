{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- Module      : Credentials.CLI.Format
-- Copyright   : (c) 2015-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Credentials.CLI.Format where

import Control.Monad.Trans.Resource

import Credentials
import Credentials.CLI.Types

import Data.Aeson         (ToJSON (..), object, (.=))
import Data.Bifunctor
import Data.ByteString    (ByteString)
import Data.Conduit
import Data.List          (foldl', intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid

import Network.AWS.Data

import Options.Applicative.Help hiding (string)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text

data Status
    = Deleted
    | Truncated

instance ToLog Status where
    build = build . toText

instance ToText Status where
    toText = \case
        Deleted   -> "deleted"
        Truncated -> "truncated"

data Emit = Emit { store' :: Store, result :: Result }

instance ToJSON Emit where
    toJSON (Emit s r) = object [toText s .= r]

instance Pretty Emit where
    pretty (Emit s r) = doc s <> char ':' .$. indent 2 (pretty r)

data Result
    = SetupR    Setup
    | CleanupR
    | PutR      Name Revision
    | GetR      Name Revision LBS.ByteString
    | DeleteR   Name Revision
    | TruncateR Name
    | ListR     [(Name, NonEmpty Revision)]

instance ToLog Result where
    build = \case
        SetupR        s -> build s
        CleanupR        -> build Deleted
        PutR      _ r   -> build r
        GetR      _ _ v -> build v
        DeleteR   {}    -> build Deleted
        TruncateR {}    -> build Truncated
        ListR        rs -> foldMap f rs
          where
            f (n, v :| vs) =
                build n % "," % mconcat (intersperse "," $ map build (v:vs)) % "\n"

instance ToJSON Result where
    toJSON = \case
        SetupR        s -> object ["status" =~ s]
        CleanupR        -> object ["status" =~ Deleted]
        PutR      n r   -> object ["name"   =~ n, "revision" =~ r]
        GetR      n r v -> object ["name"   =~ n, "revision" =~ r, "secret" =~ toBS v]
        DeleteR   n r   -> object ["name"   =~ n, "revision" =~ r, "status" =~ Deleted]
        TruncateR n     -> object ["name"   =~ n, "status"   =~ Truncated]
        ListR        rs -> object (map go rs)
      where
        k =~ v = k .= toText v

        go (n, v :| vs) = toText n .= map toText (v:vs)

instance Pretty Result where
    pretty = \case
        SetupR        s -> stat s
        CleanupR        -> stat Deleted
        PutR      n r   -> name n .$. rev r
        GetR      n r v -> name n .$. rev r .$. val v
        DeleteR   n r   -> name n .$. rev r .$. stat Deleted
        TruncateR n     -> name n .$. stat Truncated
        ListR        rs -> list rs
      where
        name n = "name:"     <+> doc n
        rev  r = "revision:" <+> doc r
        stat s = "status:"   <+> doc s
        val  v = "secret:"   <+> doc (toBS v)

        list []     = mempty
        list (r:rs) = foldl' (.$.) (f r) (map f rs)
          where
            f (n, v :| vs) = doc n <> ":" .$.
                indent 2 (extractChunk (revs v vs))

            revs v vs = table $ (v, "# latest") : map ((,mempty)) vs

        table [] = mempty
        table xs = pure $ vcat
            [indent 2 (fillBreak n (item k) <+> v) | (k, v) <- ys]
          where
            n  = maximum (map (Text.length . fst) ys) + 2
            ys = map (first toText) xs

        item x = "-" <+> doc x

doc :: ToText a => a -> Doc
doc = text . string
