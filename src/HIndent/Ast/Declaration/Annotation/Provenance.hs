{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.Declaration.Annotation.Provenance
  ( Provenance
  , mkProvenance
  ) where

import HIndent.Ast.Name.Prefix
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data Provenance
  = Value (WithComments PrefixName)
  | Type (WithComments PrefixName)
  | Module

instance Pretty Provenance where
  pretty (Value x) = pretty x
  pretty (Type x) = string "type " >> pretty x
  pretty Module = string "module"

mkProvenance :: GHC.AnnProvenance GHC.GhcPs -> Provenance
mkProvenance (GHC.ValueAnnProvenance x) =
  Value $ mkWithCommentsFromGenLocated $ fmap mkPrefixName x
mkProvenance (GHC.TypeAnnProvenance x) =
  Type $ mkWithCommentsFromGenLocated $ fmap mkPrefixName x
mkProvenance GHC.ModuleAnnProvenance = Module
