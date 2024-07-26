module HIndent.Ast.Declaration.Annotation.Provenance
  ( Provenance
  , mkProvenance
  ) where

import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Provenance
  = Value (WithComments PrefixName)
  | Type (WithComments PrefixName)
  | Module

instance CommentExtraction Provenance where
  nodeComments Value {} = NodeComments [] [] []
  nodeComments Type {} = NodeComments [] [] []
  nodeComments Module = NodeComments [] [] []

instance Pretty Provenance where
  pretty' (Value x) = pretty x
  pretty' (Type x) = string "type " >> pretty x
  pretty' Module = string "module"

mkProvenance :: GHC.AnnProvenance GHC.GhcPs -> Provenance
mkProvenance (GHC.ValueAnnProvenance x) =
  Value $ fromGenLocated $ fmap mkPrefixName x
mkProvenance (GHC.TypeAnnProvenance x) =
  Type $ fromGenLocated $ fmap mkPrefixName x
mkProvenance GHC.ModuleAnnProvenance = Module
