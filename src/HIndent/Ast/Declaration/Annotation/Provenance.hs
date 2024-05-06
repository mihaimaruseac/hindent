module HIndent.Ast.Declaration.Annotation.Provenance
  ( Provenance
  , mkProvenance
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Provenance
  = Value (GHC.LIdP GHC.GhcPs)
  | Type (GHC.LIdP GHC.GhcPs)
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
mkProvenance (GHC.ValueAnnProvenance x) = Value x
mkProvenance (GHC.TypeAnnProvenance x) = Type x
mkProvenance GHC.ModuleAnnProvenance = Module
