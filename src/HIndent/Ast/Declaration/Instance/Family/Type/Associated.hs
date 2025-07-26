{-# LANGUAGE RecordWildCards, CPP #-}

module HIndent.Ast.Declaration.Instance.Family.Type.Associated
  ( AssociatedType
  , mkAssociatedType
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype AssociatedType =
  AssociatedType (GHC.FamEqn GHC.GhcPs (GHC.LocatedA (GHC.HsType GHC.GhcPs)))

instance CommentExtraction AssociatedType where
  nodeComments (AssociatedType _) = NodeComments [] [] []

instance Pretty AssociatedType where
  pretty' (AssociatedType equation) = string "type " >> pretty equation

mkAssociatedType :: GHC.TyFamInstDecl GHC.GhcPs -> AssociatedType
mkAssociatedType GHC.TyFamInstDecl {..} = AssociatedType tfid_eqn
