{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module HIndent.Ast.Declaration.Instance.Family.Type.Associated.Default
  ( AssociatedTypeDefault
  , mkAssociatedTypeDefault
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype AssociatedTypeDefault =
  AssociatedTypeDefault
    (GHC.FamEqn GHC.GhcPs (GHC.LocatedA (GHC.HsType GHC.GhcPs)))

instance CommentExtraction AssociatedTypeDefault where
  nodeComments (AssociatedTypeDefault _) = NodeComments [] [] []

instance Pretty AssociatedTypeDefault where
  pretty' (AssociatedTypeDefault equation) =
    string "type instance " >> pretty equation

mkAssociatedTypeDefault :: GHC.TyFamInstDecl GHC.GhcPs -> AssociatedTypeDefault
mkAssociatedTypeDefault GHC.TyFamInstDecl {..} = AssociatedTypeDefault tfid_eqn
