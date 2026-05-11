{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Family.Type.Associated
  ( AssociatedType
  , mkAssociatedType
  ) where

import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.Type
import HIndent.Ast.Type.Argument.Collection
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data AssociatedType = AssociatedType
  { name :: WithComments PrefixName
  , types :: TypeArgumentCollection
  , bind :: WithComments Type
  }

instance CommentExtraction AssociatedType where
  nodeComments AssociatedType {} = NodeComments [] [] []

instance Pretty AssociatedType where
  pretty' AssociatedType {..} = spaced [lhs, string "=", pretty bind]
    where
      lhs =
        spaced
          $ [string "type", pretty name]
              <> [pretty types | hasTypeArguments types]

mkAssociatedType :: GHC.TyFamInstDecl GHC.GhcPs -> AssociatedType
mkAssociatedType GHC.TyFamInstDecl {GHC.tfid_eqn = GHC.FamEqn {..}} =
  AssociatedType
    { name = fromGenLocated $ fmap mkPrefixName feqn_tycon
    , types = mkTypeArgumentCollection feqn_pats
    , bind = mkType <$> fromGenLocated feqn_rhs
    }
