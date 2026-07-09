{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Family.Type.Associated.Default
  ( AssociatedTypeDefault
  , mkAssociatedTypeDefault
  ) where

import HIndent.Ast.Name.Prefix
import HIndent.Ast.Type
import HIndent.Ast.Type.Argument.Collection
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data AssociatedTypeDefault = AssociatedTypeDefault
  { name :: WithComments PrefixName
  , types :: TypeArgumentCollection
  , bind :: WithComments Type
  }

instance Pretty AssociatedTypeDefault where
  pretty AssociatedTypeDefault {..} = spaced [lhs, string "=", pretty bind]
    where
      lhs =
        spaced
          $ [string "type instance", pretty name]
              <> [pretty types | hasTypeArguments types]

mkAssociatedTypeDefault :: GHC.TyFamInstDecl GHC.GhcPs -> AssociatedTypeDefault
mkAssociatedTypeDefault GHC.TyFamInstDecl {GHC.tfid_eqn = GHC.FamEqn {..}} =
  AssociatedTypeDefault
    { name = mkWithCommentsFromGenLocated $ fmap mkPrefixName feqn_tycon
    , types = mkTypeArgumentCollection feqn_pats
    , bind = mkType <$> mkWithCommentsFromGenLocated feqn_rhs
    }
