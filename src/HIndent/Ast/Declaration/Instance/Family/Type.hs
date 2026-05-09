{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Family.Type
  ( TypeFamilyInstance
  , mkTypeFamilyInstance
  ) where

import HIndent.Ast.Name.Prefix
import HIndent.Ast.Type
import HIndent.Ast.Type.Argument.Collection
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data TypeFamilyInstance = TypeFamilyInstance
  { name :: WithComments PrefixName
  , types :: TypeArgumentCollection
  , bind :: WithComments Type
  }

instance Pretty TypeFamilyInstance where
  pretty TypeFamilyInstance {..} = spaced [lhs, string "=", pretty bind]
    where
      lhs =
        spaced
          $ [string "type instance", pretty name]
              <> [pretty types | hasTypeArguments types]

mkTypeFamilyInstance :: GHC.InstDecl GHC.GhcPs -> Maybe TypeFamilyInstance
mkTypeFamilyInstance GHC.TyFamInstD {GHC.tfid_inst = GHC.TyFamInstDecl {GHC.tfid_eqn = GHC.FamEqn {..}}} =
  Just $ TypeFamilyInstance {..}
  where
    name = mkWithCommentsFromGenLocated $ fmap mkPrefixName feqn_tycon
    types = mkTypeArgumentCollection feqn_pats
    bind = mkType <$> mkWithCommentsFromGenLocated feqn_rhs
mkTypeFamilyInstance _ = Nothing
