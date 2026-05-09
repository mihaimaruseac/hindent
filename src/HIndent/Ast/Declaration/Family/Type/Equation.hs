{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family.Type.Equation
  ( TypeEquation
  , mkTypeEquation
  ) where

import HIndent.Ast.Name.Prefix
import HIndent.Ast.Type
import HIndent.Ast.Type.Argument.Collection
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data TypeEquation = TypeEquation
  { name :: WithComments PrefixName
  , types :: TypeArgumentCollection
  , bind :: WithComments Type
  }

instance Pretty TypeEquation where
  pretty TypeEquation {..} = spaced [lhs, string "=", pretty bind]
    where
      lhs = spaced $ [pretty name] <> [pretty types | hasTypeArguments types]

mkTypeEquation :: GHC.TyFamInstEqn GHC.GhcPs -> TypeEquation
mkTypeEquation GHC.FamEqn {..} =
  TypeEquation
    { name = mkWithCommentsFromGenLocated $ fmap mkPrefixName feqn_tycon
    , types = mkTypeArgumentCollection feqn_pats
    , bind = mkType <$> mkWithCommentsFromGenLocated feqn_rhs
    }
