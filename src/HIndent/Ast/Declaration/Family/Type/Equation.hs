{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family.Type.Equation
  ( TypeEquation
  , mkTypeEquation
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

data TypeEquation = TypeEquation
  { name :: WithComments PrefixName
  , types :: TypeArgumentCollection
  , bind :: WithComments Type
  }

instance CommentExtraction TypeEquation where
  nodeComments TypeEquation {} = NodeComments [] [] []

instance Pretty TypeEquation where
  pretty' TypeEquation {..} = spaced [lhs, string "=", pretty bind]
    where
      lhs = spaced $ [pretty name] <> [pretty types | hasTypeArguments types]

mkTypeEquation :: GHC.TyFamInstEqn GHC.GhcPs -> TypeEquation
mkTypeEquation GHC.FamEqn {..} = TypeEquation {..}
  where
    name = mkWithCommentsFromGenLocated $ fmap mkPrefixName feqn_tycon
    types = mkTypeArgumentCollection feqn_pats
    bind = mkType <$> mkWithCommentsFromGenLocated feqn_rhs
