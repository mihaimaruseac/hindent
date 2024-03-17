module HIndent.Ast.Declaration.Signature.BooleanFormula
  ( BooleanFormula
  , mkBooleanFormula
  ) where

import qualified GHC.Data.BooleanFormula as GHC
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data BooleanFormula
  = Var (GHC.LIdP GHC.GhcPs)
  | And [WithComments BooleanFormula]
  | Or [WithComments BooleanFormula]
  | Parens (WithComments BooleanFormula)

instance CommentExtraction BooleanFormula where
  nodeComments Var {} = NodeComments [] [] []
  nodeComments And {} = NodeComments [] [] []
  nodeComments Or {} = NodeComments [] [] []
  nodeComments Parens {} = NodeComments [] [] []

instance Pretty BooleanFormula where
  pretty' (Var x) = pretty x
  pretty' (And xs) = hvCommaSep $ fmap pretty xs
  pretty' (Or xs) = hvBarSep $ fmap pretty xs
  pretty' (Parens x) = parens $ pretty x

mkBooleanFormula :: GHC.BooleanFormula (GHC.LIdP GHC.GhcPs) -> BooleanFormula
mkBooleanFormula (GHC.Var x) = Var x
mkBooleanFormula (GHC.And xs) =
  And $ fmap (fmap mkBooleanFormula . fromGenLocated) xs
mkBooleanFormula (GHC.Or xs) =
  Or $ fmap (fmap mkBooleanFormula . fromGenLocated) xs
mkBooleanFormula (GHC.Parens x) = Parens $ mkBooleanFormula <$> fromGenLocated x
