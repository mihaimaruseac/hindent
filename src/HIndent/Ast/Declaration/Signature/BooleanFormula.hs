{-# LANGUAGE CPP #-}

module HIndent.Ast.Declaration.Signature.BooleanFormula
  ( BooleanFormula
  , mkBooleanFormula
  ) where

import qualified GHC.Data.BooleanFormula as BF
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data BooleanFormula
  = Var (WithComments PrefixName)
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
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkBooleanFormula :: BF.BooleanFormula GHC.GhcPs -> BooleanFormula
mkBooleanFormula (BF.Var x) = Var $ fromGenLocated $ fmap mkPrefixName x
mkBooleanFormula (BF.And xs) = And $ fmap mkBooleanFormulaWithComments xs
mkBooleanFormula (BF.Or xs) = Or $ fmap mkBooleanFormulaWithComments xs
mkBooleanFormula (BF.Parens x) = Parens $ mkBooleanFormulaWithComments x

mkBooleanFormulaWithComments ::
     BF.LBooleanFormula GHC.GhcPs -> WithComments BooleanFormula
mkBooleanFormulaWithComments = fmap mkBooleanFormula . fromGenLocated
#else
mkBooleanFormula :: BF.BooleanFormula (GHC.LIdP GHC.GhcPs) -> BooleanFormula
mkBooleanFormula (BF.Var x) = Var $ fromGenLocated $ fmap mkPrefixName x
mkBooleanFormula (BF.And xs) = And $ fmap mkBooleanFormulaWithComments xs
mkBooleanFormula (BF.Or xs) = Or $ fmap mkBooleanFormulaWithComments xs
mkBooleanFormula (BF.Parens x) = Parens $ mkBooleanFormulaWithComments x

mkBooleanFormulaWithComments ::
     BF.LBooleanFormula (GHC.LIdP GHC.GhcPs) -> WithComments BooleanFormula
mkBooleanFormulaWithComments = fmap mkBooleanFormula . fromGenLocated
#endif
