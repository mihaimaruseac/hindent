{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Rule
  ( RuleDeclaration
  , mkRuleDeclaration
  ) where

import qualified GHC.Core as GHC
import qualified GHC.Data.FastString as GHC
import HIndent.Ast.Declaration.Rule.Binder
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data RuleDeclaration = RuleDeclaration
  { name :: WithComments GHC.RuleName
  , binders :: [WithComments RuleBinder]
  , lhs :: WithComments (GHC.HsExpr GHC.GhcPs)
  , rhs :: WithComments (GHC.HsExpr GHC.GhcPs)
  }

instance CommentExtraction RuleDeclaration where
  nodeComments RuleDeclaration {} = NodeComments [] [] []

instance Pretty RuleDeclaration where
  pretty' (RuleDeclaration {..}) =
    spaced
      [ prettyWith name (doubleQuotes . string . GHC.unpackFS)
      , prettyLhs
      , string "="
      , pretty rhs
      ]
    where
      prettyLhs =
        if null binders
          then pretty lhs
          else do
            string "forall "
            spaced $ fmap pretty binders
            dot
            space
            pretty lhs

mkRuleDeclaration :: GHC.RuleDecl GHC.GhcPs -> RuleDeclaration
mkRuleDeclaration rule@GHC.HsRule {..} = RuleDeclaration {..}
  where
    name = getName rule
    binders = fmap (fmap mkRuleBinder . fromGenLocated) rd_tmvs
    lhs = fromGenLocated rd_lhs
    rhs = fromGenLocated rd_rhs

getName :: GHC.RuleDecl GHC.GhcPs -> WithComments GHC.RuleName
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
getName = fromGenLocated . GHC.rd_name
#else
getName = fromGenLocated . fmap snd . GHC.rd_name
#endif
