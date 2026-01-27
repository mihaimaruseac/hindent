{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Rule
  ( RuleDeclaration
  , mkRuleDeclaration
  ) where

import qualified GHC.Types.Basic as GHC
import HIndent.Ast.Declaration.Rule.Binder
import HIndent.Ast.Declaration.Rule.Name
import HIndent.Ast.Expression (Expression, mkExpression)
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data RuleDeclaration = RuleDeclaration
  { name :: WithComments RuleName
  , binders :: [WithComments RuleBinder]
  , lhs :: WithComments Expression
  , rhs :: WithComments Expression
  }

instance CommentExtraction RuleDeclaration where
  nodeComments RuleDeclaration {} = NodeComments [] [] []

instance Pretty RuleDeclaration where
  pretty' (RuleDeclaration {..}) =
    spaced [pretty name, prettyLhs, string "=", pretty rhs]
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
    name = mkRuleName <$> getName rule
    binders = mkRuleBinders rule
    lhs = mkExpression <$> fromGenLocated rd_lhs
    rhs = mkExpression <$> fromGenLocated rd_rhs

mkRuleBinders :: GHC.RuleDecl GHC.GhcPs -> [WithComments RuleBinder]
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkRuleBinders GHC.HsRule {rd_bndrs = GHC.RuleBndrs {..}} =
  fmap (fmap mkRuleBinder . fromGenLocated) rb_tmvs
#else
mkRuleBinders GHC.HsRule {..} =
  fmap (fmap mkRuleBinder . fromGenLocated) rd_tmvs
#endif
getName :: GHC.RuleDecl GHC.GhcPs -> WithComments GHC.RuleName
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
getName = fromGenLocated . GHC.rd_name
#else
getName = fromGenLocated . fmap snd . GHC.rd_name
#endif
