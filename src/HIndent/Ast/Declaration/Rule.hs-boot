module HIndent.Ast.Declaration.Rule
  ( RuleDeclaration
  , mkRuleDeclaration
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty

data RuleDeclaration

instance Pretty RuleDeclaration

mkRuleDeclaration :: GHC.RuleDecl GHC.GhcPs -> RuleDeclaration
