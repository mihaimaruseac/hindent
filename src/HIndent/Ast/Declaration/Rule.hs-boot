module HIndent.Ast.Declaration.Rule
  ( RuleDeclaration
  , mkRuleDeclaration
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments
import {-# SOURCE #-} HIndent.Pretty

data RuleDeclaration

instance CommentExtraction RuleDeclaration
instance Pretty RuleDeclaration

mkRuleDeclaration :: GHC.RuleDecl GHC.GhcPs -> RuleDeclaration
