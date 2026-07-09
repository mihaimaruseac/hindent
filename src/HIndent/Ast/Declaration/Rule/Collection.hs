{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Rule.Collection
  ( RuleCollection
  , mkRuleCollection
  ) where

import {-# SOURCE #-} HIndent.Ast.Declaration.Rule
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

newtype RuleCollection =
  RuleCollection [WithComments RuleDeclaration]

instance Pretty RuleCollection where
  pretty (RuleCollection xs) =
    lined $ string "{-# RULES" : fmap pretty xs ++ [string " #-}"]

mkRuleCollection :: GHC.RuleDecls GHC.GhcPs -> RuleCollection
mkRuleCollection GHC.HsRules {..} =
  RuleCollection
    $ fmap (fmap mkRuleDeclaration . mkWithCommentsFromGenLocated) rds_rules
