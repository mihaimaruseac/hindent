{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Bind
  ( Bind
  , mkBind
  ) where

import HIndent.Ast.Declaration.Bind.GuardedRhs
import HIndent.Ast.Declaration.PatternSynonym
import HIndent.Ast.Pattern
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments

-- The difference between `Function` and `Pattern` is the same as the difference
-- between `FunBind` and `PatBind` in GHC AST. See
-- https://hackage.haskell.org/package/ghc-lib-parser-9.8.2.20240223/docs/src/Language.Haskell.Syntax.Binds.html.
--
-- TODO: Merge them.
data Bind
  = Function
      { fun_matches :: GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
      }
  | Pattern
      { lhs :: WithComments Pattern
      , rhs :: WithComments GuardedRhs
      }
  | PatternSynonym (WithComments PatternSynonym)

instance CommentExtraction Bind where
  nodeComments Function {} = emptyNodeComments
  nodeComments Pattern {} = emptyNodeComments
  nodeComments PatternSynonym {} = emptyNodeComments

instance Pretty Bind where
  pretty' Function {..} = pretty fun_matches
  pretty' Pattern {..} = pretty lhs >> pretty rhs
  pretty' (PatternSynonym ps) = pretty ps

mkBind :: GHC.HsBind GHC.GhcPs -> Bind
mkBind GHC.FunBind {..} = Function {..}
mkBind GHC.PatBind {..} = Pattern {..}
  where
    lhs = mkPattern <$> fromGenLocated pat_lhs
    rhs = mkWithComments $ mkGuardedRhs pat_rhs
mkBind (GHC.PatSynBind _ psb) =
  PatternSynonym $ mkWithComments $ mkPatternSynonym psb
mkBind _ = error "This AST node should not appear."
