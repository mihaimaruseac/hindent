{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.WhereClause
  ( WhereClause
  , mkWhereClause
  , mkMatchWhereClause
  , mkPatternWhereClause
  ) where

import HIndent.Ast.LocalBinds (LocalBinds, mkLocalBinds)
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.WithComments (WithComments, mkWithComments, prettyWith)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments (CommentExtraction(..))

newtype WhereClause = WhereClause
  { binds :: WithComments LocalBinds
  }

instance CommentExtraction WhereClause where
  nodeComments _ = NodeComments [] [] []

instance Pretty WhereClause where
  pretty' WhereClause {..} =
    lined [string "where", prettyWith binds $ indentedBlock . pretty]

mkMatchWhereClause ::
     GHC.Match GHC.GhcPs body -> Maybe (WithComments WhereClause)
mkMatchWhereClause GHC.Match {..} = mkWhereClause m_grhss

mkPatternWhereClause :: GHC.HsBind GHC.GhcPs -> Maybe (WithComments WhereClause)
mkPatternWhereClause GHC.PatBind {..} = mkWhereClause pat_rhs
mkPatternWhereClause _ = error "This AST node should not appear."

mkWhereClause :: GHC.GRHSs GHC.GhcPs body -> Maybe (WithComments WhereClause)
mkWhereClause GHC.GRHSs {..} = do
  binds <- mkLocalBinds grhssLocalBinds
  pure $ mkWithComments WhereClause {..}
