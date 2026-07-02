{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.WhereClause
  ( WhereClause
  , mkWhereClause
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
  pretty WhereClause {..} =
    lined [string "where", prettyWith binds $ indentedBlock . pretty]

mkWhereClause :: GHC.GRHSs GHC.GhcPs body -> Maybe (WithComments WhereClause)
mkWhereClause GHC.GRHSs {..} = do
  binds <- mkLocalBinds grhssLocalBinds
  pure $ mkWithComments WhereClause {..}
