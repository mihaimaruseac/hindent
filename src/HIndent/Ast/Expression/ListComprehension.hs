{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.ListComprehension
  ( ListComprehension
  , mkListComprehension
  ) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty, printCommentsAnd)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments (CommentExtraction(..), emptyNodeComments)
import HIndent.Printer (Printer)

data ListComprehension = ListComprehension
  { leading :: GHC.ExprLStmt GHC.GhcPs
  , clauses :: NonEmpty (GHC.ExprLStmt GHC.GhcPs)
  }

instance CommentExtraction ListComprehension where
  nodeComments _ = emptyNodeComments

instance Pretty ListComprehension where
  pretty' ListComprehension {..} = horizontal <-|> vertical
    where
      horizontal =
        brackets
          $ spaced
              [ pretty leading
              , string "|"
              , hCommaSep $ pretty <$> NonEmpty.toList clauses
              ]
      vertical = do
        string "[ "
        renderClause leading
        newline
        forM_ (clausePairs clauses) $ \(prefix, clause) -> do
          string prefix |=> renderClause clause
          newline
        string "]"
      clausePairs (q :| qs) = ("| ", q) : fmap (", ", ) qs
      renderClause :: GHC.ExprLStmt GHC.GhcPs -> Printer ()
      renderClause clause =
        printCommentsAnd clause $ \clause' ->
          case clause' of
            GHC.ParStmt _ blocks _ _ -> vBarSep $ fmap renderParBlock blocks
            _ -> pretty clause'
      renderParBlock :: GHC.ParStmtBlock GHC.GhcPs GHC.GhcPs -> Printer ()
      renderParBlock (GHC.ParStmtBlock _ xs _ _) = vCommaSep $ fmap pretty xs
#if !MIN_VERSION_ghc_lib_parser(9, 8, 1)
      renderParBlock GHC.XParStmtBlock {} =
        error "`ghc-lib-parser` never generates this AST node."
#endif
mkListComprehension :: [GHC.ExprLStmt GHC.GhcPs] -> ListComprehension
mkListComprehension [] =
  error "List comprehension requires at least two statements."
mkListComprehension [_] =
  error "List comprehension requires at least two statements."
mkListComprehension (leading:second:rest) =
  ListComprehension {clauses = second :| rest, ..}
