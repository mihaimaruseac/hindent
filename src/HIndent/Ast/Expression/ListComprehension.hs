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
import HIndent.Ast.Statement (ExprStatement)
import HIndent.Ast.WithComments (WithComments)
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments (CommentExtraction(..), emptyNodeComments)
import HIndent.Printer (Printer)

data ListComprehension = ListComprehension
  { leading :: WithComments ExprStatement
  , clauses :: NonEmpty (WithComments ExprStatement)
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
        pretty leading
        newline
        forM_ (clausePairs clauses) $ \(prefix, clause) -> do
          string prefix |=> pretty clause
          newline
        string "]"
      clausePairs (q :| qs) = ("| ", q) : fmap (", ", ) qs

mkListComprehension :: [WithComments ExprStatement] -> ListComprehension
mkListComprehension [] =
  error "List comprehension requires at least two statements."
mkListComprehension [_] =
  error "List comprehension requires at least two statements."
mkListComprehension (leading:second:rest) =
  ListComprehension {clauses = second :| rest, ..}
