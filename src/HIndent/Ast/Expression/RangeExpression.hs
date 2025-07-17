{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.RangeExpression
  ( RangeExpression(..)
  , mkRangeExpression
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data RangeExpression
  = From (GHC.LHsExpr GHC.GhcPs)
  | FromThen
      { from :: GHC.LHsExpr GHC.GhcPs
      , next :: GHC.LHsExpr GHC.GhcPs
      }
  | FromTo
      { from :: GHC.LHsExpr GHC.GhcPs
      , to :: GHC.LHsExpr GHC.GhcPs
      }
  | FromThenTo
      { from :: GHC.LHsExpr GHC.GhcPs
      , next :: GHC.LHsExpr GHC.GhcPs
      , to :: GHC.LHsExpr GHC.GhcPs
      }

instance CommentExtraction RangeExpression where
  nodeComments From {} = emptyNodeComments
  nodeComments FromThen {} = emptyNodeComments
  nodeComments FromTo {} = emptyNodeComments
  nodeComments FromThenTo {} = emptyNodeComments

instance Pretty RangeExpression where
  pretty' (From f) = brackets $ spaced [pretty f, string ".."]
  pretty' (FromThen {..}) =
    brackets $ spaced [pretty from >> comma >> pretty next, string ".."]
  pretty' (FromTo {..}) =
    brackets $ spaced [pretty from, string "..", pretty to]
  pretty' (FromThenTo {..}) =
    brackets
      $ spaced [pretty from >> comma >> pretty next, string "..", pretty to]

mkRangeExpression :: GHC.ArithSeqInfo GHC.GhcPs -> RangeExpression
mkRangeExpression (GHC.From f) = From f
mkRangeExpression (GHC.FromThen f n) = FromThen {from = f, next = n}
mkRangeExpression (GHC.FromTo f t) = FromTo {from = f, to = t}
mkRangeExpression (GHC.FromThenTo f n t) =
  FromThenTo {from = f, next = n, to = t}
