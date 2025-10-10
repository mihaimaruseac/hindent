{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.RangeExpression
  ( RangeExpression(..)
  , mkRangeExpression
  ) where

import {-# SOURCE #-} HIndent.Ast.Expression (Expression, mkExpression)
import HIndent.Ast.WithComments (WithComments, fromGenLocated)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data RangeExpression
  = From (WithComments Expression)
  | FromThen
      { from :: WithComments Expression
      , next :: WithComments Expression
      }
  | FromTo
      { from :: WithComments Expression
      , to :: WithComments Expression
      }
  | FromThenTo
      { from :: WithComments Expression
      , next :: WithComments Expression
      , to :: WithComments Expression
      }

instance CommentExtraction RangeExpression where
  nodeComments From {} = emptyNodeComments
  nodeComments FromThen {} = emptyNodeComments
  nodeComments FromTo {} = emptyNodeComments
  nodeComments FromThenTo {} = emptyNodeComments

instance Pretty RangeExpression where
  pretty' (From f) = brackets $ spaced [pretty f, string ".."]
  pretty' FromThen {..} =
    brackets $ spaced [pretty from >> comma >> pretty next, string ".."]
  pretty' FromTo {..} = brackets $ spaced [pretty from, string "..", pretty to]
  pretty' FromThenTo {..} =
    brackets
      $ spaced [pretty from >> comma >> pretty next, string "..", pretty to]

mkRangeExpression :: GHC.ArithSeqInfo GHC.GhcPs -> RangeExpression
mkRangeExpression (GHC.From f) = From $ mkExpression <$> fromGenLocated f
mkRangeExpression (GHC.FromThen f n) =
  FromThen
    { from = mkExpression <$> fromGenLocated f
    , next = mkExpression <$> fromGenLocated n
    }
mkRangeExpression (GHC.FromTo f t) =
  FromTo
    { from = mkExpression <$> fromGenLocated f
    , to = mkExpression <$> fromGenLocated t
    }
mkRangeExpression (GHC.FromThenTo f n t) =
  FromThenTo
    { from = mkExpression <$> fromGenLocated f
    , next = mkExpression <$> fromGenLocated n
    , to = mkExpression <$> fromGenLocated t
    }
