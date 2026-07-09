{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.RangeExpression
  ( RangeExpression(..)
  , mkRangeExpression
  ) where

import {-# SOURCE #-} HIndent.Ast.Expression (Expression, mkExpression)
import HIndent.Ast.WithComments (WithComments, mkWithCommentsFromGenLocated)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

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

instance Pretty RangeExpression where
  pretty (From f) = brackets $ spaced [pretty f, string ".."]
  pretty FromThen {..} =
    brackets $ spaced [pretty from >> comma >> pretty next, string ".."]
  pretty FromTo {..} = brackets $ spaced [pretty from, string "..", pretty to]
  pretty FromThenTo {..} =
    brackets
      $ spaced [pretty from >> comma >> pretty next, string "..", pretty to]

mkRangeExpression :: GHC.ArithSeqInfo GHC.GhcPs -> RangeExpression
mkRangeExpression (GHC.From f) =
  From $ mkExpression <$> mkWithCommentsFromGenLocated f
mkRangeExpression (GHC.FromThen f n) =
  FromThen
    { from = mkExpression <$> mkWithCommentsFromGenLocated f
    , next = mkExpression <$> mkWithCommentsFromGenLocated n
    }
mkRangeExpression (GHC.FromTo f t) =
  FromTo
    { from = mkExpression <$> mkWithCommentsFromGenLocated f
    , to = mkExpression <$> mkWithCommentsFromGenLocated t
    }
mkRangeExpression (GHC.FromThenTo f n t) =
  FromThenTo
    { from = mkExpression <$> mkWithCommentsFromGenLocated f
    , next = mkExpression <$> mkWithCommentsFromGenLocated n
    , to = mkExpression <$> mkWithCommentsFromGenLocated t
    }
