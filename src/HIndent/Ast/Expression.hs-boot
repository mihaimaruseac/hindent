module HIndent.Ast.Expression
  ( Expression
  , GuardExpression
  , mkExpression
  , mkGuardExpression
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty)
import HIndent.Pretty.NodeComments

data Expression

data GuardExpression

instance Pretty Expression

instance Pretty GuardExpression

instance CommentExtraction Expression

instance CommentExtraction GuardExpression

mkExpression :: GHC.HsExpr GHC.GhcPs -> Expression
mkGuardExpression :: Expression -> GuardExpression
