module HIndent.Ast.Expression
  ( DoBlockExpression
  , Expression
  , GuardExpression
  , mkDoBlockExpression
  , mkExpression
  , mkGuardExpression
  ) where

import {-# SOURCE #-} HIndent.Pretty (Pretty)
import HIndent.Ast.WithComments (WithComments)
import HIndent.Pretty.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC

data Expression
data GuardExpression
data DoBlockExpression

instance Pretty Expression
instance Pretty DoBlockExpression
instance Pretty GuardExpression
instance CommentExtraction Expression
instance CommentExtraction DoBlockExpression
instance CommentExtraction GuardExpression

mkExpression :: GHC.HsExpr GHC.GhcPs -> Expression
mkDoBlockExpression :: Expression -> Maybe DoBlockExpression
mkGuardExpression :: WithComments Expression -> GuardExpression
