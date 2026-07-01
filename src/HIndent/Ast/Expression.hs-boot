module HIndent.Ast.Expression
  ( Expression
  , GuardExpression
  , mkExpression
  , mkGuardExpression
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty (Pretty)

data Expression

data GuardExpression

instance Pretty Expression

instance Pretty GuardExpression

mkExpression :: GHC.HsExpr GHC.GhcPs -> Expression
mkGuardExpression :: Expression -> GuardExpression
