module HIndent.Ast.Expression
  ( Expression
  , mkExpression
  ) where

import                qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-}           HIndent.Pretty

data Expression

instance Pretty Expression

mkExpression :: GHC.HsExpr GHC.GhcPs -> Expression
