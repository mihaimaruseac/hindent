{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module HIndent.Ast.Expression.OverloadedLabel
  ( OverloadedLabel
  , mkOverloadedLabel
  ) where

import qualified GHC.Data.FastString as GHC
import HIndent.Ast.TextValue
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype OverloadedLabel =
  OverloadedLabel TextValue

instance Pretty OverloadedLabel where
  pretty (OverloadedLabel s) = string "#" >> pretty s

mkOverloadedLabel :: GHC.FastString -> OverloadedLabel
mkOverloadedLabel fs = OverloadedLabel $ mkTextValueFromString $ GHC.unpackFS fs
