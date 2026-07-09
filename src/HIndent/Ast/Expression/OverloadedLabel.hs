{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module HIndent.Ast.Expression.OverloadedLabel
  ( OverloadedLabel
  , mkOverloadedLabel
  ) where

import qualified GHC.Data.FastString as GHC
import HIndent.Ast.TextValue
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

newtype OverloadedLabel =
  OverloadedLabel TextValue

instance Pretty OverloadedLabel where
  pretty (OverloadedLabel value) = string "#" >> pretty value

mkOverloadedLabel :: GHC.FastString -> OverloadedLabel
mkOverloadedLabel value =
  OverloadedLabel $ mkTextValueFromString $ GHC.unpackFS value
