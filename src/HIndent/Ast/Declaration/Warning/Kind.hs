{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.Declaration.Warning.Kind
  ( Kind(..)
  ) where

import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data Kind
  = Warning
  | Deprecated

instance Pretty Kind where
  pretty Warning = string "WARNING"
  pretty Deprecated = string "DEPRECATED"
