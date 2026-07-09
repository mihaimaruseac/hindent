{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module HIndent.Ast.Type.Strictness
  ( Strictness
  , mkStrictness
  ) where

import qualified GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data Strictness
  = Lazy
  | Strict
  deriving (Eq)

instance Pretty Strictness where
  pretty Lazy = string "~"
  pretty Strict = string "!"

mkStrictness :: GHC.SrcStrictness -> Maybe Strictness
mkStrictness GHC.SrcLazy = Just Lazy
mkStrictness GHC.SrcStrict = Just Strict
mkStrictness GHC.NoSrcStrict = Nothing
