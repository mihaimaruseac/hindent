{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.Declaration.Foreign.Safety
  ( Safety
  , mkSafety
  ) where

import qualified GHC.Types.ForeignCall as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data Safety
  = Safe
  | Interruptible
  | Unsafe

instance Pretty Safety where
  pretty Safe = string "safe"
  pretty Interruptible = string "interruptible"
  pretty Unsafe = string "unsafe"

mkSafety :: GHC.Safety -> Safety
mkSafety GHC.PlaySafe = Safe
mkSafety GHC.PlayInterruptible = Interruptible
mkSafety GHC.PlayRisky = Unsafe
