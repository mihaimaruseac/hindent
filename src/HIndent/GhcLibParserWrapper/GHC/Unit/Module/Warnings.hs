{-# LANGUAGE CPP #-}

module HIndent.GhcLibParserWrapper.GHC.Unit.Module.Warnings
  ( module GHC.Unit.Module.Warnings
  , WarningTxt'
  ) where

import GHC.Unit.Module.Warnings
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
import GHC.Hs

type WarningTxt' = WarningTxt GhcPs
#else
type WarningTxt' = WarningTxt
#endif
