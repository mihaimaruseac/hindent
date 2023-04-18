{-# LANGUAGE CPP #-}

-- | Wrapper for 'GHC.Hs'
module HIndent.GhcLibParserWrapper.GHC.Hs
  ( module GHC.Hs
  , HsModule'
  ) where

import GHC.Hs
#if MIN_VERSION_ghc_lib_parser(9,6,1)
type HsModule' = HsModule GhcPs
#else
type HsModule' = HsModule
#endif
