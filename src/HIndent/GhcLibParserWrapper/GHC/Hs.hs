{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

-- | Wrapper for 'GHC.Hs'
module HIndent.GhcLibParserWrapper.GHC.Hs
  ( module GHC.Hs
  , HsModule'
  , getModuleAnn
  , getDeprecMessage
  ) where

import GHC.Hs
import HIndent.GhcLibParserWrapper.GHC.Unit.Module.Warnings
-- | The wrapper for `HsModule`
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
type HsModule' = HsModule GhcPs
#else
type HsModule' = HsModule
#endif
getModuleAnn :: HsModule' -> EpAnn AnnsModule
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
getModuleAnn HsModule {hsmodExt = XModulePs {..}} = hsmodAnn
#else
getModuleAnn HsModule {..} = hsmodAnn
#endif
getDeprecMessage :: HsModule' -> Maybe (LocatedP WarningTxt')
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
getDeprecMessage HsModule {hsmodExt = XModulePs {..}} = hsmodDeprecMessage
#else
getDeprecMessage HsModule {..} = hsmodDeprecMessage
#endif
