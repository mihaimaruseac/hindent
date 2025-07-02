{-# LANGUAGE CPP #-}

-- | Operations for handling languages (e.g., Haskell2010).
module HIndent.Language
  ( convertLanguage
  ) where

import qualified GHC.Driver.Session as GLP
import GHC.Stack
import qualified Language.Haskell.Extension as Cabal

-- | This function converts a value of 'Language' defined in the 'Cabal'
-- package to the same value of 'Language' defined in the 'ghc-lib-parser'
-- package.
--
-- This function raises an error if a 'UnknownLanguage' value is passed.
convertLanguage :: HasCallStack => Cabal.Language -> GLP.Language
convertLanguage Cabal.Haskell98 = GLP.Haskell98
convertLanguage Cabal.Haskell2010 = GLP.Haskell2010
#if MIN_VERSION_Cabal(3, 6, 0)
convertLanguage Cabal.GHC2021 = GLP.GHC2021
#endif
#if MIN_VERSION_Cabal(3, 12, 0)
#if MIN_VERSION_ghc_lib_parser(9, 10, 0)
convertLanguage Cabal.GHC2024 = GLP.GHC2024
#else
convertLanguage Cabal.GHC2024 =
  error
    "GHC2024 language is not supported. Hindent was built with GHC < 9.10. Please rebuild Hindent with GHC 9.10 or later."
#endif
#endif
convertLanguage (Cabal.UnknownLanguage s) = error $ "Unknown language: " ++ s
