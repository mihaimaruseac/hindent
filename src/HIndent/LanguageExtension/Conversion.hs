{-# LANGUAGE CPP #-}

-- | Operations for converting extensions types.
module HIndent.LanguageExtension.Conversion
  ( fromCabalExtension
  , uniqueExtensions
  , convertExtension
  , strToExt
  ) where

import qualified GHC.LanguageExtensions                             as GLP
import           HIndent.LanguageExtension.Types
import qualified Language.Haskell.Extension                         as Cabal
import qualified Language.Haskell.GhclibParserEx.GHC.Driver.Session as GLP

-- | Converts from an `Extension` defined in the `Cabal` package to an
-- `Extension` defined in HIndent.
--
-- Note that this function returns `Nothing` if `UnknownExtension` is
-- passed or if an extension is not supported by GHC.
fromCabalExtension :: Cabal.Extension -> Maybe Extension
fromCabalExtension (Cabal.EnableExtension x) =
  EnableExtension <$> convertExtension x
fromCabalExtension (Cabal.DisableExtension x) =
  DisableExtension <$> convertExtension x
fromCabalExtension Cabal.UnknownExtension {} = Nothing

-- | This function converts each value of the type 'Extension' defined in
-- 'HIndent.LanguageExtension.Types' in the list to the same value of the
-- type 'Extension' defined in the package 'ghc-lib-parser'.
--
-- If the extension has the 'No' suffix, the extension is removed from the
-- result. If both extensions having and not having the suffix exist in the
-- list, only the most backward one has the effect.
--
-- If converting an extension fails due to neither GHC nor 'ghc-lib-parser'
-- not supporting, or deprecation or removal, the extension is ignored.
uniqueExtensions :: [Extension] -> [GLP.Extension]
uniqueExtensions [] = []
uniqueExtensions ((EnableExtension e):xs) = e : uniqueExtensions xs
uniqueExtensions ((DisableExtension e):xs) =
  uniqueExtensions $ filter (/= EnableExtension e) xs

-- | This function converts a value of 'KnownExtension' defined in the
-- 'Cabal' package to the same value of 'Extension' defined in
-- 'ghc-lib-parser'.
--
-- This function returns a 'Just' value if it succeeds in converting.
-- Otherwise (e.g., 'ghc-lib-parser' does not the passed extension, or it
-- is deprecated or removed), it returns a 'Nothing'.
convertExtension :: Cabal.KnownExtension -> Maybe GLP.Extension
convertExtension = GLP.readExtension . show

-- | Converts the given string to an extension, or returns a 'Nothing' on
-- fail.
strToExt :: String -> Maybe Extension
strToExt ('N':'o':s) = DisableExtension <$> GLP.readExtension s
strToExt s           = EnableExtension <$> GLP.readExtension s
