{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Operations related to language extensions.
module HIndent.LanguageExtension
  ( implicitExtensions
  , extensionImplies
  , collectLanguageExtensionsFromSource
  , getExtensions
  , defaultExtensions
  ) where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import qualified GHC.Driver.Session as GLP
import HIndent.LanguageExtension.Conversion
import HIndent.LanguageExtension.Types
import HIndent.Pragma
import Text.Regex.TDFA
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
import qualified GHC.LanguageExtensions.Type as GLP
#endif
-- | This function returns a list of extensions that the passed language
-- (e.g., GHC2021) enables.
implicitExtensions :: GLP.Language -> [Extension]
implicitExtensions = fmap EnableExtension . GLP.languageExtensions . Just

-- | This function returns a list of extensions that the passed extension
-- enables and disables.
--
-- For example, @GADTs@ enables @GADTSyntax@ and @RebindableSyntax@
-- disables @ImplicitPrelude@.
extensionImplies :: Extension -> [Extension]
extensionImplies (EnableExtension e) =
  toExtension <$> filter (\(a, _, _) -> e == a) GLP.impliedXFlags
  where
    toExtension (_, True, e') = EnableExtension e'
    toExtension (_, False, e') = DisableExtension e'
extensionImplies _ = []

-- | Collect pragmas specified in the source code.
collectLanguageExtensionsFromSource :: String -> [Extension]
collectLanguageExtensionsFromSource =
  (++)
    <$> collectLanguageExtensionsSpecifiedViaLanguagePragma
    <*> collectLanguageExtensionsFromSourceViaOptionsPragma

-- | Consume an extensions list from arguments.
getExtensions :: [String] -> [Extension]
getExtensions = foldr f defaultExtensions
  where
    f "Haskell98" _ = []
    f x a =
      case strToExt x of
        Just x'@EnableExtension {} -> x' : delete x' a
        Just (DisableExtension x') -> delete (EnableExtension x') a
        _ -> error $ "Unknown extension: " ++ x

-- | Collects language extensions enabled or disabled by @{-# LANGUAGE FOO
-- #-}@.
--
-- This function ignores language extensions not supported by Cabal.
collectLanguageExtensionsSpecifiedViaLanguagePragma :: String -> [Extension]
collectLanguageExtensionsSpecifiedViaLanguagePragma =
  concatMap ((mapMaybe (strToExt . stripSpaces) . splitOn ",") . snd)
    . filter ((== "LANGUAGE") . fst)
    . extractPragmasFromCode

-- | Extracts the language extensions specified by @-XFOO@ from @OPTIONS@
-- or @OPTIONS_GHC@ pragmas
collectLanguageExtensionsFromSourceViaOptionsPragma :: String -> [Extension]
collectLanguageExtensionsFromSourceViaOptionsPragma =
  concatMap
    (mapMaybe (strToExt . stripSpaces)
       . extractLanguageExtensionsFromOptions
       . snd)
    . filter ((`elem` ["OPTIONS", "OPTIONS_GHC"]) . fst)
    . extractPragmasFromCode

-- | Extracts the language extensions specified in the '-XFOO' format from
-- the given string
extractLanguageExtensionsFromOptions :: String -> [String]
extractLanguageExtensionsFromOptions options =
  fmap
    trimXOption
    (getAllTextMatches (options =~ "-X[^,[:space:]]+") :: [String])
  where
    trimXOption ('-':'X':xs) = xs
    trimXOption _ = error "Unreachable: the option must have the `-X` prefix."

-- | Removes spaces before and after the string.
stripSpaces :: String -> String
stripSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

defaultExtensions :: [Extension]
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
defaultExtensions =
  EnableExtension GLP.ListTuplePuns : implicitExtensions GLP.GHC2021
#else
defaultExtensions = implicitExtensions GLP.GHC2021
#endif
