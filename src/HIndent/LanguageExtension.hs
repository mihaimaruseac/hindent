{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Operations related to language extensions.
module HIndent.LanguageExtension
  ( implicitExtensions
  , extensionImplies
  , collectLanguageExtensionsFromSource
  , defaultExtensions
  , allExtensions
  , getExtensions
  ) where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Text                            as T
import qualified GHC.Driver.Session                   as GLP
import qualified GHC.LanguageExtensions               as GLP
import           HIndent.LanguageExtension.Conversion
import           HIndent.LanguageExtension.Types
import           HIndent.Pragma
import           Text.Regex.TDFA

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
    toExtension (_, True, e')  = EnableExtension e'
    toExtension (_, False, e') = DisableExtension e'
extensionImplies _ = []

-- | Collect pragmas specified in the source code.
collectLanguageExtensionsFromSource :: String -> [Extension]
collectLanguageExtensionsFromSource =
  (++) <$> collectLanguageExtensionsSpecifiedViaLanguagePragma <*>
  collectLanguageExtensionsFromSourceViaOptionsPragma

-- | Consume an extensions list from arguments.
getExtensions :: [T.Text] -> [Extension]
getExtensions = foldr (f . T.unpack) defaultExtensions
  where
    f "Haskell98" _ = []
    f x a =
      case strToExt x of
        Just x'@EnableExtension {} -> x' : delete x' a
        Just (DisableExtension x') -> delete (EnableExtension x') a
        _                          -> error $ "Unknown extension: " ++ x

-- | Collects language extensions enabled or disabled by @{-# LANGUAGE FOO
-- #-}@.
--
-- This function ignores language extensions not supported by Cabal.
collectLanguageExtensionsSpecifiedViaLanguagePragma :: String -> [Extension]
collectLanguageExtensionsSpecifiedViaLanguagePragma =
  mapMaybe (strToExt . stripSpaces) .
  concatMap (splitOn ",") .
  fmap snd . filter ((== "LANGUAGE") . fst) . extractPragmasFromCode

-- | Extracts the language extensions specified by @-XFOO@ from @OPTIONS@
-- or @OPTIONS_GHC@ pragmas
collectLanguageExtensionsFromSourceViaOptionsPragma :: String -> [Extension]
collectLanguageExtensionsFromSourceViaOptionsPragma =
  mapMaybe (strToExt . stripSpaces) .
  concatMap extractLanguageExtensionsFromOptions .
  fmap snd .
  filter ((`elem` ["OPTIONS", "OPTIONS_GHC"]) . fst) . extractPragmasFromCode

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

-- | Default extensions.
defaultExtensions :: [Extension]
defaultExtensions = fmap EnableExtension $ [minBound ..] \\ badExtensions

-- | All extensions supported by Cabal.
allExtensions :: [Extension]
allExtensions = fmap EnableExtension [minBound ..]

-- | Extensions which steal too much syntax.
badExtensions :: [GLP.Extension]
badExtensions =
  [ GLP.Arrows -- steals proc
  , GLP.TransformListComp -- steals the group keyword
  , GLP.UnboxedTuples -- breaks (#) lens operator
  , GLP.UnboxedSums -- Same as 'UnboxedTuples'
    -- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
  , GLP.PatternSynonyms -- steals the pattern keyword
  , GLP.RecursiveDo -- steals the rec keyword
  , GLP.TypeApplications -- since GHC
  , GLP.StaticPointers -- Steals the `static` keyword
  , GLP.AlternativeLayoutRule -- Breaks a few tests
  , GLP.AlternativeLayoutRuleTransitional -- Same as `AlternativeLayoutRule`
  , GLP.LexicalNegation -- Cannot handle minus signs in some cases
  , GLP.OverloadedRecordDot -- Breaks 'a.b'
  , GLP.OverloadedRecordUpdate -- Cannot handle symbol members starting
                               -- with a dot in a record well
  ]
