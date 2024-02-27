{-# LANGUAGE CPP #-}

-- | Pragma-related functions.
module HIndent.Pragma
  ( extractPragmasFromCode
  , extractPragmaNameAndElement
  , pragmaExists
  , isPragma
  ) where

import Data.Bifunctor
import Data.Char
import Data.Generics
import Data.List
import Data.List.Split
import Data.Maybe
import GHC.Hs
import GHC.Parser.Lexer
import HIndent.GhcLibParserWrapper.GHC.Hs
import HIndent.Parse
import Text.Regex.TDFA hiding (empty)

-- | Extracts all pragmas from the given source code.
--
-- FIXME: The function is slow because it lexicographically analyzes the
-- given source code. An alternative way is to use regular expressions.
-- However, this method cannot determine if what appears to be a pragma is
-- really a pragma, or requires complex regular expressions. For example,
-- @{-\n\n{-# LANGUAGE CPP #-}\n\n-}@ is not a pragma, but is likely to be
-- recognized as such.
extractPragmasFromCode :: String -> [(String, String)] -- ^ [(Pragma's name (e.g., @"LANGUAGE"@), Pragma's element (e.g., @"CPP, DerivingVia"@))]
extractPragmasFromCode =
  mapMaybe extractPragmaNameAndElement . mapMaybe extractBlockComment . lexCode
  where
    extractBlockComment (ITblockComment c _) = Just c
    extractBlockComment _ = Nothing

-- | Extracts the pragma's name and its element from the given pragma.
--
-- This function returns a 'Nothing' if it fails to extract them.
extractPragmaNameAndElement :: String -> Maybe (String, String) -- ^ [(Pragma's name (e.g., @"LANGUAGE"@), Pragma's element (e.g., @"CPP, DerivingVia"@))]
extractPragmaNameAndElement l
  | (_, _, _, [name, element]) <-
      match pragmaRegex l :: (String, String, String, [String]) =
    Just (name, element)
extractPragmaNameAndElement _ = Nothing

-- | This function returns a 'True' if the passed 'EpaCommentTok' is
-- a pragma. Otherwise, it returns a 'False'.
isPragma :: EpaCommentTok -> Bool
isPragma (EpaBlockComment c) = match pragmaRegex c
isPragma _ = False

-- | A regex to match against a pragma.
pragmaRegex :: Regex
pragmaRegex =
  makeRegexOpts
    compOption
    execOption
    "^{-#[[:space:]]*([^[:space:]]+)[[:space:]]+([^#]+)#-}"

-- | The option for matching against a pragma.
execOption :: ExecOption
execOption = ExecOption {captureGroups = True}

-- | The option for matching against a pragma.
--
-- 'multiline' is set to 'False' to match against multiline pragmas, e.g.,
-- @{-# LANGUAGE CPP\nOverloadedStrings #-}@.
compOption :: CompOption
compOption =
  CompOption
    { caseSensitive = True
    , multiline = False
    , rightAssoc = True
    , newSyntax = True
    , lastStarGreedy = True
    }

-- | This function returns a 'True' if the module has pragmas.
-- Otherwise, it returns a 'False'.
pragmaExists :: HsModule' -> Bool
pragmaExists = not . null . collectPragmas
-- | This function collects pragma comments from the
-- given module and modifies them into 'String's.
--
-- A pragma's name is converted to the @SHOUT_CASE@ (e.g., @lAnGuAgE@ ->
-- @LANGUAGE@).
#if MIN_VERSION_ghc_lib_parser(9,6,1)
collectPragmas :: HsModule GhcPs -> [String]
collectPragmas =
  fmap (uncurry constructPragma)
    . mapMaybe extractPragma
    . listify isBlockComment
    . hsmodAnn
    . hsmodExt
#else
collectPragmas :: HsModule -> [String]
collectPragmas =
  fmap (uncurry constructPragma)
    . mapMaybe extractPragma
    . listify isBlockComment
    . hsmodAnn
#endif
-- | This function returns a 'Just' value with the pragma
-- extracted from the passed 'EpaCommentTok' if it has one. Otherwise, it
-- returns a 'Nothing'.
extractPragma :: EpaCommentTok -> Maybe (String, [String])
extractPragma (EpaBlockComment c) =
  second (fmap strip . splitOn ",") <$> extractPragmaNameAndElement c
  where
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
extractPragma _ = Nothing

-- | Construct a pragma.
constructPragma :: String -> [String] -> String
constructPragma optionOrPragma xs =
  "{-# " ++ fmap toUpper optionOrPragma ++ " " ++ intercalate ", " xs ++ " #-}"

-- | Checks if the given comment is a block one.
isBlockComment :: EpaCommentTok -> Bool
isBlockComment EpaBlockComment {} = True
isBlockComment _ = False
