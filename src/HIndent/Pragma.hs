{-# LANGUAGE CPP #-}

-- | Pragma-related functions.
module HIndent.Pragma
  ( extractPragmasFromCode
  , extractPragmaNameAndElement
  , pragmaRegex
  ) where

import           Data.Maybe
import           GHC.Parser.Lexer
import           HIndent.Parse
import           Text.Regex.TDFA  hiding (empty)

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
    extractBlockComment _                    = Nothing

-- | Extracts the pragma's name and its element from the given pragma.
--
-- This function returns a 'Nothing' if it fails to extract them.
extractPragmaNameAndElement :: String -> Maybe (String, String) -- ^ [(Pragma's name (e.g., @"LANGUAGE"@), Pragma's element (e.g., @"CPP, DerivingVia"@))]
extractPragmaNameAndElement l
  | (_, _, _, [name, element]) <-
      match pragmaRegex l :: (String, String, String, [String]) =
    Just (name, element)
extractPragmaNameAndElement _ = Nothing

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
