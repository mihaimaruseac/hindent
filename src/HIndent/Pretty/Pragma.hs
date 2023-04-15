{-# LANGUAGE CPP #-}

-- | Pretty-printing pragmas
module HIndent.Pretty.Pragma
  ( prettyPragmas
  , pragmaExists
  , isPragma
  ) where

import Data.Bifunctor
import Data.Char
import Data.Generics.Schemes
import Data.List
import Data.List.Split
import Data.Maybe
import GHC.Hs
import HIndent.Pragma
import HIndent.Pretty.Combinators.Lineup
import HIndent.Pretty.Combinators.String
import HIndent.Printer
import Text.Regex.TDFA
-- | This function pretty-prints the module's pragmas
#if GLP961
prettyPragmas :: HsModule GhcPs -> Printer ()
#else
prettyPragmas :: HsModule -> Printer ()
#endif
prettyPragmas = lined . fmap string . collectPragmas
-- | This function returns a 'True' if the module has pragmas.
-- Otherwise, it returns a 'False'.
#if GLP961
pragmaExists :: HsModule GhcPs -> Bool
#else
pragmaExists :: HsModule -> Bool
#endif
pragmaExists = not . null . collectPragmas
-- | This function collects pragma comments from the
-- given module and modifies them into 'String's.
--
-- A pragma's name is converted to the @SHOUT_CASE@ (e.g., @lAnGuAgE@ ->
-- @LANGUAGE@).
#if GLP961
collectPragmas :: HsModule GhcPs -> [String]
collectPragmas =
  fmap (uncurry constructPragma) .
  mapMaybe extractPragma . listify isBlockComment . hsmodAnn . hsmodExt
#else
collectPragmas :: HsModule -> [String]
collectPragmas =
  fmap (uncurry constructPragma) .
  mapMaybe extractPragma . listify isBlockComment . hsmodAnn
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

-- | This function returns a 'True' if the passed 'EpaCommentTok' is
-- a pragma. Otherwise, it returns a 'False'.
isPragma :: EpaCommentTok -> Bool
isPragma (EpaBlockComment c) = match pragmaRegex c
isPragma _ = False

-- | Construct a pragma.
constructPragma :: String -> [String] -> String
constructPragma optionOrPragma xs =
  "{-# " ++ fmap toUpper optionOrPragma ++ " " ++ intercalate ", " xs ++ " #-}"

-- | Checks if the given comment is a block one.
isBlockComment :: EpaCommentTok -> Bool
isBlockComment EpaBlockComment {} = True
isBlockComment _ = False
