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
import HIndent.GhcLibParserWrapper.GHC.Hs
import HIndent.Pragma
import HIndent.Pretty.Combinators.Lineup
import HIndent.Pretty.Combinators.String
import HIndent.Printer

-- | This function pretty-prints the module's pragmas
prettyPragmas :: HsModule' -> Printer ()
prettyPragmas = lined . fmap string . collectPragmas

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
