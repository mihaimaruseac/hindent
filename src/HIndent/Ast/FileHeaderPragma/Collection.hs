{-# LANGUAGE CPP #-}

module HIndent.Ast.FileHeaderPragma.Collection
  ( FileHeaderPragmaCollection
  , mkFileHeaderPragmaCollection
  , hasPragmas
  ) where

import Data.Bifunctor
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Generics.SYB
import HIndent.Ast.FileHeaderPragma
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pragma
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype FileHeaderPragmaCollection =
  FileHeaderPragmaCollection [FileHeaderPragma]

instance CommentExtraction FileHeaderPragmaCollection where
  nodeComments _ = NodeComments [] [] []

instance Pretty FileHeaderPragmaCollection where
  pretty' (FileHeaderPragmaCollection xs) = lined $ fmap pretty xs

mkFileHeaderPragmaCollection :: GHC.HsModule' -> FileHeaderPragmaCollection
mkFileHeaderPragmaCollection =
  FileHeaderPragmaCollection . fmap mkFileHeaderPragma . collectPragmas

hasPragmas :: FileHeaderPragmaCollection -> Bool
hasPragmas (FileHeaderPragmaCollection xs) = not $ null xs

-- | This function collects pragma comments from the
-- given module and modifies them into 'String's.
--
-- A pragma's name is converted to the @SHOUT_CASE@ (e.g., @lAnGuAgE@ ->
-- @LANGUAGE@).
collectPragmas :: GHC.HsModule' -> [String]
collectPragmas =
  fmap (uncurry constructPragma)
    . mapMaybe extractPragma
    . listify isBlockComment
    . GHC.getModuleAnn

-- | This function returns a 'Just' value with the pragma
-- extracted from the passed 'EpaCommentTok' if it has one. Otherwise, it
-- returns a 'Nothing'.
extractPragma :: GHC.EpaCommentTok -> Maybe (String, [String])
extractPragma (GHC.EpaBlockComment c) =
  second (fmap strip . splitOn ",") <$> extractPragmaNameAndElement c
  where
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
extractPragma _ = Nothing

-- | Construct a pragma.
constructPragma :: String -> [String] -> String
constructPragma optionOrPragma xs =
  "{-# " ++ fmap toUpper optionOrPragma ++ " " ++ intercalate ", " xs ++ " #-}"

-- | Checks if the given comment is a block one.
isBlockComment :: GHC.EpaCommentTok -> Bool
isBlockComment GHC.EpaBlockComment {} = True
isBlockComment _ = False
