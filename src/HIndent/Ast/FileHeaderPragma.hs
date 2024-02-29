module HIndent.Ast.FileHeaderPragma
  ( FileHeaderPragma
  , mkFileHeaderPragma
  ) where

import Data.Bifunctor
import Data.Char
import Data.List
import Data.List.Split
import qualified GHC.Hs as GHC
import HIndent.Ast.NodeComments
import HIndent.Pragma
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype FileHeaderPragma =
  FileHeaderPragma String

instance CommentExtraction FileHeaderPragma where
  nodeComments _ = NodeComments [] [] []

instance Pretty FileHeaderPragma where
  pretty' (FileHeaderPragma x) = string x

mkFileHeaderPragma :: GHC.EpaCommentTok -> Maybe FileHeaderPragma
mkFileHeaderPragma =
  fmap (FileHeaderPragma . uncurry constructPragma) . extractPragma

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
