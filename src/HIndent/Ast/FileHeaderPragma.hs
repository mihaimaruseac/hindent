module HIndent.Ast.FileHeaderPragma
  ( FileHeaderPragma
  , mkFileHeaderPragma
  ) where

import Data.Bifunctor
import Data.Char
import Data.List.Split
import qualified Data.Text as Text
import qualified GHC.Hs as GHC
import HIndent.Ast.NodeComments
import HIndent.Ast.TextValue
import HIndent.Pragma
import HIndent.Pretty
import HIndent.Pretty.NodeComments

newtype FileHeaderPragma =
  FileHeaderPragma TextValue

instance CommentExtraction FileHeaderPragma where
  nodeComments _ = NodeComments [] [] []

instance Pretty FileHeaderPragma where
  pretty' (FileHeaderPragma value) = pretty value

mkFileHeaderPragma :: GHC.EpaCommentTok -> Maybe FileHeaderPragma
mkFileHeaderPragma =
  fmap
    (FileHeaderPragma
       . uncurry constructPragma
       . bimap mkTextValueFromString (fmap mkTextValueFromString))
    . extractPragma

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
constructPragma :: TextValue -> [TextValue] -> TextValue
constructPragma optionOrPragma xs =
  mkTextValueFromText
    $ Text.concat
        [ Text.pack "{-# "
        , Text.map toUpper $ toText optionOrPragma
        , Text.pack " "
        , Text.intercalate (Text.pack ", ") $ fmap toText xs
        , Text.pack " #-}"
        ]
