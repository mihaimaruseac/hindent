{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.FileHeaderPragma
  ( FileHeaderPragma
  , mkFileHeaderPragma
  ) where

import Data.Char
import Data.List.Split
import qualified Data.Text as Text
import qualified GHC.Hs as GHC
import HIndent.Ast.TextValue
import HIndent.Pragma
import HIndent.Pretty

newtype FileHeaderPragma =
  FileHeaderPragma TextValue

instance Pretty FileHeaderPragma where
  pretty (FileHeaderPragma x) = pretty x

mkFileHeaderPragma :: GHC.EpaCommentTok -> Maybe FileHeaderPragma
mkFileHeaderPragma =
  fmap (FileHeaderPragma . uncurry constructPragma) . extractPragma

-- | This function returns a @Just@ value with the pragma
-- extracted from the passed @EpaCommentTok@ if it has one. Otherwise, it
-- returns @Nothing@.
extractPragma :: GHC.EpaCommentTok -> Maybe (TextValue, [TextValue])
extractPragma (GHC.EpaBlockComment c) =
  fmap
    (\(name, elements) ->
       ( mkTextValueFromString name
       , mkTextValueFromString . strip <$> splitOn "," elements))
    (extractPragmaNameAndElement c)
  where
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
extractPragma _ = Nothing

-- | Construct a pragma.
constructPragma :: TextValue -> [TextValue] -> TextValue
constructPragma optionOrPragma xs =
  mkTextValueFromText
    $ Text.concat
        [ "{-# "
        , Text.map toUpper $ toText optionOrPragma
        , " "
        , Text.intercalate ", " $ fmap toText xs
        , " #-}"
        ]
