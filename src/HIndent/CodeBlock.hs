{-# LANGUAGE OverloadedStrings #-}

module HIndent.CodeBlock
  ( CodeBlock(..)
  , cppSplitBlocks
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8

-- | A block of code.
data CodeBlock
    = Shebang ByteString
    | HaskellSource Int ByteString
    -- ^ Includes the starting line (indexed from 0) for error reporting
    | CPPDirectives ByteString
     deriving (Show, Eq)

-- | Break a Haskell code string into chunks, using CPP as a delimiter.
-- Lines that start with '#if', '#end', or '#else' are their own chunks, and
-- also act as chunk separators. For example, the code
--
-- > #ifdef X
-- > x = X
-- > y = Y
-- > #else
-- > x = Y
-- > y = X
-- > #endif
--
-- will become five blocks, one for each CPP line and one for each pair of declarations.
cppSplitBlocks :: ByteString -> [CodeBlock]
cppSplitBlocks inp =
  modifyLast (inBlock (<> trailing)) .
  groupLines . classifyLines . zip [0 ..] . S8.lines $
  inp
  where
    groupLines :: [CodeBlock] -> [CodeBlock]
    groupLines (line1:line2:remainingLines) =
      case mergeLines line1 line2 of
        Just line1And2 -> groupLines (line1And2 : remainingLines)
        Nothing -> line1 : groupLines (line2 : remainingLines)
    groupLines xs@[_] = xs
    groupLines xs@[] = xs
    mergeLines :: CodeBlock -> CodeBlock -> Maybe CodeBlock
    mergeLines (CPPDirectives src1) (CPPDirectives src2) =
      Just $ CPPDirectives (src1 <> "\n" <> src2)
    mergeLines (Shebang src1) (Shebang src2) =
      Just $ Shebang (src1 <> "\n" <> src2)
    mergeLines (HaskellSource lineNumber1 src1) (HaskellSource _lineNumber2 src2) =
      Just $ HaskellSource lineNumber1 (src1 <> "\n" <> src2)
    mergeLines _ _ = Nothing
    shebangLine :: ByteString -> Bool
    shebangLine = S8.isPrefixOf "#!"
    cppLine :: ByteString -> Bool
    cppLine src =
      any
        (`S8.isPrefixOf` src)
        ["#if", "#end", "#else", "#define", "#undef", "#elif", "#include", "#error", "#warning"]
        -- Note: #ifdef and #ifndef are handled by #if
    hasEscapedTrailingNewline :: ByteString -> Bool
    hasEscapedTrailingNewline src = "\\" `S8.isSuffixOf` src
    classifyLines :: [(Int, ByteString)] -> [CodeBlock]
    classifyLines allLines@((lineIndex, src):nextLines)
      | cppLine src =
        let (cppLines, nextLines') = spanCPPLines allLines
         in CPPDirectives (S8.intercalate "\n" (map snd cppLines)) :
            classifyLines nextLines'
      | shebangLine src = Shebang src : classifyLines nextLines
      | otherwise = HaskellSource lineIndex src : classifyLines nextLines
    classifyLines [] = []
    spanCPPLines ::
         [(Int, ByteString)] -> ([(Int, ByteString)], [(Int, ByteString)])
    spanCPPLines (line@(_, src):nextLines)
      | hasEscapedTrailingNewline src =
        let (cppLines, nextLines') = spanCPPLines nextLines
         in (line : cppLines, nextLines')
      | otherwise = ([line], nextLines)
    spanCPPLines [] = ([], [])
    -- Hack to work around some parser issues in haskell-src-exts: Some pragmas
    -- need to have a newline following them in order to parse properly, so we include
    -- the trailing newline in the code block if it existed.
    trailing :: ByteString
    trailing =
      if S8.isSuffixOf "\n" inp
        then "\n"
        else ""
    modifyLast :: (a -> a) -> [a] -> [a]
    modifyLast _ [] = []
    modifyLast f [x] = [f x]
    modifyLast f (x:xs) = x : modifyLast f xs
    inBlock :: (ByteString -> ByteString) -> CodeBlock -> CodeBlock
    inBlock f (HaskellSource line txt) = HaskellSource line (f txt)
    inBlock _ dir = dir
