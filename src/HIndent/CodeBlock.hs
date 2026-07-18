{-# LANGUAGE OverloadedStrings #-}

module HIndent.CodeBlock
  ( CodeBlock(..)
  , cppSplitBlocks
  ) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.Char (isDigit, isSpace, toUpper)
import Text.Read (readMaybe)

-- | A block of code.
data CodeBlock
  = Shebang ByteString
  | HaskellSource Int ByteString
    -- ^ Includes the starting line (indexed from 0) for error reporting
  | LinePragma ByteString
  | CPPDirectives ByteString
  deriving (Show, Eq)

-- | Break a Haskell code string into chunks, using CPP as a delimiter.
-- Lines that start with @#if@, @#end@, or @#else@ are their own chunks, and
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
  modifyLast (inBlock (<> trailing))
    . groupLines
    . classifyLines
    . zip [0 ..]
    . S8.lines
    $ inp
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
    cppLine :: ByteString -> Maybe ByteString
    cppLine src =
      case S8.uncons src of
        Just ('#', rest) -> normalizeCPPDirective rest
        _ -> Nothing
    normalizeCPPDirective :: ByteString -> Maybe ByteString
    normalizeCPPDirective src
      | directive `elem` cppDirectives =
        Just
          $ if S8.null payload
              then "#" <> directive
              else "#" <> directive <> " " <> payload
      | otherwise = Nothing
      where
        (directive, rest) = S8.break isSpace (S8.dropWhile isSpace src)
        payload = S8.dropWhile isSpace rest
    cppDirectives :: [ByteString]
    cppDirectives =
      [ "if"
      , "ifdef"
      , "ifndef"
      , "endif"
      , "else"
      , "define"
      , "undef"
      , "elif"
      , "include"
      , "error"
      , "warning"
      ]
    hasEscapedTrailingNewline :: ByteString -> Bool
    hasEscapedTrailingNewline src = "\\" `S8.isSuffixOf` src
    classifyLines :: [(Int, ByteString)] -> [CodeBlock]
    classifyLines allLines@((lineIndex, src):nextLines)
      | Just normalizedSrc <- cppLine src =
        let (cppLines, nextLines') = spanCPPLines allLines
         in CPPDirectives
              (S8.intercalate "\n" (normalizedSrc : map snd (drop 1 cppLines)))
              : classifyLines nextLines'
      | shebangLine src = Shebang src : classifyLines nextLines
      | Just normalized <- linePragmaLine src =
        LinePragma normalized : classifyLines nextLines
      | otherwise = HaskellSource lineIndex src : classifyLines nextLines
    classifyLines [] = []
    linePragmaLine :: ByteString -> Maybe ByteString
    linePragmaLine src = do
      let (indent, afterIndent) = S8.span isSpace src
      afterOpen <- S8.stripPrefix "{-#" afterIndent
      afterOpenSpace <- dropRequiredSpace afterOpen
      let (name, afterName) = S8.span (not . isSpace) afterOpenSpace
      guard $ S8.map toUpper name == "LINE"
      afterNameSpace <- dropRequiredSpace afterName
      let (lineNumber, afterLineNumber) = S8.span isDigit afterNameSpace
      guard $ not $ S8.null lineNumber
      afterLineNumberSpace <- dropRequiredSpace afterLineNumber
      (fileName, afterFileName) <- stringLiteral afterLineNumberSpace
      afterFileNameSpace <- dropRequiredSpace afterFileName
      if S8.dropWhile isSpace afterFileNameSpace == "#-}"
        then pure
               $ indent
                   <> "{-# LINE "
                   <> S8.pack (show (read (S8.unpack lineNumber) :: Integer))
                   <> " "
                   <> fileName
                   <> " #-}"
        else Nothing
      where
        dropRequiredSpace text =
          case S8.uncons text of
            Just (char, _)
              | isSpace char -> Just $ S8.dropWhile isSpace text
            _ -> Nothing
        stringLiteral text = do
          (literal, remaining) <- takeStringLiteral text
          _ <- readMaybe (S8.unpack literal) :: Maybe String
          pure (literal, remaining)
        takeStringLiteral text = do
          ('"', remaining) <- S8.uncons text
          go "\"" remaining
          where
            go literal remaining = do
              (char, next) <- S8.uncons remaining
              case char of
                '"' -> Just (literal <> "\"", next)
                '\\' -> do
                  (escaped, afterEscaped) <- S8.uncons next
                  go (literal <> S8.pack ['\\', escaped]) afterEscaped
                _ -> go (S8.snoc literal char) next
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
