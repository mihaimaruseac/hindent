{-# LANGUAGE OverloadedStrings #-}

module HIndent.CodeBlock
  ( CodeBlock(..)
  , cppSplitBlocks
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Function
import Data.List
import Data.Monoid

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
  map (classify . unlines') .
  groupBy ((==) `on` nonHaskellLine) . zip [0 ..] . S8.lines $
  inp
  where
    nonHaskellLine :: (Int, ByteString) -> Bool
    nonHaskellLine (_, src) = cppLine src || shebangLine src
    shebangLine :: ByteString -> Bool
    shebangLine = S8.isPrefixOf "#!"
    cppLine :: ByteString -> Bool
    cppLine src =
      any
        (`S8.isPrefixOf` src)
        ["#if", "#end", "#else", "#define", "#undef", "#elif", "#include", "#error", "#warning"]
        -- Note: #ifdef and #ifndef are handled by #if
    unlines' :: [(Int, ByteString)] -> (Int, ByteString)
    unlines' [] = (0, S.empty)
    unlines' srcs@((line, _):_) =
      (line, mconcat . intersperse "\n" $ map snd srcs)
    classify :: (Int, ByteString) -> CodeBlock
    classify (line, text)
      | shebangLine text = Shebang text
      | cppLine text = CPPDirectives text
      | otherwise = HaskellSource line text
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
