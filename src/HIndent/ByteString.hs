{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions to manipulate `ByteString`.
module HIndent.ByteString
  ( findPrefix
  , stripPrefix
  , addPrefix
  , unlines'
  , hasTrailingLine
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.List (intersperse)
import Data.Maybe

-- | Returns the prefix that all the given `ByteString`s except for the ones composed of `\n`s have.
--
-- The regex of the prefix is `>?[ \t]*`.
findPrefix :: [ByteString] -> ByteString
findPrefix = takePrefix . findCommonPrefix . dropNewlines

-- | Removes the given prefix from the passed `ByteString`, or raises an error.
stripPrefix :: ByteString -> ByteString -> ByteString
stripPrefix prefix =
  fromMaybe (error "Missing expected prefix") . S.stripPrefix prefix

-- | Add a prefix to all lines in a `ByteString`.
addPrefix :: ByteString -> ByteString -> ByteString
addPrefix prefix = unlines' . map (prefix <>) . S8.lines

-- | Returns the prefix that all the given `ByteString`s have.
findCommonPrefix :: [ByteString] -> ByteString
findCommonPrefix [] = ""
findCommonPrefix ("":_) = ""
findCommonPrefix (p:ps) =
  if all (startsWithChar first) ps
    then S8.cons first (findCommonPrefix (S.tail p : map S.tail ps))
    else ""
  where
    first = S8.head p

-- | `unlines'` for `ByteString`.
unlines' :: [ByteString] -> ByteString
unlines' = S.concat . intersperse "\n"

-- | Returns the prefix from the `ByteString`
--
-- The regex of the prefix is `>?[ \t]*`.
takePrefix :: ByteString -> ByteString
takePrefix txt
  | S8.null txt = ""
  | S8.head txt == '>' = S8.cons '>' $ takeSpaceOrTab $ S8.tail txt
  | otherwise = takeSpaceOrTab txt

-- | Filters out `ByteString`s composed of only `\n`s.
dropNewlines :: [ByteString] -> [ByteString]
dropNewlines = filter (not . S.null . S8.dropWhile (== '\n'))

-- | `takeWhile` for spaces or tabs
takeSpaceOrTab :: ByteString -> ByteString
takeSpaceOrTab = S8.takeWhile isSpaceOrTab

-- | Does the strict bytestring have a trailing newline?
hasTrailingLine :: ByteString -> Bool
hasTrailingLine xs = not (S8.null xs) && S8.last xs == '\n'

-- | Returns if the `ByteString` starts with the given `Char`.
startsWithChar :: Char -> ByteString -> Bool
startsWithChar c x = S8.length x > 0 && S8.head x == c

-- | Returns if the `Char` is either a space or a tab.
isSpaceOrTab :: Char -> Bool
isSpaceOrTab = (`elem` [' ', '\t'])
