{-# LANGUAGE RecordWildCards #-}

-- | Error types and functions.
module HIndent.Error
  ( ParseError(..)
  , prettyParseError
  ) where

-- | Parse error type with the location.
data ParseError = ParseError
  { errorLine :: Int -- ^ The row of the parse error's location.
  , errorCol :: Int -- ^ The column of the parse error's location.
  , errorFile :: FilePath -- ^ The filename HIndent failed to parse.
  } deriving (Eq, Ord, Show, Read)

-- | Pretty-print `ParseError`.
prettyParseError :: ParseError -> String
prettyParseError ParseError {..} =
  "Parse failed at ("
    <> show errorLine
    <> ", "
    <> show errorCol
    <> ") in "
    <> errorFile
    <> "."
