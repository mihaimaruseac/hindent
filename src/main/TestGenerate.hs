module Main(main) where

import Control.Monad
import Control.Applicative
import Data.List (find, unfoldr, isPrefixOf, intercalate)

import System.Directory
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L

import qualified HIndent

testDir, expectedDir :: String
testDir = "tests"
expectedDir = "expected"

-- This executable generates a test suite for a style using a test suite
-- from a previous style. Given arguments `from` and `to` which are style names,
-- it will take all the input files for `from`, create identical input files for `to`,
-- run each block in the input files through the `to` style, and generate expectation files matching the output.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [from, to] -> generateTests from to
    _          -> putStrLn "Two arguments required: source tests and destination tests."

generateTests :: String -> String -> IO ()
generateTests from to = do
  --  Find the source and destination styles.
  let Just toStyle = findStyle (T.pack to)

  --  Get all the test and expectation files for the original test suite.
  testFilenamesFrom <- filter (not . isPrefixOf ".") <$> getDirectoryContents (tests from)

  -- Verify the target directories exist.
  createDirectoryIfMissing True $ tests to
  createDirectoryIfMissing True $ expected to

  -- Copy test files to the new style test directory.
  forM_ testFilenamesFrom $ \filename ->
    copyFile (tests from ++ filename) (tests to ++ filename)

  -- Generate expectation files for the new style test directory.
  forM_ testFilenamesFrom $ \filename -> do
    let dstFilename = expected to ++ expectedFilename filename
    contents <- expectationFileContents toStyle <$> readFile (tests to ++ filename)
    writeFile dstFilename contents

  where
    findStyle style = find ((== style). HIndent.styleName) HIndent.styles
    tests style = "test/" ++ style ++ "/" ++ testDir ++ "/"
    expected style = "test/" ++ style ++ "/" ++ expectedDir ++ "/"
    expectedFilename filename = take (length filename - 4) filename ++ "exp"

expectationFileContents :: HIndent.Style -> String -> String
expectationFileContents style contents =
  let testDecls = parsePieces contents
      fmt input = L.unpack $ L.toLazyText $ case HIndent.reformat style Nothing $ L.pack input of
                                              Left err      -> error err
                                              Right builder -> builder
      outputs = map (replaceEmptyNewlines . fmt) testDecls
  in intercalate "\n" outputs
  where
    replaceEmptyNewlines = unlines . map replaceNewline . lines
    replaceNewline "" = ">"
    replaceNewline x = x

parsePieces :: String -> [String]
parsePieces str = map (intercalate "\n" . map mkNewlines) pieces
  where
    pieces = unfoldr unfolder (lines str)

    unfolder :: [String] -> Maybe ([String], [String])
    unfolder [] = Nothing
    unfolder remaining = Just $
     case break pieceBreak (zip remaining (tail remaining ++ [""]))  of
       (nonNull, [])     -> (map fst nonNull, [])
       (nonNull, _:rest) -> (map fst nonNull, map fst rest)

    pieceBreak :: (String, String) -> Bool
    pieceBreak ("", "") = error "Two consecutive line breaks!"
    pieceBreak (line, next) = null line && head next /= ' '

    mkNewlines :: String -> String
    mkNewlines ">" = ""
    mkNewlines x = x
