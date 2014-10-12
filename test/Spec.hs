module Main(main) where

import Control.Monad
import Control.Applicative
import Data.List (find, unfoldr, break, isPrefixOf, intercalate)

import Test.Hspec
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L

import qualified HIndent

styles :: [FilePath]
styles = ["gibiansky"]

testDir, expectedDir :: String
testDir = "tests"
expectedDir = "expected"

main :: IO ()
main = forM_ styles testStyle

testStyle :: FilePath -> IO ()
testStyle style = do
  let Just theStyle = find ((== T.pack style). HIndent.styleName) HIndent.styles
  testFilenames <- filter (not . isPrefixOf ".") <$> getDirectoryContents tests
  let expFiles = map ((expected ++) . expectedFilename) testFilenames
      testFiles = map (tests++) testFilenames
  specs <- forM (zip testFiles expFiles) (uncurry $ useTestFiles theStyle)
  hspec $ foldl1 (>>) specs

  where
    tests = "test/" ++ style ++ "/" ++ testDir ++ "/"
    expected = "test/" ++ style ++ "/" ++ expectedDir ++ "/"

    expectedFilename filename = take (length filename - 4) filename ++ "exp"

useTestFiles :: HIndent.Style -> FilePath -> FilePath -> IO Spec
useTestFiles style test exp = do
  testContents <- readFile test
  expContents <- readFile exp
  let testDecls = parsePieces testContents
      expDecls = parsePieces expContents
  return $ describe ("hindent applied to chunks in " ++ test) $ foldl1 (>>) $ zipWith (mkSpec style) testDecls expDecls

mkSpec :: HIndent.Style -> String -> String -> Spec
mkSpec style input desired = it "works" $
  case HIndent.reformat style (L.pack input) of
    Left err      -> expectationFailure ("Error: " ++ err)
    Right builder -> L.unpack (L.toLazyText builder) `shouldBe` desired

parsePieces :: String -> [String]
parsePieces str = map (intercalate "\n") pieces
  where
    pieces = unfoldr unfolder (lines str)

    unfolder :: [String] -> Maybe ([String], [String])
    unfolder [] = Nothing
    unfolder remaining = Just $
     case break null remaining of
       (nonNull, [])     -> (nonNull, [])
       (nonNull, _:rest) -> (nonNull, rest)
