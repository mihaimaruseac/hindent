module Main (main) where

import           Control.Monad
import           Control.Applicative
import           Data.List (find, unfoldr, break, isPrefixOf, intercalate)

import           Test.Hspec
import           System.Directory
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L

import qualified HIndent

styles :: [FilePath]
styles = ["fundamental", "gibiansky", "chris-done", "johan-tibell"]

testDir, expectedDir :: String
testDir = "tests"
expectedDir = "expected"

main :: IO ()
main = forM_ styles testStyle

testStyle :: FilePath -> IO ()
testStyle style = do
  let Just theStyle = find ((== T.pack style) . HIndent.styleName) HIndent.styles
  testFilenames <- filter (not . isPrefixOf ".") <$> getDirectoryContents tests
  let expFiles = map ((expected ++) . expectedFilename) testFilenames
      testFiles = map (tests ++) testFilenames
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
  when (length testDecls /= length expDecls) $
    error $ concat
              [ "Mismatched number of pieces in files "
              , test
              , " ("
              , show $ length testDecls
              , ")"
              , " and "
              , exp
              , " ("
              , show $ length expDecls
              , ")"
              ]
  return $ describe ("hindent applied to chunks in " ++ test) $ foldl1 (>>) $ zipWith (mkSpec style)
                                                                                testDecls expDecls

mkSpec :: HIndent.Style -> String -> String -> Spec
mkSpec style input desired = it "works" $
  case HIndent.reformat style Nothing (L.pack input) of
    Left err      -> expectationFailure ("Error: " ++ err)
    Right builder -> L.unpack (L.toLazyText builder) `shouldBe` desired

parsePieces :: String -> [String]
parsePieces str = map (intercalate "\n" . map mkNewlines) pieces
  where
    pieces = unfoldr unfolder (lines str)

    unfolder :: [String] -> Maybe ([String], [String])
    unfolder [] = Nothing
    unfolder remaining = Just $
      case break pieceBreak (zip remaining (tail remaining ++ [""])) of
        (nonNull, [])     -> (map fst nonNull, [])
        (nonNull, _:rest) -> (map fst nonNull, map fst rest)

    pieceBreak :: (String, String) -> Bool
    pieceBreak ("", "") = error $ "Two consecutive line breaks in:\n" ++ str
    pieceBreak (line, next) = null line && head next /= ' '

    mkNewlines :: String -> String
    mkNewlines ">" = ""
    mkNewlines x = x
