{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test the pretty printer.
module Main where

import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import qualified Data.ByteString            as S
import qualified Data.ByteString.Builder    as S
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8  as LUTF8
import qualified Data.ByteString.UTF8       as UTF8
import           Data.Function
import           Data.Version
import qualified HIndent
import           HIndent.CodeBlock
import qualified HIndent.LanguageExtension  as HIndent
import           HIndent.Types
import           Markdone
import qualified System.Info
import           Test.Hspec
import           Text.Read
import           Text.Regex.TDFA

-- | Main benchmarks.
main :: IO ()
main = do
  bytes <- S.readFile "TESTS.md"
  forest <- parse (tokenize bytes)
  hspec $ do
    codeBlocksSpec
    markdoneSpec
    toSpec forest

reformat :: Config -> S.ByteString -> ByteString
reformat cfg code =
  either (("-- " <>) . L8.pack) S.toLazyByteString $
  HIndent.reformat cfg (Just HIndent.defaultExtensions) Nothing code

-- | Convert the Markdone document to Spec benchmarks.
toSpec :: [Markdone] -> Spec
toSpec = go
  where
    cfg = HIndent.Types.defaultConfig {configTrailingNewline = False}
    go (Section name children:next) = do
      describe (UTF8.toString name) (go children)
      go next
    go (PlainText desc:CodeFence lang code:next) =
      case lang of
        "haskell" -> do
          it (UTF8.toString desc) $
            shouldBeReadable (reformat cfg code) (L.fromStrict code)
          go next
        "haskell 4" -> do
          let cfg' = cfg {configIndentSpaces = 4}
          it (UTF8.toString desc) $
            shouldBeReadable (reformat cfg' code) (L.fromStrict code)
          go next
        "haskell given" ->
          case skipEmptyLines next of
            CodeFence "haskell expect" codeExpect:next' -> do
              it (UTF8.toString desc) $
                shouldBeReadable (reformat cfg code) (L.fromStrict codeExpect)
              go next'
            _ ->
              error
                "'haskell given' block must be followed by a 'haskell expect' block"
        "haskell pending" -> do
          it (UTF8.toString desc) pending
          go next
        s
          | Just from <- fromVersion $ UTF8.toString s ->
            if compilerVersion >= from
              then do
                it (UTF8.toString desc) $
                  shouldBeReadable (reformat cfg code) (L.fromStrict code)
                go next
              else do
                it
                  (UTF8.toString desc)
                  (pendingWith $ pendingForVersionMsg from)
                go next
        _ -> go next
    go (PlainText {}:next) = go next
    go (CodeFence {}:next) = go next
    go [] = return ()
    pendingForVersionMsg from =
      "The test is for GHC versions since " ++
      showVersion from ++
      " but you are using GHC version " ++ showVersion compilerVersion ++ "."
    fromVersion :: String -> Maybe Version
    fromVersion s
      | (_, _, _, [x, y, z]) <-
          s =~ fromRegex :: (String, String, String, [String])
      , (Just x', Just y', Just z') <- (readMaybe x, readMaybe y, readMaybe z) =
        Just $ Version [x', y', z'] []
    fromVersion _ = Nothing
    fromRegex :: String
    fromRegex = "haskell since ([0-9]+)\\.([0-9]+)\\.([0-9]+)"

-- | Version of 'shouldBe' that prints strings in a readable way,
-- better for our use-case.
shouldBeReadable :: ByteString -> ByteString -> Expectation
shouldBeReadable x y =
  shouldBe (Readable x (Just (diff y x))) (Readable y Nothing)

-- | Prints a string without quoting and escaping.
data Readable =
  Readable
    { readableString :: ByteString
    , readableDiff   :: Maybe String
    }

instance Eq Readable where
  (==) = on (==) readableString

instance Show Readable where
  show (Readable x d') =
    "\n" ++
    LUTF8.toString x ++
    (case d' of
       Just d  -> "\nThe diff:\n" ++ d
       Nothing -> "")

-- | A diff display.
diff :: ByteString -> ByteString -> String
diff x y = ppDiff (on getGroupedDiff (lines . LUTF8.toString) x y)

skipEmptyLines :: [Markdone] -> [Markdone]
skipEmptyLines (PlainText "":rest) = rest
skipEmptyLines other               = other

codeBlocksSpec :: Spec
codeBlocksSpec =
  describe "splitting source into code blocks" $ do
    it "should put just Haskell code in its own block" $ do
      let input = "this is totally haskell code\n\nit deserves its own block!\n"
      cppSplitBlocks input `shouldBe` [HaskellSource 0 input]
    it "should put #if/#endif and Haskell code into separate blocks" $ do
      cppSplitBlocks
        "haskell code\n#if DEBUG\ndebug code\n#endif\nmore haskell code\n" `shouldBe`
        [ HaskellSource 0 "haskell code"
        , CPPDirectives "#if DEBUG"
        , HaskellSource 2 "debug code"
        , CPPDirectives "#endif"
        , HaskellSource 4 "more haskell code\n"
        ]
    it "should put the shebang line into its own block" $ do
      cppSplitBlocks
        "#!/usr/bin/env runhaskell\n{-# LANGUAGE OverloadedStrings #-}\n" `shouldBe`
        [ Shebang "#!/usr/bin/env runhaskell"
        , HaskellSource 1 "{-# LANGUAGE OverloadedStrings #-}\n"
        ]
    it "should put a multi-line #define into its own block" $ do
      let input = "#define A \\\n  macro contents \\\n  go here\nhaskell code\n"
      cppSplitBlocks input `shouldBe`
        [ CPPDirectives "#define A \\\n  macro contents \\\n  go here"
        , HaskellSource 3 "haskell code\n"
        ]
    it "should put an unterminated multi-line #define into its own block" $ do
      cppSplitBlocks "#define A \\" `shouldBe` [CPPDirectives "#define A \\"]
      cppSplitBlocks "#define A \\\n" `shouldBe` [CPPDirectives "#define A \\"]
      cppSplitBlocks "#define A \\\n.\\" `shouldBe`
        [CPPDirectives "#define A \\\n.\\"]

markdoneSpec :: Spec
markdoneSpec = do
  describe "markdown tokenizer" $ do
    it "should tokenize plain text" $ do
      let input =
            "this is a line\nthis is another line\n\nthis is a new paragraph\n"
      tokenize input `shouldBe`
        [ PlainLine "this is a line"
        , PlainLine "this is another line"
        , PlainLine ""
        , PlainLine "this is a new paragraph"
        ]
    it "should tokenize headings" $ do
      tokenize "# Heading" `shouldBe` [Heading 1 "Heading"]
    it "should tokenize code fence beginnings with labels" $ do
      tokenize "``` haskell\n" `shouldBe` [BeginFence "haskell"]
      tokenize "```haskell expect\n" `shouldBe` [BeginFence "haskell expect"]
      tokenize "before\n```code\nafter\n" `shouldBe`
        [PlainLine "before", BeginFence "code", PlainLine "after"]
    it "should tokenize full code fences" $ do
      tokenize "```haskell\ncode goes here\n```" `shouldBe`
        [BeginFence "haskell", PlainLine "code goes here", EndFence]
    it "should tokenize lines inside code fences as plain text" $ do
      tokenize "```haskell\n#!/usr/bin/env stack\n```" `shouldBe`
        [BeginFence "haskell", PlainLine "#!/usr/bin/env stack", EndFence]
      tokenize "```haskell\n# not a heading\n```" `shouldBe`
        [BeginFence "haskell", PlainLine "# not a heading", EndFence]
  describe "markdown parser" $ do
    it "should parse a heading followed by text as a section" $ do
      let input =
            [ Heading 1 "This is a heading"
            , PlainLine "This is plain text"
            , PlainLine "split across two lines."
            ]
      output <- parse input
      output `shouldBe`
        [ Section
            "This is a heading"
            [PlainText "This is plain text\nsplit across two lines."]
        ]

-- | Returns the version of the compiler used to build this program.
--
-- This function is a wrapper for `compilerVersion` and
-- `fullCompilerVersion` defined in `System.info`. If the `base` package
-- 4.15.0.0 or later is used, the latter function is defined and hence the
-- value is returned, otherwise the former value is returned.
compilerVersion :: Version
#if MIN_VERSION_base(4,15,0)
compilerVersion = System.Info.fullCompilerVersion
#else
compilerVersion = System.Info.compilerVersion
#endif
