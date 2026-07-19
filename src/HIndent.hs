{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell indenter.
module HIndent
  ( -- * The entry point.
    hindent
  , -- * Formatting functions.
    reformat
  , -- * Config
    Config(..)
  , defaultConfig
  , getConfig
  , -- * Extension
    Extension(..)
  , defaultExtensions
  , -- * Error
    ParseError(..)
  , prettyParseError
  , -- * Testing
    testAst
  , HsModule'
  ) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char
import Data.Maybe
import Data.Version
import Foreign.C
import GHC.IO.Exception
import GHC.Parser.Lexer hiding (buffer, options)
import GHC.Types.SrcLoc
import HIndent.Ast
import HIndent.ByteString
import HIndent.CabalFile
import HIndent.CodeBlock
import HIndent.CommandlineOptions
import HIndent.Config
import HIndent.Error
import HIndent.GhcLibParserWrapper.GHC.Hs
import HIndent.LanguageExtension
import qualified HIndent.LanguageExtension.Conversion as CE
import HIndent.LanguageExtension.Types
import HIndent.ModulePreprocessing
import HIndent.Parse
import HIndent.Pretty
import HIndent.Printer
import Options.Applicative hiding (ParseError, action, style)
import Paths_hindent
import qualified System.Directory as IO
import System.Exit
import qualified System.IO as IO

data IndentationState = IndentationState
  { adjustment :: Int
  , branches :: [Int]
  }

data CPPBoundary
  = BranchStart
  | BranchAlternative
  | BranchEnd
  | OtherDirective

-- | Runs HIndent with the given commandline options.
hindent :: [String] -> IO ()
hindent args = do
  config <- getConfig
  runMode <-
    handleParseResult
      $ execParserPure
          defaultPrefs
          (info
             (options config <**> helper)
             (header "hindent - Reformat Haskell source code"))
          args
  case runMode of
    ShowVersion -> putStrLn ("hindent " ++ showVersion version)
    Run style exts action paths ->
      if null paths
        then S8.interact
               (either (error . prettyParseError) id
                  . reformat style exts Nothing)
        else forConcurrently_ paths $ \filepath -> do
               cabalexts <- getCabalExtensionsForSourcePath filepath
               text <- S.readFile filepath
               case reformat style (cabalexts ++ exts) (Just filepath) text of
                 Left e -> error $ prettyParseError e
                 Right out ->
                   unless (text == out)
                     $ case action of
                         Validate -> do
                           IO.putStrLn $ filepath ++ " is not formatted"
                           exitWith (ExitFailure 1)
                         Reformat -> do
                           tmpDir <- IO.getTemporaryDirectory
                           (fp, h) <- IO.openTempFile tmpDir "hindent.hs"
                           S8.hPutStr h out
                           IO.hFlush h
                           IO.hClose h
                           let exdev e =
                                 if ioe_errno e
                                      == Just ((\(Errno a) -> a) eXDEV)
                                   then IO.copyFile fp filepath
                                          >> IO.removeFile fp
                                   else throw e
                           IO.copyPermissions filepath fp
                           IO.renameFile fp filepath `catch` exdev

-- | Format the given source.
reformat ::
     Config
  -> [Extension]
  -> Maybe FilePath
  -> ByteString
  -> Either ParseError ByteString
reformat config mexts mfilepath rawCode =
  preserveTrailingNewline
    (fmap unlines' . processBlocks (IndentationState 0 []) . cppSplitBlocks)
    rawCode
  where
    processBlocks ::
         IndentationState -> [CodeBlock] -> Either ParseError [ByteString]
    processBlocks _ [] = Right []
    processBlocks state (block:blocks) = do
      (formatted, nextState) <- processBlock state block
      remaining <- processBlocks nextState blocks
      Right (formatted : remaining)
    processBlock ::
         IndentationState
      -> CodeBlock
      -> Either ParseError (ByteString, IndentationState)
    processBlock state (Shebang text) = Right (text, state)
    processBlock state (LinePragma text) = Right (text, state)
    processBlock state (CPPDirectives text) =
      Right (text, foldl updateIndentationState state (S8.lines text))
    processBlock state@IndentationState {..} (HaskellSource yPos text) =
      let ls = S8.lines text
          originalPrefix = findPrefix ls
          prefix = adjustPrefix adjustment originalPrefix
          code = unlines' (map stripPrefixIfNotNull ls)
          stripPrefixIfNotNull s =
            if S.null s
              then s
              else stripPrefix originalPrefix s
       in case parseModule mfilepath allExts (UTF8.toString code) of
            POk _ m ->
              let formatted =
                    addPrefix prefix
                      $ L.toStrict
                      $ S.toLazyByteString
                      $ prettyPrint config m
                  nextAdjustment =
                    fromMaybe adjustment
                      $ (-)
                          <$> lastIndentation formatted
                          <*> lastIndentation text
               in Right (formatted, state {adjustment = nextAdjustment})
            PFailed st ->
              let rawErrLoc = psRealLoc $ loc st
               in Left
                    $ ParseError
                        { errorLine = srcLocLine rawErrLoc + yPos
                        , errorCol = srcLocCol rawErrLoc
                        , errorFile = fromMaybe "<interactive>" mfilepath
                        }
    updateIndentationState :: IndentationState -> ByteString -> IndentationState
    updateIndentationState state@IndentationState {..} directive =
      case classifyCPPBoundary directive of
        BranchStart -> state {branches = adjustment : branches}
        BranchAlternative ->
          state {adjustment = fromMaybe adjustment (listToMaybe branches)}
        BranchEnd ->
          case branches of
            branchAdjustment:remainingBranches ->
              IndentationState branchAdjustment remainingBranches
            [] -> state
        OtherDirective -> state
    classifyCPPBoundary :: ByteString -> CPPBoundary
    classifyCPPBoundary directive =
      case S8.takeWhile
             (not . isSpace)
             (S8.dropWhile isSpace (S8.drop 1 directive)) of
        "if" -> BranchStart
        "ifdef" -> BranchStart
        "ifndef" -> BranchStart
        "else" -> BranchAlternative
        "elif" -> BranchAlternative
        "endif" -> BranchEnd
        _ -> OtherDirective
    adjustPrefix :: Int -> ByteString -> ByteString
    adjustPrefix indentationAdjustment prefix = marker <> adjustedWhitespace
      where
        (marker, whitespace) =
          if ">" `S8.isPrefixOf` prefix
            then (">", S8.drop 1 prefix)
            else ("", prefix)
        adjustedLength = max 0 (S8.length whitespace + indentationAdjustment)
        adjustedWhitespace =
          if adjustedLength <= S8.length whitespace
            then S8.take adjustedLength whitespace
            else whitespace
                   <> S8.replicate (adjustedLength - S8.length whitespace) ' '
    lastIndentation :: ByteString -> Maybe Int
    lastIndentation =
      fmap indentation
        . listToMaybe
        . reverse
        . filter (not . S8.all isSpace)
        . S8.lines
      where
        indentation line =
          S8.length
            $ S8.takeWhile (\character -> character == ' ' || character == '\t')
            $ if ">" `S8.isPrefixOf` line
                then S8.drop 1 line
                else line
    preserveTrailingNewline f x
      | S8.null x || S8.all isSpace x = return mempty
      | hasTrailingLine x || configTrailingNewline config =
        fmap
          (\x' ->
             if hasTrailingLine x'
               then x'
               else x' <> "\n")
          (f x)
      | otherwise = f x
    allExts =
      CE.uniqueExtensions
        $ concatMap (\x -> x : extensionImplies x)
        $ mexts ++ configExtensions config ++ allExtsFromCode
    allExtsFromCode = concatMap f codeBlocks
      where
        f (HaskellSource _ text) =
          collectLanguageExtensionsFromSource $ UTF8.toString text
        f _ = []
    codeBlocks = cppSplitBlocks rawCode

-- | Generate an AST from the given module for debugging.
testAst :: ByteString -> Either ParseError HsModule'
testAst x =
  case parseModule Nothing exts (UTF8.toString x) of
    POk _ m -> Right $ modifyASTForPrettyPrinting m
    PFailed st ->
      Left
        $ ParseError <$> srcLocLine <*> srcLocCol <*> pure "<interactive>"
        $ psRealLoc
        $ loc st
  where
    exts =
      CE.uniqueExtensions
        $ collectLanguageExtensionsFromSource
        $ UTF8.toString x

-- | Print the module.
prettyPrint :: Config -> HsModule' -> Builder
prettyPrint config =
  runPrinterStyle config . pretty . mkModule . modifyASTForPrettyPrinting
