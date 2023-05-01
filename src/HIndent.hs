{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell indenter.
module HIndent
  ( hindent
  , -- * Formatting functions.
    reformat
  , prettyPrint
  , -- * Testing
    testAst
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char
import Data.List hiding (stripPrefix)
import Data.Maybe
import Data.Version
import Foreign.C
import GHC.IO.Exception
import GHC.Parser.Lexer hiding (buffer, options)
import GHC.Types.SrcLoc
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

-- | Runs HIndent with the given commandline options.
hindent :: [String] -> IO ()
hindent args = do
  config <- getConfig
  runMode <-
    handleParseResult $
    execParserPure
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
               (either (error . prettyParseError) id .
                reformat style exts Nothing)
        else forM_ paths $ \filepath -> do
               cabalexts <- getCabalExtensionsForSourcePath filepath
               text <- S.readFile filepath
               case reformat style (cabalexts ++ exts) (Just filepath) text of
                 Left e -> error $ prettyParseError e
                 Right out ->
                   unless (text == out) $
                   case action of
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
                             if ioe_errno e == Just ((\(Errno a) -> a) eXDEV)
                               then IO.copyFile fp filepath >> IO.removeFile fp
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
reformat config mexts mfilepath =
  preserveTrailingNewline
    (fmap (mconcat . intersperse "\n") . mapM processBlock . cppSplitBlocks)
  where
    processBlock :: CodeBlock -> Either ParseError ByteString
    processBlock (Shebang text) = Right text
    processBlock (CPPDirectives text) = Right text
    processBlock (HaskellSource yPos text) =
      let ls = S8.lines text
          prefix = findPrefix ls
          code = unlines' (map (stripPrefix prefix) ls)
          allExts =
            CE.uniqueExtensions $
            mexts ++
            configExtensions config ++
            collectLanguageExtensionsFromSource (UTF8.toString code)
       in case parseModule mfilepath allExts (UTF8.toString code) of
            POk _ m ->
              Right $
              addPrefix prefix $
              L.toStrict $ S.toLazyByteString $ prettyPrint config m
            PFailed st ->
              let rawErrLoc = psRealLoc $ loc st
               in Left $
                  ParseError
                    { errorLine = srcLocLine rawErrLoc + yPos
                    , errorCol = srcLocCol rawErrLoc
                    , errorFile = fromMaybe "<interactive>" mfilepath
                    }
    unlines' = S.concat . intersperse "\n"
    addPrefix :: ByteString -> ByteString -> ByteString
    addPrefix prefix = unlines' . map (prefix <>) . S8.lines
    stripPrefix :: ByteString -> ByteString -> ByteString
    stripPrefix prefix =
      fromMaybe (error "Missing expected prefix") . S.stripPrefix prefix
    findPrefix :: [ByteString] -> ByteString
    findPrefix = takePrefix False . findSmallestPrefix . dropNewlines
    dropNewlines :: [ByteString] -> [ByteString]
    dropNewlines = filter (not . S.null . S8.dropWhile (== '\n'))
    takePrefix :: Bool -> ByteString -> ByteString
    takePrefix bracketUsed txt =
      case (S8.uncons txt, bracketUsed) of
        (Just ('>', txt'), False) -> S8.cons '>' (takePrefix True txt')
        (Just (c, txt'), _)
          | c == ' ' || c == '\t' -> S8.cons c (takePrefix bracketUsed txt')
        _ -> ""
    findSmallestPrefix :: [ByteString] -> ByteString
    findSmallestPrefix [] = ""
    findSmallestPrefix ("":_) = ""
    findSmallestPrefix (p:ps) =
      let first = S8.head p
          startsWithChar c x = S8.length x > 0 && S8.head x == c
       in if all (startsWithChar first) ps
            then S8.cons first (findSmallestPrefix (S.tail p : map S.tail ps))
            else ""
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

-- | Generate an AST from the given module for debugging.
testAst :: ByteString -> Either ParseError HsModule'
testAst x =
  case parseModule Nothing exts (UTF8.toString x) of
    POk _ m -> Right $ modifyASTForPrettyPrinting m
    PFailed st ->
      Left $
      ParseError <$> srcLocLine <*> srcLocCol <*> pure "<interactive>" $
      psRealLoc $ loc st
  where
    exts =
      CE.uniqueExtensions $
      collectLanguageExtensionsFromSource $ UTF8.toString x

-- | Does the strict bytestring have a trailing newline?
hasTrailingLine :: ByteString -> Bool
hasTrailingLine xs = not (S8.null xs) && S8.last xs == '\n'

-- | Print the module.
prettyPrint :: Config -> HsModule' -> Builder
prettyPrint config m =
  runPrinterStyle config (pretty $ modifyASTForPrettyPrinting m)

-- | Pretty print the given printable thing.
runPrinterStyle :: Config -> Printer () -> Builder
runPrinterStyle config m =
  maybe (error "Printer failed with mzero call.") psOutput $
  execStateT (runPrinter m) initState
  where
    initState =
      PrintState
        { psIndentLevel = 0
        , psOutput = mempty
        , psNewline = False
        , psColumn = 0
        , psLine = 1
        , psConfig = config
        , psFitOnOneLine = False
        , psEolComment = False
        }
