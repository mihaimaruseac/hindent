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
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Unsafe as S
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
import HIndent.GhcLibParserWrapper.GHC.Hs
import HIndent.LanguageExtension
import qualified HIndent.LanguageExtension.Conversion as CE
import HIndent.LanguageExtension.Types
import HIndent.ModulePreprocessing
import HIndent.Parse
import HIndent.Pretty
import HIndent.Printer
import Options.Applicative hiding (action, style)
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
        then L8.interact
               (either error S.toLazyByteString .
                reformat style exts Nothing . L8.toStrict)
        else forM_ paths $ \filepath -> do
               cabalexts <- getCabalExtensionsForSourcePath filepath
               text <- S.readFile filepath
               case reformat style (cabalexts ++ exts) (Just filepath) text of
                 Left e -> error e
                 Right out ->
                   unless (L8.fromStrict text == S.toLazyByteString out) $
                   case action of
                     Validate -> do
                       IO.putStrLn $ filepath ++ " is not formatted"
                       exitWith (ExitFailure 1)
                     Reformat -> do
                       tmpDir <- IO.getTemporaryDirectory
                       (fp, h) <- IO.openTempFile tmpDir "hindent.hs"
                       L8.hPutStr h (S.toLazyByteString out)
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
  -> Either String Builder
reformat config mexts mfilepath =
  preserveTrailingNewline
    (fmap (mconcat . intersperse "\n") . mapM processBlock . cppSplitBlocks)
  where
    processBlock :: CodeBlock -> Either String Builder
    processBlock (Shebang text) = Right $ S.byteString text
    processBlock (CPPDirectives text) = Right $ S.byteString text
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
              S.lazyByteString $
              addPrefix prefix $ S.toLazyByteString $ prettyPrint config m
            PFailed st ->
              let rawErrLoc = psRealLoc $ loc st
                  adjustedLoc =
                    (srcLocLine rawErrLoc + yPos, srcLocCol rawErrLoc)
               in Left $ "Parse failed near " ++ show adjustedLoc
    unlines' = S.concat . intersperse "\n"
    unlines'' = L.concat . intersperse "\n"
    addPrefix :: ByteString -> L8.ByteString -> L8.ByteString
    addPrefix prefix = unlines'' . map (L8.fromStrict prefix <>) . L8.lines
    stripPrefix :: ByteString -> ByteString -> ByteString
    stripPrefix prefix line =
      if S.null (S8.dropWhile (== '\n') line)
        then line
        else fromMaybe (error "Missing expected prefix") . s8_stripPrefix prefix $
             line
    findPrefix :: [ByteString] -> ByteString
    findPrefix = takePrefix False . findSmallestPrefix . dropNewlines
    dropNewlines :: [ByteString] -> [ByteString]
    dropNewlines = filter (not . S.null . S8.dropWhile (== '\n'))
    takePrefix :: Bool -> ByteString -> ByteString
    takePrefix bracketUsed txt =
      case S8.uncons txt of
        Nothing -> ""
        Just ('>', txt') ->
          if not bracketUsed
            then S8.cons '>' (takePrefix True txt')
            else ""
        Just (c, txt') ->
          if c == ' ' || c == '\t'
            then S8.cons c (takePrefix bracketUsed txt')
            else ""
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
             if hasTrailingLine (L.toStrict (S.toLazyByteString x'))
               then x'
               else x' <> "\n")
          (f x)
      | otherwise = f x

-- | Generate an AST from the given module for debugging.
testAst :: ByteString -> Either String HsModule'
testAst x =
  case parseModule Nothing exts (UTF8.toString x) of
    POk _ m -> Right $ modifyASTForPrettyPrinting m
    PFailed st ->
      Left $
      "Parse failed near " ++
      show ((,) <$> srcLocLine <*> srcLocCol $ psRealLoc $ loc st)
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
  maybe
    (error "Printer failed with mzero call.")
    psOutput
    (execStateT
       (runPrinter m)
       (PrintState
          { psIndentLevel = 0
          , psOutput = mempty
          , psNewline = False
          , psColumn = 0
          , psLine = 1
          , psConfig = config
          , psFitOnOneLine = False
          , psEolComment = False
          }))

s8_stripPrefix :: ByteString -> ByteString -> Maybe ByteString
s8_stripPrefix bs1@(S.PS _ _ l1) bs2
  | bs1 `S.isPrefixOf` bs2 = Just (S.unsafeDrop l1 bs2)
  | otherwise = Nothing
