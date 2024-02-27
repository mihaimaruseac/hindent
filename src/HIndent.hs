{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
  , -- * Error
    ParseError(..)
  , prettyParseError
  , -- * Testing
    testAst
  , HsModule'
  ) where

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
        else forM_ paths $ \filepath -> do
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
    (fmap unlines' . mapM processBlock . cppSplitBlocks)
    rawCode
  where
    processBlock :: CodeBlock -> Either ParseError ByteString
    processBlock (Shebang text) = Right text
    processBlock (CPPDirectives text) = Right text
    processBlock (HaskellSource yPos text) =
      let ls = S8.lines text
          prefix = findPrefix ls
          code = unlines' (map stripPrefixIfNotNull ls)
          stripPrefixIfNotNull s =
            if S.null s
              then s
              else stripPrefix prefix s
       in case parseModule mfilepath allExts (UTF8.toString code) of
            POk _ m ->
              Right
                $ addPrefix prefix
                $ L.toStrict
                $ S.toLazyByteString
                $ prettyPrint config m
            PFailed st ->
              let rawErrLoc = psRealLoc $ loc st
               in Left
                    $ ParseError
                        { errorLine = srcLocLine rawErrLoc + yPos
                        , errorCol = srcLocCol rawErrLoc
                        , errorFile = fromMaybe "<interactive>" mfilepath
                        }
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
prettyPrint config m =
  runPrinterStyle config (pretty $ mkModule $ modifyASTForPrettyPrinting m)
