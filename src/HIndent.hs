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
import qualified GHC.Types.Name.Reader as GHC
import GHC.Types.SrcLoc
import qualified GHC.Types.SrcLoc as GHC
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

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC

-- | An internally formatted block and the declaration names needed when
-- joining it to adjacent CPP blocks.
data FormattedBlock = FormattedBlock
  { block :: CodeBlock
  , text :: ByteString
  , firstDefinition :: Maybe GHC.RdrName
  , lastSignature :: [GHC.RdrName]
  }

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
  preserveTrailingNewline (processBlocks . cppSplitBlocks) rawCode
  where
    processBlocks :: [CodeBlock] -> Either ParseError ByteString
    processBlocks blocks = joinBlocks <$> mapM processBlock blocks
    processBlock :: CodeBlock -> Either ParseError FormattedBlock
    processBlock block@(Shebang text) =
      Right FormattedBlock {firstDefinition = Nothing, lastSignature = [], ..}
    processBlock block@(LinePragma text) =
      Right FormattedBlock {firstDefinition = Nothing, lastSignature = [], ..}
    processBlock block@(CPPDirectives text) =
      Right FormattedBlock {firstDefinition = Nothing, lastSignature = [], ..}
    processBlock block@(HaskellSource yPos source) =
      let ls = S8.lines source
          prefix = findPrefix ls
          code = unlines' (map stripPrefixIfNotNull ls)
          stripPrefixIfNotNull s =
            if S.null s
              then s
              else stripPrefix prefix s
       in case parseModule mfilepath allExts (UTF8.toString code) of
            POk _ m ->
              let text =
                    addPrefix prefix
                      $ L.toStrict
                      $ S.toLazyByteString
                      $ prettyPrint config m
                  firstDefinition = moduleFirstDefinition m
                  lastSignature = moduleLastSignature m
               in Right FormattedBlock {..}
            PFailed st ->
              let rawErrLoc = psRealLoc $ loc st
               in Left
                    $ ParseError
                        { errorLine = srcLocLine rawErrLoc + yPos
                        , errorCol = srcLocCol rawErrLoc
                        , errorFile = fromMaybe "<interactive>" mfilepath
                        }
    joinBlocks :: [FormattedBlock] -> ByteString
    joinBlocks blocks = S.concat $ zipWith3 output before after blocks
      where
        boundaries = collectBoundaries Nothing blocks
        before = Nothing : map Just boundaries
        after = map Just boundaries ++ [Nothing]
        output previousBoundary nextBoundary FormattedBlock {..} =
          normalize
            (previousBoundary == Just True)
            (nextBoundary == Just True)
            text
            <> case nextBoundary of
                 Just True -> "\n\n"
                 Just False -> "\n"
                 Nothing -> mempty
        normalize stripStart stripEnd =
          (if stripEnd
             then S8.dropWhileEnd (== '\n')
             else id)
            . (if stripStart
                 then S8.dropWhile (== '\n')
                 else id)
        collectBoundaries _ [] = []
        collectBoundaries _ [_] = []
        collectBoundaries previous (current:remaining@(_:_)) =
          needsBlankLine previous current remaining
            : collectBoundaries (Just current) remaining
    needsBlankLine ::
         Maybe FormattedBlock -> FormattedBlock -> [FormattedBlock] -> Bool
    needsBlankLine _ FormattedBlock { block = HaskellSource {}
                                    , lastSignature = signature
                                    } (FormattedBlock {block = CPPDirectives directives}:following)
      | startsConditional directives =
        not
          $ signaturesMatchDefinition signature
          $ listToMaybe following >>= firstDefinition
    needsBlankLine (Just FormattedBlock {lastSignature = signature}) FormattedBlock {block = CPPDirectives directives} (FormattedBlock { block = HaskellSource {}
                                                                                                                                       , firstDefinition = definition
                                                                                                                                       }:_)
      | endsConditional directives =
        not $ signaturesMatchDefinition signature definition
    needsBlankLine _ _ _ = False
    signaturesMatchDefinition signatures (Just definition) =
      definition `elem` signatures
    signaturesMatchDefinition _ _ = False
    startsConditional :: ByteString -> Bool
    startsConditional =
      maybe False ("#if" `S8.isPrefixOf`) . listToMaybe . S8.lines
    endsConditional :: ByteString -> Bool
    endsConditional =
      maybe False ("#endif" `S8.isPrefixOf`) . listToMaybe . reverse . S8.lines
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

moduleFirstDefinition :: GHC.HsModule' -> Maybe GHC.RdrName
moduleFirstDefinition m =
  case GHC.hsmodDecls m of
    declaration:_ ->
      case GHC.unLoc declaration of
        GHC.ValD _ GHC.FunBind {GHC.fun_id = name} -> Just $ GHC.unLoc name
        _ -> Nothing
    [] -> Nothing

moduleLastSignature :: GHC.HsModule' -> [GHC.RdrName]
moduleLastSignature m =
  case reverse $ GHC.hsmodDecls m of
    declaration:_ ->
      case GHC.unLoc declaration of
        GHC.SigD _ (GHC.TypeSig _ names _) -> map GHC.unLoc names
        _ -> []
    [] -> []
