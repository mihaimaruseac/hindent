{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

-- | Main entry point to hindent.
--
-- hindent
module Main
  ( main
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe
import qualified Data.Text as T
import Data.Version (showVersion)
import Foreign.C.Error
import GHC.IO.Exception
import HIndent
import HIndent.CabalFile
import HIndent.CommandlineOptions
import HIndent.Config
import HIndent.LanguageExtension
import HIndent.LanguageExtension.Types
import Options.Applicative hiding (action, style)
import Paths_hindent (version)
import qualified System.Directory as IO
import System.Exit (exitWith)
import qualified System.IO as IO

-- | Main entry point.
main :: IO ()
main = do
  config <- getConfig
  runMode <-
    execParser
      (info
         (options config <**> helper)
         (header "hindent - Reformat Haskell source code"))
  case runMode of
    ShowVersion -> putStrLn ("hindent " ++ showVersion version)
    Run style exts action paths ->
      if null paths
        then L8.interact
               (either error S.toLazyByteString .
                reformat style (Just exts) Nothing . L8.toStrict)
        else forM_ paths $ \filepath -> do
               cabalexts <- getCabalExtensionsForSourcePath filepath
               text <- S.readFile filepath
               case reformat
                      style
                      (Just $ cabalexts ++ exts)
                      (Just filepath)
                      text of
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
