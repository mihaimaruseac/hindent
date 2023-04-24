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
import System.Environment
import System.Exit (exitWith)
import qualified System.IO as IO

-- | Main entry point.
main :: IO ()
main = getArgs >>= hindent
