{-# LANGUAGE ViewPatterns #-}
-- | Main entry point to hindent.
--
-- hindent

module Main where

import           HIndent
import           HIndent.Types(Config(..))

import           Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as T
import           Data.Version (showVersion)
import           Paths_hindent (version)
import           System.Environment
import           Text.Read

-- | Main entry point.
main :: IO ()
main =
  do args <- getArgs
     case args of
       "--style":(findStyle -> Just style):opts ->
         T.interact
           (either error T.toLazyText .
            reformat (modifyStyle opts style))
       ["--version"] -> putStrLn $ "hindent " ++ showVersion version
       _ ->
         error ("arguments: --style [" ++
                intercalate "|"
                            (map (T.unpack . styleName) styles) ++
                "]")
  where 
    findStyle name = find ((== T.pack name) . styleName) styles

    modifyStyle [] style = style
    modifyStyle ("--line-length":(readMaybe -> Just maxCol):opts) style =
     (modifyStyle opts style) {styleDefConfig = (styleDefConfig style) {configMaxColumns = maxCol}}
    modifyStyle args _ = error $ "Unknown args: " ++ show args
