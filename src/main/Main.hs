{-# LANGUAGE ViewPatterns #-}
-- | Main entry point to hindent.
--
-- hindent

module Main where

import           Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as T
import           HIndent
import           HIndent.Types
import           System.Environment

-- | Main entry point.
main :: IO ()
main =
  do args <- getArgs
     case args of
       ["--style",findStyle -> Just style] ->
         T.interact
           (either error T.toLazyText .
            reformat (styleDefConfig style) style)
       _ ->
         error ("arguments: --style [" ++
                intercalate "|" (map (T.unpack . styleName) styles) ++
                "]")
  where findStyle name =
          find ((== T.pack name) . styleName) styles
