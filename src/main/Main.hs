-- | Main entry point to hindent.
--
-- hindent

module Main where

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Builder as T
import           HIndent
import           System.Environment

-- | Main entry point.
main :: IO ()
main =
  do args <- getArgs
     case args of
       [] ->
         T.interact
           (either error T.toLazyText .
            reformat)
       (_:_) ->
         error "No arguments accepted at this time. Provide a declaration in STDIN."
