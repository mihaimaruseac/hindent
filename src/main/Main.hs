{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Main entry point to hindent.
--
-- hindent

module Main where

import           HIndent

import           Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as T
import           Data.Version (showVersion)
import           Descriptive
import           Descriptive.Options
import           Paths_hindent (version)
import           System.Environment

-- | Main entry point.
main :: IO ()
main =
  do args <- getArgs
     case consume options (map T.pack args) of
       Left{} ->
         error (T.unpack (textDescription (describe options [])))
       Right result ->
         case result of
           Left{} -> putStrLn ("hindent " ++ showVersion version)
           Right style -> T.interact
                            (either error T.toLazyText .
                             reformat style)

-- | Program options.
options :: Consumer [Text] Option (Either Text (Style))
options =
  fmap Left (constant "--version") <|>
  (fmap Right
        (constant "--style" *>
         foldr1 (<|>)
                (map (\style ->
                        fmap (const style)
                             (constant (styleName style)))
                     styles)))
