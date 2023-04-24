{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

-- | Main entry point to hindent.
--
-- hindent
module Main
  ( main
  ) where

import HIndent
import System.Environment

-- | Main entry point.
main :: IO ()
main = getArgs >>= hindent
