{-# LANGUAGE BangPatterns #-}

-- | Benchmark the pretty printer.

module Main where

import           Control.DeepSeq
import           Criterion
import           Criterion.Main
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Builder as T
import           HIndent

-- | Main benchmarks.
main :: IO ()
main =
  defaultMain
    [env setupEnv
         (\ ~bigDecls ->
             bgroup "Main"
                    [bench "HIndent.reformat big declarations: "
                                   (nf (either error T.toLazyText .
                                        reformat johanTibell (Just defaultExtensions))
                                       bigDecls)])]

-- | Setup the environment for the benchmarks.
setupEnv :: IO Text
setupEnv = do
  bigDecls <- LT.readFile "benchmarks/BigDeclarations.hs"
  let !decls = force bigDecls
  return decls
