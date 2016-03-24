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
                    [bgroup "BigDeclarations"
                            [bench ("HIndent.reformat: " ++
                                    show (styleName style))
                                   (nf (either error T.toLazyText .
                                        reformat style (Just defaultExtensions))
                                       bigDecls)|style <- styles]])]

-- | Setup the environment for the benchmarks.
setupEnv :: IO Text
setupEnv = do
  bigDecls <- LT.readFile "benchmarks/BigDeclarations.hs"
  let !decls = force bigDecls
  return decls
