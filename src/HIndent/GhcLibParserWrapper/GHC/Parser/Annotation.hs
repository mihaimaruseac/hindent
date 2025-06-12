{-# LANGUAGE CPP #-}

module HIndent.GhcLibParserWrapper.GHC.Parser.Annotation
  ( module GHC.Parser.Annotation
  , epaLocationToRealSrcSpan
  ) where

import GHC.Parser.Annotation
import GHC.Types.SrcLoc
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
epaLocationToRealSrcSpan :: EpaLocation' a -> RealSrcSpan
epaLocationToRealSrcSpan = epaLocationRealSrcSpan
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
epaLocationToRealSrcSpan :: EpaLocation' a -> RealSrcSpan
epaLocationToRealSrcSpan = anchor
#else
epaLocationToRealSrcSpan :: Anchor -> RealSrcSpan
epaLocationToRealSrcSpan = anchor
#endif
