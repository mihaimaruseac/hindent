{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Annotation
  ( Annotation
  , mkAnnotation
  ) where

import HIndent.Ast.Declaration.Annotation.Provenance
import {-# SOURCE #-} HIndent.Ast.Expression
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Annotation = Annotation
  { provenance :: Provenance
  , expr :: GHC.LHsExpr GHC.GhcPs
  }

instance CommentExtraction Annotation where
  nodeComments Annotation {} = NodeComments [] [] []

instance Pretty Annotation where
  pretty' Annotation {..} =
    spaced
      [ string "{-# ANN"
      , pretty provenance
      , pretty $ fmap mkExpression expr
      , string "#-}"
      ]

mkAnnotation :: GHC.AnnDecl GHC.GhcPs -> Annotation
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkAnnotation (GHC.HsAnnotation _ prov expr) =
  Annotation {provenance = mkProvenance prov, ..}
#else
mkAnnotation (GHC.HsAnnotation _ _ prov expr) =
  Annotation {provenance = mkProvenance prov, ..}
#endif
