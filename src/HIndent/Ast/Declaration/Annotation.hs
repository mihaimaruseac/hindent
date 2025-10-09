{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Annotation
  ( Annotation
  , mkAnnotation
  ) where

import HIndent.Ast.Declaration.Annotation.Provenance
import {-# SOURCE #-} HIndent.Ast.Expression (Expression, mkExpression)
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Annotation = Annotation
  { provenance :: Provenance
  , expr :: WithComments Expression
  }

instance CommentExtraction Annotation where
  nodeComments Annotation {} = NodeComments [] [] []

instance Pretty Annotation where
  pretty' Annotation {..} =
    spaced [string "{-# ANN", pretty provenance, pretty expr, string "#-}"]

mkAnnotation :: GHC.AnnDecl GHC.GhcPs -> Annotation
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkAnnotation (GHC.HsAnnotation _ prov expression) = Annotation {..}
  where
    provenance = mkProvenance prov
    expr = mkExpression <$> fromGenLocated expression
#else
mkAnnotation (GHC.HsAnnotation _ _ prov expression) = Annotation {..}
  where
    provenance = mkProvenance prov
    expr = mkExpression <$> fromGenLocated expression
#endif
