{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.Pragmatic
  ( ExpressionPragma
  , mkExpressionPragma
  ) where

import qualified GHC.Hs as GHC
import qualified GHC.Types.SourceText as GHC
import HIndent.Ast.NodeComments (NodeComments(..))
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
import HIndent.Ast.WithComments (WithComments, addComments, mkWithComments)
#else
import HIndent.Ast.WithComments (WithComments, fromEpAnn)
#endif
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators (spaced, string)
import HIndent.Pretty.NodeComments (CommentExtraction(..))

newtype ExpressionPragma = SccPragma
  { label :: GHC.StringLiteral
  }

instance CommentExtraction ExpressionPragma where
  nodeComments _ = NodeComments [] [] []

instance Pretty ExpressionPragma where
  pretty' SccPragma {..} = spaced [string "{-# SCC", pretty label, string "#-}"]

mkExpressionPragma :: GHC.HsPragE GHC.GhcPs -> WithComments ExpressionPragma
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExpressionPragma (GHC.HsPragSCC (ann, _) literal) =
  addComments (nodeComments ann) $ mkWithComments SccPragma {label = literal}
#elif MIN_VERSION_ghc_lib_parser(9, 8, 1)
mkExpressionPragma (GHC.HsPragSCC (ann, _) literal) =
  fromEpAnn ann SccPragma {label = literal}
#else
mkExpressionPragma (GHC.HsPragSCC ann _ literal) =
  fromEpAnn ann SccPragma {label = literal}
#endif
