{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.Pragmatic
  ( ExpressionPragma
  , mkExpressionPragma
  ) where

import qualified GHC.Hs as GHC
import HIndent.Ast.TextValue
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
import HIndent.Ast.WithComments (WithComments, mkWithComments)
#else
import HIndent.Ast.WithComments (WithComments, fromEpAnn)
#endif
import HIndent.Pretty (Pretty(..))
import HIndent.Pretty.Combinators (spaced, string)

newtype ExpressionPragma = SccPragma
  { label :: TextValue
  }

instance Pretty ExpressionPragma where
  pretty SccPragma {..} = spaced [string "{-# SCC", pretty label, string "#-}"]

mkExpressionPragma :: GHC.HsPragE GHC.GhcPs -> WithComments ExpressionPragma
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
mkExpressionPragma (GHC.HsPragSCC (_, _) literal) =
  mkWithComments SccPragma {label = mkTextValueFromStringLiteral literal}
#else
mkExpressionPragma (GHC.HsPragSCC ann _ literal) =
  fromEpAnn ann SccPragma {label = mkTextValueFromStringLiteral literal}
#endif
