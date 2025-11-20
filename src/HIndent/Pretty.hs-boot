{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module HIndent.Pretty
  ( Pretty(..)
  , pretty
  , printCommentsAnd
  ) where

import qualified GHC.Types.SourceText as GHC
import qualified GHC.Types.SrcLoc as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments
import HIndent.GhcOrdered.BindGroupElement
import HIndent.Pretty.Types
import HIndent.Printer

class CommentExtraction a =>
      Pretty a
  where
  pretty' :: a -> Printer ()

pretty :: Pretty a => a -> Printer ()
printCommentsAnd ::
     (CommentExtraction l)
  => GHC.GenLocated l e
  -> (e -> Printer ())
  -> Printer ()
instance (CommentExtraction l, Pretty e) => Pretty (GHC.GenLocated l e)

instance Pretty GHC.EpaComment

instance Pretty
           (GHC.FamEqn
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))

instance Pretty BindGroupElement

instance Pretty DataFamInstDecl'

instance Pretty GHC.StringLiteral

instance Pretty Context

instance Pretty
           (GHC.HsScaled
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
instance Pretty
           (GHC.HsArg
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))
#else
instance Pretty
           (GHC.HsArg
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))
#endif
instance Pretty (GHC.HsLit GHC.GhcPs)

instance Pretty (GHC.HsOverLit GHC.GhcPs)

instance Pretty QualifiedDo

instance Pretty
           (GHC.StmtLR
              GHC.GhcPs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)))

instance Pretty
           (GHC.HsWildCardBndrs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))
