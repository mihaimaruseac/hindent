{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module HIndent.Pretty
  ( Pretty(..)
  , pretty
  , printCommentsAnd
  ) where

import qualified GHC.Types.SourceText as GHC
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments
import HIndent.Pretty.SigBindFamily
import HIndent.Pretty.Types
import HIndent.Printer

class CommentExtraction a =>
      Pretty a
  where
  pretty' :: a -> Printer ()

data MatchGroup body

pretty :: Pretty a => a -> Printer ()
printCommentsAnd ::
     (CommentExtraction l)
  => SrcLoc.GenLocated l e
  -> (e -> Printer ())
  -> Printer ()
instance (CommentExtraction l, Pretty e) => Pretty (SrcLoc.GenLocated l e)

instance Pretty GHC.EpaComment

instance Pretty
           (GHC.FamEqn
              GHC.GhcPs
              (SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))

instance Pretty SigBindFamily

instance Pretty
           (GHC.Match
              GHC.GhcPs
              (SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)))

instance Pretty
           (GHC.Match
              GHC.GhcPs
              (SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs)))

instance Pretty GHC.StringLiteral

instance Pretty Context

instance Pretty (GHC.DerivClauseTys GHC.GhcPs)

instance Pretty
           (GHC.HsScaled
              GHC.GhcPs
              (SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
instance Pretty
           (GHC.HsArg
              GHC.GhcPs
              (SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))
              (SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))
#else
instance Pretty
           (GHC.HsArg
              (SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))
              (SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))
#endif
instance Pretty (GHC.HsLit GHC.GhcPs)

instance Pretty (GHC.HsOverLit GHC.GhcPs)

instance Pretty (GHC.HsLocalBindsLR GHC.GhcPs GHC.GhcPs)

instance Pretty (GHC.HsValBindsLR GHC.GhcPs GHC.GhcPs)

instance Pretty QualifiedDo

instance Pretty (GHC.HsCmd GHC.GhcPs)

instance Pretty
           (GHC.StmtLR
              GHC.GhcPs
              GHC.GhcPs
              (SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)))

instance Pretty
           (GHC.StmtLR
              GHC.GhcPs
              GHC.GhcPs
              (SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs)))

instance Pretty
           (GHC.HsWildCardBndrs
              GHC.GhcPs
              (SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))

instance Pretty
           (GHC.HsRecFields
              GHC.GhcPs
              (SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)))

instance Pretty (GHC.DotFieldOcc GHC.GhcPs)

instance Pretty (GHC.HsCmdTop GHC.GhcPs)

instance Pretty (GHC.HsPragE GHC.GhcPs)
