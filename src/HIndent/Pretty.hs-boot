{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

module HIndent.Pretty
  ( Pretty(..)
  , pretty
  , printCommentsAnd
  ) where

import           Data.Void
import qualified GHC.Core.Type                      as GHC
import qualified GHC.Types.Basic                    as GHC
import qualified GHC.Types.Name.Reader              as GHC
import qualified GHC.Types.SrcLoc                   as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHc
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.SigBindFamily
import           HIndent.Pretty.Types
import           HIndent.Printer

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

instance Pretty (GHC.HsType GHC.GhcPs)

instance Pretty
           (GHC.FamEqn
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))

instance Pretty GHC.RdrName

instance Pretty (GHC.ConDeclField GHC.GhcPs)

instance Pretty (GHC.HsOuterTyVarBndrs GHC.Specificity GHc.GhcPs)

instance Pretty SigBindFamily

instance Pretty InfixOp

instance Pretty GHC.OverlapMode

instance Pretty HsSigType'

instance Pretty
           (GHC.MatchGroup
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)))

instance Pretty (GHC.Pat GHC.GhcPs)

instance Pretty
           (GHc.GRHSs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)))

instance Pretty
           (GHC.HsConDetails
              Void
              (GHC.GenLocated GHC.SrcSpanAnnN GHC.RdrName)
              [GHC.RecordPatSynField GHC.GhcPs])

instance Pretty (GHC.HsPatSynDir GHC.GhcPs)

instance Pretty PatInsidePatDecl

instance Pretty (GHC.DefaultDecl GHc.GhcPs)

instance Pretty (GHC.ForeignDecl GHC.GhcPs)

instance Pretty (GHC.WarnDecls GHC.GhcPs)

instance Pretty (GHC.AnnDecl GHC.GhcPs)

instance Pretty (GHC.RuleDecls GHC.GhcPs)

instance Pretty (GHC.SpliceDecl GHC.GhcPs)

instance Pretty (GHC.RoleAnnotDecl GHC.GhcPs)

instance Pretty (GHC.HsSigType GHC.GhcPs)

instance Pretty Context

instance Pretty (GHC.DerivClauseTys GHC.GhcPs)

instance Pretty a => Pretty (GHC.HsScaled GHC.GhcPs a)
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
instance Pretty
           (GHC.HsArg
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))
#else
instance Pretty
           (GHC.HsArg
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHc.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))
#endif
