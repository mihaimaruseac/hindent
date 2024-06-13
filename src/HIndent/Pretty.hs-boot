{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

module HIndent.Pretty
  ( Pretty(..)
  , pretty
  , printCommentsAnd
  ) where

import           Data.Void
import qualified GHC.Types.Name                     as GHC
import qualified GHC.Types.Name.Reader              as GHC
import qualified GHC.Types.SourceText               as GHC
import qualified GHC.Types.SrcLoc                   as GHC
import qualified GHC.Unit                           as GHC
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

instance Pretty SigBindFamily

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

instance Pretty GHC.StringLiteral

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
instance Pretty (GHC.FieldOcc GHC.GhcPs)

instance Pretty GHC.OccName

instance Pretty GHC.HsIPName

instance Pretty (GHC.HsOverLit GHC.GhcPs)

instance Pretty (GHC.HsLit GHC.GhcPs)

instance Pretty LambdaCase

instance Pretty
           (GHC.HsWildCardBndrs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs)))

instance Pretty InfixApp

instance Pretty InfixExpr

instance Pretty (GHC.HsTupArg GHC.GhcPs)

instance Pretty
           (GHC.StmtLR
              GHC.GhcPs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)))

instance Pretty QualifiedDo

instance Pretty GRHSExpr

instance Pretty LetIn

instance Pretty ListComprehension

instance Pretty DoExpression

instance Pretty
           (GHC.HsRecFields
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)))

instance Pretty (GHC.AmbiguousFieldOcc GHC.GhcPs)

instance Pretty (GHC.FieldLabelStrings GHC.GhcPs)
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
instance Pretty (GHC.HsFieldLabel GHC.GhcPs)
#endif
instance Pretty (GHC.ArithSeqInfo GHC.GhcPs)

instance Pretty
           (GHC.StmtLR
              GHC.GhcPs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs)))

instance Pretty (GHC.HsCmdTop GHC.GhcPs)

instance Pretty (GHC.HsPragE GHC.GhcPs)

instance Pretty GHC.ModuleName

instance Pretty InfixExpr

instance Pretty QualifiedDo

instance Pretty
           (GHC.StmtLR
              GHC.GhcPs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)))
