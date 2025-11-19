{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Pretty.NodeComments
  ( CommentExtraction(..)
  , emptyNodeComments
  ) where

import Data.Maybe
import Data.Void
import GHC.Data.BooleanFormula
import GHC.Hs
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import HIndent.Ast.NodeComments
import HIndent.Pretty.SigBindFamily
import HIndent.Pretty.Types
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
import GHC.Core.DataCon
#else
import GHC.Unit
#endif
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
import GHC.Stack
#endif
-- | An interface to extract comments from an AST node.
class CommentExtraction a where
  nodeComments :: a -> NodeComments

instance CommentExtraction l => CommentExtraction (GenLocated l e) where
  nodeComments (L l _) = nodeComments l

instance CommentExtraction (MatchGroup GhcPs a) where
  nodeComments MG {} = emptyNodeComments

instance CommentExtraction DoOrMdo where
  nodeComments = const emptyNodeComments

instance CommentExtraction QualifiedDo where
  nodeComments = const emptyNodeComments

instance CommentExtraction (HsSigType GhcPs) where
  nodeComments HsSig {} = emptyNodeComments

-- | For pattern matching.
instance CommentExtraction
           (HsRecFields GhcPs (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  nodeComments HsRecFields {} = emptyNodeComments

-- | For record updates
instance CommentExtraction
           (HsRecFields GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  nodeComments HsRecFields {} = emptyNodeComments

instance CommentExtraction (GRHSs GhcPs a) where
  nodeComments GRHSs {..} = NodeComments {..}
    where
      commentsBefore = priorComments grhssExt
      commentsOnSameLine = []
      commentsAfter = getFollowingComments grhssExt

instance CommentExtraction (ParStmtBlock GhcPs GhcPs) where
  nodeComments ParStmtBlock {} = emptyNodeComments

instance CommentExtraction RdrName where
  nodeComments Unqual {} = emptyNodeComments
  nodeComments Qual {} = emptyNodeComments
  nodeComments Orig {} = emptyNodeComments
  nodeComments Exact {} = emptyNodeComments

instance CommentExtraction EpaCommentTok where
  nodeComments = const emptyNodeComments

instance CommentExtraction (SpliceDecl GhcPs) where
  nodeComments SpliceDecl {} = emptyNodeComments

instance CommentExtraction SigBindFamily where
  nodeComments (Sig x) = nodeComments x
  nodeComments (Bind x) = nodeComments x
  nodeComments (Family x) = nodeComments x
  nodeComments (TyFamInst x) = nodeComments x
  nodeComments (DataFamInst x) = nodeComments x

instance CommentExtraction EpaComment where
  nodeComments EpaComment {} = emptyNodeComments

instance CommentExtraction SrcSpan where
  nodeComments RealSrcSpan {} = emptyNodeComments
  nodeComments UnhelpfulSpan {} = emptyNodeComments

-- HsConDeclH98Details
instance CommentExtraction
           (HsConDetails
              Void
              (HsScaled GhcPs (GenLocated SrcSpanAnnA (BangType GhcPs)))
              (GenLocated
                 SrcSpanAnnL
                 [GenLocated SrcSpanAnnA (ConDeclField GhcPs)])) where
  nodeComments PrefixCon {} = emptyNodeComments
  nodeComments RecCon {} = emptyNodeComments
  nodeComments InfixCon {} = emptyNodeComments

instance CommentExtraction (HsScaled GhcPs a) where
  nodeComments HsScaled {} = emptyNodeComments

instance CommentExtraction (BooleanFormula a) where
  nodeComments Var {} = emptyNodeComments
  nodeComments And {} = emptyNodeComments
  nodeComments Or {} = emptyNodeComments
  nodeComments Parens {} = emptyNodeComments

instance CommentExtraction (ImportDecl GhcPs) where
  nodeComments ImportDecl {..} = nodeComments ideclExt

instance CommentExtraction StringLiteral where
  nodeComments StringLiteral {} = emptyNodeComments

instance CommentExtraction (FamilyResultSig GhcPs) where
  nodeComments NoSig {} = emptyNodeComments
  nodeComments KindSig {} = emptyNodeComments
  nodeComments TyVarSig {} = emptyNodeComments

instance CommentExtraction Context where
  nodeComments Context {} = emptyNodeComments

instance CommentExtraction HorizontalContext where
  nodeComments HorizontalContext {} = emptyNodeComments

instance CommentExtraction VerticalContext where
  nodeComments VerticalContext {} = emptyNodeComments

instance CommentExtraction FamEqn' where
  nodeComments FamEqn' {..} = nodeComments famEqn

-- | 'Pretty' for 'LHsSigWcType GhcPs'.
instance CommentExtraction
           (HsWildCardBndrs GhcPs (GenLocated SrcSpanAnnA (HsSigType GhcPs))) where
  nodeComments HsWC {} = emptyNodeComments

-- | 'Pretty' for 'LHsWcType'
instance CommentExtraction
           (HsWildCardBndrs GhcPs (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  nodeComments HsWC {} = emptyNodeComments

instance CommentExtraction CExportSpec where
  nodeComments CExportStatic {} = emptyNodeComments

instance CommentExtraction TopLevelTyFamInstDecl where
  nodeComments (TopLevelTyFamInstDecl x) = nodeComments x

instance CommentExtraction (DataFamInstDecl GhcPs) where
  nodeComments DataFamInstDecl {} = emptyNodeComments

instance CommentExtraction DataFamInstDecl' where
  nodeComments DataFamInstDecl' {..} = nodeComments dataFamInstDecl

instance CommentExtraction (FixitySig GhcPs) where
  nodeComments FixitySig {} = emptyNodeComments

instance CommentExtraction Fixity where
  nodeComments Fixity {} = emptyNodeComments

instance CommentExtraction InlinePragma where
  nodeComments InlinePragma {} = emptyNodeComments

instance CommentExtraction (HsOverLit GhcPs) where
  nodeComments OverLit {} = emptyNodeComments

instance CommentExtraction OverLitVal where
  nodeComments HsIntegral {} = emptyNodeComments
  nodeComments HsFractional {} = emptyNodeComments
  nodeComments HsIsString {} = emptyNodeComments

instance CommentExtraction IntegralLit where
  nodeComments IL {} = emptyNodeComments

instance CommentExtraction FractionalLit where
  nodeComments FL {} = emptyNodeComments

instance CommentExtraction (HsLit GhcPs) where
  nodeComments HsChar {} = emptyNodeComments
  nodeComments HsCharPrim {} = emptyNodeComments
  nodeComments HsString {} = emptyNodeComments
  nodeComments HsStringPrim {} = emptyNodeComments
  nodeComments HsInt {} = emptyNodeComments
  nodeComments HsIntPrim {} = emptyNodeComments
  nodeComments HsWordPrim {} = emptyNodeComments
  nodeComments HsInt64Prim {} = emptyNodeComments
  nodeComments HsWord64Prim {} = emptyNodeComments
  nodeComments HsInteger {} = emptyNodeComments
  nodeComments HsRat {} = emptyNodeComments
  nodeComments HsFloatPrim {} = emptyNodeComments
  nodeComments HsDoublePrim {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 8, 0)
  nodeComments HsInt8Prim {} = emptyNodeComments
  nodeComments HsInt16Prim {} = emptyNodeComments
  nodeComments HsInt32Prim {} = emptyNodeComments
  nodeComments HsWord8Prim {} = emptyNodeComments
  nodeComments HsWord16Prim {} = emptyNodeComments
  nodeComments HsWord32Prim {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
  nodeComments HsMultilineString {} = emptyNodeComments
#endif
instance CommentExtraction (HsOuterSigTyVarBndrs GhcPs) where
  nodeComments HsOuterImplicit {} = emptyNodeComments
  nodeComments HsOuterExplicit {..} = nodeComments hso_xexplicit
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
instance CommentExtraction AddEpAnn where
  nodeComments (AddEpAnn _ x) = nodeComments x
#endif
instance CommentExtraction EpaLocation where
  nodeComments = nodeCommentsEpaLocation

nodeCommentsEpaLocation :: EpaLocation -> NodeComments
nodeCommentsEpaLocation EpaSpan {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsEpaLocation (EpaDelta _ _ x) = mconcat $ fmap nodeComments x
#else
nodeCommentsEpaLocation (EpaDelta _ x) = mconcat $ fmap nodeComments x
#endif
instance CommentExtraction AnnPragma where
  nodeComments = nodeCommentsAnnPragma

nodeCommentsAnnPragma :: AnnPragma -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsAnnPragma AnnPragma {..} =
  mconcat
    [ nodeComments apr_open
    , nodeComments apr_close
    , nodeComments $ fst apr_squares
    , nodeComments $ snd apr_squares
    , nodeComments apr_loc1
    , nodeComments apr_loc2
    , nodeComments apr_type
    , nodeComments apr_module
    ]
#else
nodeCommentsAnnPragma AnnPragma {..} =
  mconcat $ fmap nodeComments $ apr_open : apr_close : apr_rest
#endif
instance CommentExtraction HsRuleAnn where
  nodeComments = nodeCommentsHsRuleAnn

nodeCommentsHsRuleAnn :: HsRuleAnn -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsHsRuleAnn HsRuleAnn {..} =
  mconcat
    [ maybe
        emptyNodeComments
        (\(a, b) -> mconcat [nodeComments a, nodeComments b])
        ra_tyanns
    , maybe
        emptyNodeComments
        (\(a, b) -> mconcat [nodeComments a, nodeComments b])
        ra_tmanns
    , nodeComments ra_equal
    , nodeComments ra_rest
    ]
#else
nodeCommentsHsRuleAnn HsRuleAnn {..} =
  mconcat $ f ra_tyanns : f ra_tmanns : fmap nodeComments ra_rest
  where
    f (Just (x, y)) = mconcat $ fmap nodeComments [x, y]
    f Nothing = emptyNodeComments
#endif
instance CommentExtraction AnnFieldLabel where
  nodeComments AnnFieldLabel {afDot = Just x} = nodeComments x
  nodeComments AnnFieldLabel {afDot = Nothing} = emptyNodeComments

instance CommentExtraction EpAnnSumPat where
  nodeComments = nodeCommentsEpAnnSumPat

nodeCommentsEpAnnSumPat :: EpAnnSumPat -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsEpAnnSumPat EpAnnSumPat {..} =
  mconcat
    [ nodeComments $ fst sumPatParens
    , nodeComments $ snd sumPatParens
    , mconcat $ fmap nodeComments sumPatVbarsBefore
    , mconcat $ fmap nodeComments sumPatVbarsAfter
    ]
#else
nodeCommentsEpAnnSumPat EpAnnSumPat {..} =
  mconcat
    [ mconcat $ fmap nodeComments sumPatParens
    , mconcat $ fmap nodeComments sumPatVbarsBefore
    , mconcat $ fmap nodeComments sumPatVbarsAfter
    ]
#endif
instance CommentExtraction AnnProjection where
  nodeComments = nodeCommentsAnnProjection

nodeCommentsAnnProjection :: AnnProjection -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsAnnProjection AnnProjection {..} =
  mconcat [nodeComments apOpen, nodeComments apClose]
#else
nodeCommentsAnnProjection AnnProjection {..} =
  mconcat $ fmap nodeComments [apOpen, apClose]
#endif
instance CommentExtraction AnnsIf where
  nodeComments = nodeCommentsAnnsIf

nodeCommentsAnnsIf :: AnnsIf -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsAnnsIf AnnsIf {..} =
  mconcat
    [ nodeComments aiIf
    , nodeComments aiThen
    , nodeComments aiElse
    , maybe emptyNodeComments nodeComments aiThenSemi
    , maybe emptyNodeComments nodeComments aiElseSemi
    ]
#else
nodeCommentsAnnsIf AnnsIf {..} =
  mconcat
    $ fmap nodeComments
    $ aiIf
        : aiThen
        : aiElse
        : (maybeToList aiThenSemi <> maybeToList aiElseSemi)
#endif
instance CommentExtraction EpAnnHsCase where
  nodeComments = nodeCommentsEpAnnHsCase

nodeCommentsEpAnnHsCase :: EpAnnHsCase -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsEpAnnHsCase EpAnnHsCase {..} =
  mconcat [nodeComments hsCaseAnnCase, nodeComments hsCaseAnnOf]
#else
nodeCommentsEpAnnHsCase EpAnnHsCase {..} =
  mconcat
    $ nodeComments hsCaseAnnCase
        : nodeComments hsCaseAnnOf
        : fmap nodeComments hsCaseAnnsRest
#endif
instance CommentExtraction AnnExplicitSum where
  nodeComments = nodeCommentsAnnExplicitSum

nodeCommentsAnnExplicitSum :: AnnExplicitSum -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsAnnExplicitSum AnnExplicitSum {..} =
  mconcat
    [ nodeComments aesOpen
    , mconcat $ fmap nodeComments aesBarsBefore
    , mconcat $ fmap nodeComments aesBarsAfter
    , nodeComments aesClose
    ]
#else
nodeCommentsAnnExplicitSum AnnExplicitSum {..} =
  mconcat
    $ fmap nodeComments
    $ aesOpen : aesBarsBefore <> aesBarsAfter <> [aesClose]
#endif
instance CommentExtraction EpAnnUnboundVar where
  nodeComments = nodeCommentsEpAnnUnboundVar

nodeCommentsEpAnnUnboundVar :: EpAnnUnboundVar -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsEpAnnUnboundVar EpAnnUnboundVar {hsUnboundBackquotes = (a, b), ..} =
  mconcat [nodeComments a, nodeComments b, nodeComments hsUnboundHole]
#else
nodeCommentsEpAnnUnboundVar EpAnnUnboundVar {..} =
  mconcat
    $ fmap
        nodeComments
        [fst hsUnboundBackquotes, snd hsUnboundBackquotes, hsUnboundHole]
#endif
instance CommentExtraction AnnSig where
  nodeComments = nodeCommentsAnnSig

nodeCommentsAnnSig :: AnnSig -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsAnnSig AnnSig {..} =
  mconcat
    [ nodeComments asDcolon
    , maybe emptyNodeComments nodeComments asPattern
    , maybe emptyNodeComments nodeComments asDefault
    ]
#else
nodeCommentsAnnSig AnnSig {..} = mconcat $ fmap nodeComments $ asDcolon : asRest
#endif
instance CommentExtraction (HsBind GhcPs) where
  nodeComments = nodeCommentsHsBind

nodeCommentsHsBind :: HsBind GhcPs -> NodeComments
nodeCommentsHsBind FunBind {..} = nodeComments fun_id
#if MIN_VERSION_ghc_lib_parser(9, 10, 0)
nodeCommentsHsBind PatBind {} = emptyNodeComments
#else
nodeCommentsHsBind PatBind {..} = nodeComments pat_ext
#endif
nodeCommentsHsBind VarBind {} = emptyNodeComments
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsBind AbsBinds {} = emptyNodeComments
#endif
nodeCommentsHsBind PatSynBind {} = emptyNodeComments

instance CommentExtraction (Sig GhcPs) where
  nodeComments = nodeCommentsSig

nodeCommentsSig :: Sig GhcPs -> NodeComments
nodeCommentsSig (TypeSig x _ _) = nodeComments x
nodeCommentsSig (PatSynSig x _ _) = nodeComments x
nodeCommentsSig (ClassOpSig x _ _ _) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsSig (FixSig ((a, b), _) _) =
  mconcat [nodeComments a, maybe emptyNodeComments nodeComments b]
nodeCommentsSig (InlineSig (a, b, c) _ _) =
  mconcat [nodeComments a, nodeComments b, nodeComments c]
nodeCommentsSig (SpecSig x _ _ _) = nodeComments x
nodeCommentsSig (SpecInstSig ((a, b, c), _) _) =
  mconcat [nodeComments a, nodeComments b, nodeComments c]
nodeCommentsSig (MinimalSig ((a, b), _) _) =
  mconcat [nodeComments a, nodeComments b]
nodeCommentsSig (SCCFunSig ((a, b), _) _ _) =
  mconcat [nodeComments a, nodeComments b]
nodeCommentsSig (CompleteMatchSig ((a, b, c), _) _ _) =
  mconcat
    [nodeComments a, maybe emptyNodeComments nodeComments b, nodeComments c]
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsSig (FixSig x _) = mconcat $ fmap nodeComments x
nodeCommentsSig (InlineSig x _ _) = mconcat $ fmap nodeComments x
nodeCommentsSig (SpecSig x _ _ _) = mconcat $ fmap nodeComments x
nodeCommentsSig (SpecInstSig (x, _) _) = mconcat $ fmap nodeComments x
nodeCommentsSig (MinimalSig (x, _) _) = mconcat $ fmap nodeComments x
nodeCommentsSig (SCCFunSig (x, _) _ _) = mconcat $ fmap nodeComments x
nodeCommentsSig (CompleteMatchSig (x, _) _ _) = mconcat $ fmap nodeComments x
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsSig (FixSig x _) = nodeComments x
nodeCommentsSig (InlineSig x _ _) = nodeComments x
nodeCommentsSig (SpecSig x _ _ _) = nodeComments x
nodeCommentsSig (SpecInstSig (x, _) _) = nodeComments x
nodeCommentsSig (MinimalSig (x, _) _) = nodeComments x
nodeCommentsSig (SCCFunSig (x, _) _ _) = nodeComments x
nodeCommentsSig (CompleteMatchSig (x, _) _ _) = nodeComments x
#else
nodeCommentsSig (FixSig x _) = nodeComments x
nodeCommentsSig (InlineSig x _ _) = nodeComments x
nodeCommentsSig (SpecSig x _ _ _) = nodeComments x
nodeCommentsSig IdSig {} = emptyNodeComments
nodeCommentsSig (SpecInstSig x _ _) = nodeComments x
nodeCommentsSig (MinimalSig x _ _) = nodeComments x
nodeCommentsSig (SCCFunSig x _ _ _) = nodeComments x
nodeCommentsSig (CompleteMatchSig x _ _ _) = nodeComments x
#endif
instance CommentExtraction (HsExpr GhcPs) where
  nodeComments = nodeCommentsHsExpr

nodeCommentsHsExpr :: HsExpr GhcPs -> NodeComments
nodeCommentsHsExpr HsVar {} = emptyNodeComments
nodeCommentsHsExpr HsLam {} = emptyNodeComments
nodeCommentsHsExpr HsAppType {} = emptyNodeComments
nodeCommentsHsExpr (ExplicitSum x _ _ _) = nodeComments x
nodeCommentsHsExpr (HsCase x _ _) = nodeComments x
nodeCommentsHsExpr (HsIf x _ _ _) = nodeComments x
nodeCommentsHsExpr (HsDo x _ _) = nodeComments x
nodeCommentsHsExpr (ExplicitList x _) = nodeComments x
nodeCommentsHsExpr HsProjection {..} = nodeComments proj_ext
nodeCommentsHsExpr HsPragE {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsHsExpr (HsTypedBracket (a, b) _) =
  mconcat [nodeComments a, nodeComments b]
nodeCommentsHsExpr HsUntypedBracket {} = emptyNodeComments
#else
nodeCommentsHsExpr (HsTypedBracket x _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (HsUntypedBracket x _) = mconcat $ fmap nodeComments x
#endif
nodeCommentsHsExpr HsLet {} = emptyNodeComments
nodeCommentsHsExpr HsPar {} = emptyNodeComments
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsHsExpr HsRecSel {} = emptyNodeComments
#endif
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsExpr HsRecSel {} = emptyNodeComments
nodeCommentsHsExpr (HsTypedBracket x _) = nodeComments x
nodeCommentsHsExpr (HsUntypedBracket x _) = nodeComments x
nodeCommentsHsExpr (HsLet x _ _ _ _) = nodeComments x
nodeCommentsHsExpr (HsPar x _ _ _) = nodeComments x
nodeCommentsHsExpr (HsLamCase x _ _) = nodeComments x
#else
nodeCommentsHsExpr HsTick {} = emptyNodeComments
nodeCommentsHsExpr HsBinTick {} = emptyNodeComments
nodeCommentsHsExpr (HsBracket x _) = nodeComments x
nodeCommentsHsExpr HsRnBracketOut {} = notUsedInParsedStage
nodeCommentsHsExpr HsTcBracketOut {} = notUsedInParsedStage
nodeCommentsHsExpr (HsLet x _ _) = nodeComments x
nodeCommentsHsExpr (HsPar x _) = nodeComments x
nodeCommentsHsExpr (HsLamCase x _) = nodeComments x
nodeCommentsHsExpr HsConLikeOut {} = emptyNodeComments
nodeCommentsHsExpr HsRecFld {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsHsExpr (HsTypedSplice x _) = nodeComments x
#else
nodeCommentsHsExpr (HsTypedSplice x _) = mconcat $ fmap nodeComments x
#endif
nodeCommentsHsExpr HsUntypedSplice {} = emptyNodeComments
nodeCommentsHsExpr HsOverLabel {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsHsExpr (HsTypedSplice (x, y) _) = nodeComments x <> nodeComments y
nodeCommentsHsExpr (HsUntypedSplice x _) = nodeComments x
nodeCommentsHsExpr (HsOverLabel x _ _) = nodeComments x
#else
nodeCommentsHsExpr (HsSpliceE x _) = nodeComments x
nodeCommentsHsExpr (HsOverLabel x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr HsGetField {} = emptyNodeComments
nodeCommentsHsExpr SectionR {} = emptyNodeComments
nodeCommentsHsExpr SectionL {} = emptyNodeComments
nodeCommentsHsExpr HsApp {} = emptyNodeComments
nodeCommentsHsExpr HsLit {} = emptyNodeComments
nodeCommentsHsExpr HsOverLit {} = emptyNodeComments
nodeCommentsHsExpr HsIPVar {} = emptyNodeComments
nodeCommentsHsExpr (HsUnboundVar x _) = fromMaybe mempty $ fmap nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsHsExpr (HsStatic x _) = nodeComments x
nodeCommentsHsExpr (HsProc (a, b) _ _) =
  mconcat [nodeComments a, nodeComments b]
nodeCommentsHsExpr (ArithSeq x _ _) = nodeComments x
nodeCommentsHsExpr (ExprWithTySig x _ _) = nodeComments x
nodeCommentsHsExpr RecordUpd {rupd_ext = (s, d)} =
  mconcat
    [ maybe emptyNodeComments nodeComments s
    , maybe emptyNodeComments nodeComments d
    ]
nodeCommentsHsExpr RecordCon {rcon_ext = (s, d)} =
  mconcat
    [ maybe emptyNodeComments nodeComments s
    , maybe emptyNodeComments nodeComments d
    ]
nodeCommentsHsExpr (HsMultiIf (a, b, c) _) =
  mconcat [nodeComments a, nodeComments b, nodeComments c]
nodeCommentsHsExpr (ExplicitTuple (a, b) _ _) =
  mconcat [nodeComments a, nodeComments b]
nodeCommentsHsExpr (NegApp x _ _) = nodeComments x
nodeCommentsHsExpr OpApp {} = emptyNodeComments
#else
nodeCommentsHsExpr (HsStatic x _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (HsProc x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (ArithSeq x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (ExprWithTySig x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr RecordUpd {..} = mconcat $ fmap nodeComments rupd_ext
nodeCommentsHsExpr RecordCon {..} = mconcat $ fmap nodeComments rcon_ext
nodeCommentsHsExpr (HsMultiIf x _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (ExplicitTuple x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (NegApp x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (OpApp x _ _ _) = mconcat $ fmap nodeComments x
#endif
#else
nodeCommentsHsExpr (HsStatic x _) = nodeComments x
nodeCommentsHsExpr (HsProc x _ _) = nodeComments x
nodeCommentsHsExpr (ArithSeq x _ _) = nodeComments x
nodeCommentsHsExpr (ExprWithTySig x _ _) = nodeComments x
nodeCommentsHsExpr HsGetField {..} = nodeComments gf_ext
nodeCommentsHsExpr RecordUpd {..} = nodeComments rupd_ext
nodeCommentsHsExpr RecordCon {..} = nodeComments rcon_ext
nodeCommentsHsExpr (HsMultiIf x _) = nodeComments x
nodeCommentsHsExpr (ExplicitTuple x _ _) = nodeComments x
nodeCommentsHsExpr (SectionR x _ _) = nodeComments x
nodeCommentsHsExpr (SectionL x _ _) = nodeComments x
nodeCommentsHsExpr (NegApp x _ _) = nodeComments x
nodeCommentsHsExpr (OpApp x _ _ _) = nodeComments x
nodeCommentsHsExpr (HsApp x _ _) = nodeComments x
nodeCommentsHsExpr (HsLit x _) = nodeComments x
nodeCommentsHsExpr (HsOverLit x _) = nodeComments x
nodeCommentsHsExpr (HsIPVar x _) = nodeComments x
nodeCommentsHsExpr (HsUnboundVar x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 2)
nodeCommentsHsExpr HsEmbTy {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 12, 2)
nodeCommentsHsExpr HsForAll {} = emptyNodeComments
nodeCommentsHsExpr HsQual {} = emptyNodeComments
nodeCommentsHsExpr HsFunArr {} = emptyNodeComments
#endif
instance CommentExtraction (Match GhcPs a) where
  nodeComments = nodeCommentsMatch

nodeCommentsMatch :: Match GhcPs a -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsMatch Match {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsMatch Match {..} = mconcat $ fmap nodeComments m_ext
#else
nodeCommentsMatch Match {..} = nodeComments m_ext
#endif
instance CommentExtraction (StmtLR GhcPs GhcPs a) where
  nodeComments = nodeCommentsStmtLR

nodeCommentsStmtLR :: StmtLR GhcPs GhcPs a -> NodeComments
nodeCommentsStmtLR LastStmt {} = emptyNodeComments
nodeCommentsStmtLR BodyStmt {} = emptyNodeComments
nodeCommentsStmtLR ParStmt {} = emptyNodeComments
nodeCommentsStmtLR RecStmt {..} = nodeComments recS_ext
#if MIN_VERSION_ghc_lib_parser(9, 12, 1) || !MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsStmtLR (BindStmt x _ _) = nodeComments x
nodeCommentsStmtLR (LetStmt x _) = nodeComments x
nodeCommentsStmtLR TransStmt {..} = nodeComments trS_ext
#else
nodeCommentsStmtLR (BindStmt x _ _) = mconcat $ fmap nodeComments x
nodeCommentsStmtLR (LetStmt x _) = mconcat $ fmap nodeComments x
nodeCommentsStmtLR TransStmt {..} = mconcat $ fmap nodeComments trS_ext
#endif
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsStmtLR ApplicativeStmt {} = emptyNodeComments
#endif
instance CommentExtraction (ConDeclField GhcPs) where
  nodeComments = nodeCommentsConDeclField

nodeCommentsConDeclField :: ConDeclField GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1) || !MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsConDeclField ConDeclField {..} = nodeComments cd_fld_ext
#else
nodeCommentsConDeclField ConDeclField {..} =
  mconcat $ fmap nodeComments cd_fld_ext
#endif
instance CommentExtraction (HsDerivingClause GhcPs) where
  nodeComments = nodeCommentsHsDerivingClause

nodeCommentsHsDerivingClause :: HsDerivingClause GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1) || !MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsDerivingClause HsDerivingClause {..} =
  nodeComments deriv_clause_ext
#else
nodeCommentsHsDerivingClause HsDerivingClause {..} =
  mconcat $ fmap nodeComments deriv_clause_ext
#endif
-- | This instance is for type family declarations inside a class declaration.
instance CommentExtraction (FamilyDecl GhcPs) where
  nodeComments = nodeCommentsFamilyDecl

nodeCommentsFamilyDecl :: FamilyDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsFamilyDecl FamilyDecl {..} = nodeComments fdExt
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsFamilyDecl FamilyDecl {..} = mconcat $ fmap nodeComments fdExt
#else
nodeCommentsFamilyDecl FamilyDecl {..} = nodeComments fdExt
#endif
instance CommentExtraction (HsTyVarBndr a GhcPs) where
  nodeComments = nodeCommentsHsTyVarBndr

nodeCommentsHsTyVarBndr :: HsTyVarBndr a GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsHsTyVarBndr HsTvb {..} = nodeComments tvb_ext
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsTyVarBndr (UserTyVar x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsTyVarBndr (KindedTyVar x _ _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsHsTyVarBndr (UserTyVar x _ _) = nodeComments x
nodeCommentsHsTyVarBndr (KindedTyVar x _ _ _) = nodeComments x
#endif
instance CommentExtraction (IE GhcPs) where
  nodeComments = nodeCommentsIE

nodeCommentsIE :: IE GhcPs -> NodeComments
nodeCommentsIE IEVar {} = emptyNodeComments
nodeCommentsIE IEGroup {} = emptyNodeComments
nodeCommentsIE IEDoc {} = emptyNodeComments
nodeCommentsIE IEDocNamed {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsIE (IEThingAbs _ x _) = nodeComments x
nodeCommentsIE (IEThingAll _ x _) = nodeComments x
nodeCommentsIE (IEThingWith _ x _ _ _) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsIE (IEModuleContents (x, y) _) =
  mconcat $ maybeToList (fmap nodeComments x) <> [nodeComments y]
#else
nodeCommentsIE (IEModuleContents (x, y) _) =
  mconcat $ maybeToList (fmap nodeComments x) <> fmap nodeComments y
#endif
#elif MIN_VERSION_ghc_lib_parser(9, 8, 1)
nodeCommentsIE (IEThingAbs (_, x) _) = nodeComments x
nodeCommentsIE (IEThingAll (_, x) _) = nodeComments x
nodeCommentsIE (IEThingWith (_, x) _ _ _) = nodeComments x
nodeCommentsIE (IEModuleContents (_, x) _) = nodeComments x
#else
nodeCommentsIE (IEThingAbs x _) = nodeComments x
nodeCommentsIE (IEThingAll x _) = nodeComments x
nodeCommentsIE (IEThingWith x _ _ _) = nodeComments x
nodeCommentsIE (IEModuleContents x _) = nodeComments x
#endif
instance CommentExtraction (FamEqn GhcPs a) where
  nodeComments = nodeCommentsFamEqn

nodeCommentsFamEqn :: FamEqn GhcPs a -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsFamEqn FamEqn {feqn_ext = (as, bs, c)} =
  mconcat
    [ mconcat $ fmap nodeComments as
    , mconcat $ fmap nodeComments bs
    , nodeComments c
    ]
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsFamEqn FamEqn {..} = mconcat $ fmap nodeComments feqn_ext
#else
nodeCommentsFamEqn FamEqn {..} = nodeComments feqn_ext
#endif
instance CommentExtraction (WarnDecls GhcPs) where
  nodeComments = nodeCommentsWarnDecls

nodeCommentsWarnDecls :: WarnDecls GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsWarnDecls Warnings {wd_ext = ((a, b), _)} =
  mconcat [nodeComments a, nodeComments b]
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsWarnDecls Warnings {..} = mconcat $ fmap nodeComments $ fst wd_ext
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsWarnDecls Warnings {..} = nodeComments $ fst wd_ext
#else
nodeCommentsWarnDecls Warnings {..} = nodeComments wd_ext
#endif
instance CommentExtraction (WarnDecl GhcPs) where
  nodeComments = nodeCommentsWarnDecl

nodeCommentsWarnDecl :: WarnDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsWarnDecl (Warning (a, (b, c)) _ _) =
  mconcat [nodeComments a, nodeComments b, nodeComments c]
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsWarnDecl (Warning (_, x) _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsWarnDecl (Warning x _ _) = nodeComments x
#endif
instance CommentExtraction (RuleDecls GhcPs) where
  nodeComments = nodeCommentsRuleDecls

nodeCommentsRuleDecls :: RuleDecls GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsRuleDecls HsRules {rds_ext = ((a, b), _)} =
  mconcat [nodeComments a, nodeComments b]
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsRuleDecls HsRules {..} = mconcat $ fmap nodeComments $ fst rds_ext
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsRuleDecls HsRules {..} = nodeComments $ fst rds_ext
#else
nodeCommentsRuleDecls HsRules {..} = nodeComments rds_ext
#endif
instance CommentExtraction (RuleDecl GhcPs) where
  nodeComments = nodeCommentsRuleDecl

nodeCommentsRuleDecl :: RuleDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsRuleDecl HsRule {..} = nodeComments $ fst rd_ext
#else
nodeCommentsRuleDecl HsRule {..} = nodeComments rd_ext
#endif
instance CommentExtraction (DerivDecl GhcPs) where
  nodeComments = nodeCommentsDerivDecl

nodeCommentsDerivDecl :: DerivDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsDerivDecl DerivDecl {deriv_ext = (a, (b, c))} =
  mconcat
    [maybe emptyNodeComments nodeComments a, nodeComments b, nodeComments c]
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsDerivDecl DerivDecl {deriv_ext = (x, xs)} =
  mconcat $ maybeToList (fmap nodeComments x) <> fmap nodeComments xs
#else
nodeCommentsDerivDecl DerivDecl {..} = nodeComments deriv_ext
#endif
instance CommentExtraction (StandaloneKindSig GhcPs) where
  nodeComments = nodeCommentsStandaloneKindSig

nodeCommentsStandaloneKindSig :: StandaloneKindSig GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsStandaloneKindSig (StandaloneKindSig x _ _) =
  mconcat [nodeComments $ fst x, nodeComments $ snd x]
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsStandaloneKindSig (StandaloneKindSig x _ _) =
  mconcat $ fmap nodeComments x
#else
nodeCommentsStandaloneKindSig (StandaloneKindSig x _ _) = nodeComments x
#endif
instance CommentExtraction (DefaultDecl GhcPs) where
  nodeComments = nodeCommentsDefaultDecl

nodeCommentsDefaultDecl :: DefaultDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsDefaultDecl (DefaultDecl (a, b, c) _ _) =
  mconcat [nodeComments a, nodeComments b, nodeComments c]
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsDefaultDecl (DefaultDecl x _) = mconcat $ fmap nodeComments x
#else
nodeCommentsDefaultDecl (DefaultDecl x _) = nodeComments x
#endif
instance CommentExtraction (ForeignDecl GhcPs) where
  nodeComments = nodeCommentsForeignDecl

nodeCommentsForeignDecl :: ForeignDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsForeignDecl ForeignImport {fd_i_ext = (a, b, c)} =
  mconcat [nodeComments a, nodeComments b, nodeComments c]
nodeCommentsForeignDecl ForeignExport {fd_e_ext = (a, b, c)} =
  mconcat [nodeComments a, nodeComments b, nodeComments c]
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsForeignDecl ForeignImport {..} =
  mconcat $ fmap nodeComments fd_i_ext
nodeCommentsForeignDecl ForeignExport {..} =
  mconcat $ fmap nodeComments fd_e_ext
#else
nodeCommentsForeignDecl ForeignImport {..} = nodeComments fd_i_ext
nodeCommentsForeignDecl ForeignExport {..} = nodeComments fd_e_ext
#endif
instance CommentExtraction (AnnDecl GhcPs) where
  nodeComments = nodeCommentsAnnDecl

nodeCommentsAnnDecl :: AnnDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsAnnDecl (HsAnnotation (x, _) _ _) = nodeComments x
#else
nodeCommentsAnnDecl (HsAnnotation x _ _ _) = nodeComments x
#endif
instance CommentExtraction (RoleAnnotDecl GhcPs) where
  nodeComments = nodeCommentsRoleAnnotDecl

nodeCommentsRoleAnnotDecl :: RoleAnnotDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsRoleAnnotDecl (RoleAnnotDecl x _ _) =
  mconcat [nodeComments $ fst x, nodeComments $ snd x]
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsRoleAnnotDecl (RoleAnnotDecl x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsRoleAnnotDecl (RoleAnnotDecl x _ _) = nodeComments x
#endif
instance CommentExtraction (TyFamInstDecl GhcPs) where
  nodeComments = nodeCommentsTyFamInstDecl

nodeCommentsTyFamInstDecl :: TyFamInstDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsTyFamInstDecl TyFamInstDecl {..} =
  mconcat [nodeComments $ fst tfid_xtn, nodeComments $ snd tfid_xtn]
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsTyFamInstDecl TyFamInstDecl {..} =
  mconcat $ fmap nodeComments tfid_xtn
#else
nodeCommentsTyFamInstDecl TyFamInstDecl {..} = nodeComments tfid_xtn
#endif
instance CommentExtraction (PatSynBind GhcPs GhcPs) where
  nodeComments = nodeCommentsPatSynBind

nodeCommentsPatSynBind :: PatSynBind GhcPs GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1) || !MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPatSynBind PSB {..} = nodeComments psb_ext
#else
nodeCommentsPatSynBind PSB {..} = mconcat $ fmap nodeComments psb_ext
#endif
instance CommentExtraction InlineSpec where
  nodeComments = nodeCommentsInlineSpec

nodeCommentsInlineSpec :: InlineSpec -> NodeComments
nodeCommentsInlineSpec Inline {} = emptyNodeComments
nodeCommentsInlineSpec Inlinable {} = emptyNodeComments
nodeCommentsInlineSpec NoInline {} = emptyNodeComments
nodeCommentsInlineSpec NoUserInlinePrag {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsInlineSpec Opaque {} = emptyNodeComments
#endif
instance CommentExtraction (DerivStrategy GhcPs) where
  nodeComments = nodeCommentsDerivStrategy

nodeCommentsDerivStrategy :: DerivStrategy GhcPs -> NodeComments
nodeCommentsDerivStrategy (ViaStrategy x) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsDerivStrategy (StockStrategy x) = nodeComments x
nodeCommentsDerivStrategy (AnyclassStrategy x) = nodeComments x
nodeCommentsDerivStrategy (NewtypeStrategy x) = nodeComments x
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsDerivStrategy (StockStrategy x) = mconcat $ fmap nodeComments x
nodeCommentsDerivStrategy (AnyclassStrategy x) = mconcat $ fmap nodeComments x
nodeCommentsDerivStrategy (NewtypeStrategy x) = mconcat $ fmap nodeComments x
#else
nodeCommentsDerivStrategy (StockStrategy x) = nodeComments x
nodeCommentsDerivStrategy (AnyclassStrategy x) = nodeComments x
nodeCommentsDerivStrategy (NewtypeStrategy x) = nodeComments x
#endif
instance CommentExtraction XViaStrategyPs where
  nodeComments = nodeCommentsXViaStrategyPs

nodeCommentsXViaStrategyPs :: XViaStrategyPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1) || !MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsXViaStrategyPs (XViaStrategyPs x _) = nodeComments x
#else
nodeCommentsXViaStrategyPs (XViaStrategyPs x _) = mconcat $ fmap nodeComments x
#endif
instance CommentExtraction (RuleBndr GhcPs) where
  nodeComments = nodeCommentsRuleBndr

nodeCommentsRuleBndr :: RuleBndr GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsRuleBndr (RuleBndr x _) = nodeComments x
nodeCommentsRuleBndr (RuleBndrSig x _ _) = nodeComments x
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsRuleBndr (RuleBndr x _) = mconcat $ fmap nodeComments x
nodeCommentsRuleBndr (RuleBndrSig x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsRuleBndr (RuleBndr x _) = nodeComments x
nodeCommentsRuleBndr (RuleBndrSig x _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance CommentExtraction FieldLabelString where
  nodeComments = const emptyNodeComments

instance CommentExtraction (HsUntypedSplice GhcPs) where
  nodeComments = nodeCommentsHsUntypedSplice

nodeCommentsHsUntypedSplice :: HsUntypedSplice GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsHsUntypedSplice (HsUntypedSpliceExpr x _) = nodeComments x
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsUntypedSplice (HsUntypedSpliceExpr x _) =
  mconcat $ fmap nodeComments x
#else
nodeCommentsHsUntypedSplice (HsUntypedSpliceExpr x _) = nodeComments x
#endif
nodeCommentsHsUntypedSplice HsQuasiQuote {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
instance CommentExtraction (HsQuote GhcPs) where
  nodeComments ExpBr {} = emptyNodeComments
  nodeComments PatBr {} = emptyNodeComments
  nodeComments DecBrL {} = emptyNodeComments
  nodeComments DecBrG {} = emptyNodeComments
  nodeComments TypBr {} = emptyNodeComments
  nodeComments VarBr {} = emptyNodeComments

instance CommentExtraction (WithHsDocIdentifiers StringLiteral GhcPs) where
  nodeComments WithHsDocIdentifiers {} = emptyNodeComments

instance CommentExtraction (HsFieldBind a b) where
  nodeComments = nodeCommentsHsFieldBind

nodeCommentsHsFieldBind :: HsFieldBind a b -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsHsFieldBind HsFieldBind {..} = maybe mempty nodeComments hfbAnn
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsFieldBind HsFieldBind {..} = mconcat $ fmap nodeComments hfbAnn
#else
nodeCommentsHsFieldBind HsFieldBind {..} = nodeComments hfbAnn
#endif
#else
instance CommentExtraction (HsFieldLabel GhcPs) where
  nodeComments HsFieldLabel {..} = nodeComments hflExt
#endif
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
instance CommentExtraction (HsBracket GhcPs) where
  nodeComments ExpBr {} = emptyNodeComments
  nodeComments PatBr {} = emptyNodeComments
  nodeComments DecBrL {} = emptyNodeComments
  nodeComments DecBrG {} = emptyNodeComments
  nodeComments TypBr {} = emptyNodeComments
  nodeComments VarBr {} = emptyNodeComments
  nodeComments TExpBr {} = emptyNodeComments

instance CommentExtraction (HsRecField' (FieldOcc GhcPs) a) where
  nodeComments HsRecField {..} = nodeComments hsRecFieldAnn
#endif
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance CommentExtraction XImportDeclPass where
  nodeComments XImportDeclPass {..} = nodeComments ideclAnn

instance CommentExtraction (ForeignImport GhcPs) where
  nodeComments CImport {} = emptyNodeComments

instance CommentExtraction (ForeignExport GhcPs) where
  nodeComments CExport {} = emptyNodeComments
#else
instance CommentExtraction (HsSplice GhcPs) where
  nodeComments (HsTypedSplice x _ _ _) = nodeComments x
  nodeComments (HsUntypedSplice x _ _ _) = nodeComments x
  nodeComments HsQuasiQuote {} = emptyNodeComments
  nodeComments HsSpliced {} = emptyNodeComments

instance CommentExtraction ForeignImport where
  nodeComments CImport {} = emptyNodeComments

instance CommentExtraction ForeignExport where
  nodeComments CExport {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
instance CommentExtraction
           (HsArg
              GhcPs
              (GenLocated SrcSpanAnnA (HsType GhcPs))
              (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  nodeComments HsValArg {} = emptyNodeComments
  nodeComments HsTypeArg {} = emptyNodeComments
  nodeComments HsArgPar {} = emptyNodeComments

instance CommentExtraction (LHsRecUpdFields GhcPs) where
  nodeComments RegularRecUpdFields {} = emptyNodeComments
  nodeComments OverloadedRecUpdFields {} = emptyNodeComments
#else
instance CommentExtraction
           (HsArg
              (GenLocated SrcSpanAnnA (HsType GhcPs))
              (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  nodeComments HsValArg {} = emptyNodeComments
  nodeComments HsTypeArg {} = emptyNodeComments
  nodeComments HsArgPar {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
instance CommentExtraction (EpAnn a) where
  nodeComments (EpAnn ann _ cs) = NodeComments {..}
    where
      commentsBefore = priorComments cs
      commentsOnSameLine = filter isCommentOnSameLine $ getFollowingComments cs
      commentsAfter =
        filter (not . isCommentOnSameLine) $ getFollowingComments cs
      isCommentOnSameLine (L comAnn _) =
        srcSpanEndLine (epaLocationRealSrcSpan ann)
          == srcSpanStartLine (epaLocationRealSrcSpan comAnn)

instance CommentExtraction (EpaLocation' NoComments) where
  nodeComments EpaSpan {} = emptyNodeComments
  nodeComments EpaDelta {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (EpAnn a) where
  nodeComments (EpAnn ann _ cs) = NodeComments {..}
    where
      commentsBefore = priorComments cs
      commentsOnSameLine = filter isCommentOnSameLine $ getFollowingComments cs
      commentsAfter =
        filter (not . isCommentOnSameLine) $ getFollowingComments cs
      isCommentOnSameLine (L comAnn _) =
        srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)

instance CommentExtraction (EpaLocation' NoComments) where
  nodeComments EpaSpan {} = emptyNodeComments
  nodeComments EpaDelta {} = emptyNodeComments
#else
instance CommentExtraction Anchor where
  nodeComments Anchor {} = emptyNodeComments

instance CommentExtraction (SrcAnn a) where
  nodeComments (SrcSpanAnn ep _) = nodeComments ep

instance CommentExtraction (EpAnn a) where
  nodeComments (EpAnn ann _ cs) = NodeComments {..}
    where
      commentsBefore = priorComments cs
      commentsOnSameLine = filter isCommentOnSameLine $ getFollowingComments cs
      commentsAfter =
        filter (not . isCommentOnSameLine) $ getFollowingComments cs
      isCommentOnSameLine (L comAnn _) =
        srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)
  nodeComments EpAnnNotUsed = emptyNodeComments
#endif

#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
instance CommentExtraction a => CommentExtraction (AnnList a) where
  nodeComments AnnList {..} = mconcat [a, b, c, d, e]
    where
      a = maybe mempty nodeComments al_anchor
      b = nodeComments al_brackets
      c = mconcat $ fmap nodeComments al_semis
      d = nodeComments al_rest
      e = mconcat $ fmap nodeComments al_trailing
#else
instance CommentExtraction AnnList where
  nodeComments AnnList {..} = mconcat [a, b, c, d, e]
    where
      a = maybe mempty nodeComments al_anchor
      b = maybe mempty nodeComments al_open
      c = maybe mempty nodeComments al_close
      d = mconcat $ fmap nodeComments al_rest
      e = mconcat $ fmap nodeComments al_trailing
#endif
instance CommentExtraction AnnParen where
  nodeComments = nodeCommentsAnnParen

nodeCommentsAnnParen :: AnnParen -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsAnnParen (AnnParens open close) =
  mconcat [nodeComments open, nodeComments close]
nodeCommentsAnnParen (AnnParensHash open close) =
  mconcat [nodeComments open, nodeComments close]
nodeCommentsAnnParen (AnnParensSquare open close) =
  mconcat [nodeComments open, nodeComments close]
#else
nodeCommentsAnnParen AnnParen {..} =
  mconcat $ fmap nodeComments [ap_open, ap_close]
#endif
instance CommentExtraction TrailingAnn where
  nodeComments = nodeCommentsTrailingAnn

nodeCommentsTrailingAnn :: TrailingAnn -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
nodeCommentsTrailingAnn (AddSemiAnn token) = nodeComments token
nodeCommentsTrailingAnn (AddCommaAnn token) = nodeComments token
nodeCommentsTrailingAnn (AddVbarAnn token) = nodeComments token
nodeCommentsTrailingAnn (AddDarrowAnn token) = nodeComments token
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsTrailingAnn AddSemiAnn {..} = nodeComments ta_location
nodeCommentsTrailingAnn AddCommaAnn {..} = nodeComments ta_location
nodeCommentsTrailingAnn AddVbarAnn {..} = nodeComments ta_location
nodeCommentsTrailingAnn AddDarrowAnn {..} = nodeComments ta_location
nodeCommentsTrailingAnn AddDarrowUAnn {..} = nodeComments ta_location
#else
nodeCommentsTrailingAnn (AddSemiAnn token) = nodeComments token
nodeCommentsTrailingAnn (AddCommaAnn token) = nodeComments token
nodeCommentsTrailingAnn (AddVbarAnn token) = nodeComments token
#endif
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
instance CommentExtraction AnnTransStmt where
  nodeComments AnnTransStmt {..} =
    mconcat
      [ nodeComments ats_then
      , maybe emptyNodeComments nodeComments ats_group
      , maybe emptyNodeComments nodeComments ats_by
      , maybe emptyNodeComments nodeComments ats_using
      ]

instance CommentExtraction (EpToken a) where
  nodeComments NoEpTok = emptyNodeComments
  nodeComments (EpTok x) = nodeComments x

instance CommentExtraction AnnTyVarBndr where
  nodeComments AnnTyVarBndr {..} =
    mconcat
      [ mconcat $ fmap nodeComments atv_opens
      , mconcat $ fmap nodeComments atv_closes
      , nodeComments atv_tv
      , nodeComments atv_dcolon
      ]

instance CommentExtraction (EpUniToken a b) where
  nodeComments NoEpUniTok = emptyNodeComments
  nodeComments (EpUniTok x _) = nodeComments x

instance CommentExtraction AnnPSB where
  nodeComments AnnPSB {..} =
    mconcat
      [ nodeComments ap_pattern
      , maybe emptyNodeComments nodeComments ap_openc
      , maybe emptyNodeComments nodeComments ap_closec
      , maybe emptyNodeComments nodeComments ap_larrow
      , maybe emptyNodeComments nodeComments ap_equal
      ]

instance CommentExtraction NamespaceSpecifier where
  nodeComments NoNamespaceSpecifier = emptyNodeComments
  nodeComments (TypeNamespaceSpecifier x) = nodeComments x
  nodeComments (DataNamespaceSpecifier x) = nodeComments x

instance CommentExtraction AnnFamilyDecl where
  nodeComments AnnFamilyDecl {..} =
    mconcat
      [ mconcat $ fmap nodeComments afd_openp
      , mconcat $ fmap nodeComments afd_closep
      , nodeComments afd_type
      , nodeComments afd_data
      , nodeComments afd_family
      , nodeComments afd_dcolon
      , nodeComments afd_equal
      , nodeComments afd_vbar
      , nodeComments afd_where
      , nodeComments afd_openc
      , nodeComments afd_dotdot
      , nodeComments afd_closec
      ]

instance CommentExtraction AnnListBrackets where
  nodeComments (ListParens s d) = mconcat [nodeComments s, nodeComments d]
  nodeComments (ListBraces s d) = mconcat [nodeComments s, nodeComments d]
  nodeComments (ListSquare s d) = mconcat [nodeComments s, nodeComments d]
  nodeComments (ListBanana s d) = mconcat [nodeComments s, nodeComments d]
  nodeComments ListNone = emptyNodeComments

instance CommentExtraction () where
  nodeComments () = emptyNodeComments

instance CommentExtraction AnnArithSeq where
  nodeComments AnnArithSeq {..} =
    mconcat
      [ nodeComments aas_open
      , maybe emptyNodeComments nodeComments aas_comma
      , nodeComments aas_dotdot
      , nodeComments aas_close
      ]

instance (CommentExtraction a, CommentExtraction b) =>
         CommentExtraction (BracketAnn a b) where
  nodeComments (BracketNoE x) = nodeComments x
  nodeComments (BracketHasE x) = nodeComments x

instance CommentExtraction AnnSpecSig where
  nodeComments AnnSpecSig {..} =
    mconcat
      [ nodeComments ass_open
      , nodeComments ass_close
      , nodeComments ass_dcolon
      , nodeComments ass_act
      ]

instance CommentExtraction ActivationAnn where
  nodeComments ActivationAnn {..} =
    mconcat
      [ nodeComments aa_openc
      , nodeComments aa_closec
      , maybe emptyNodeComments nodeComments aa_tilde
      , maybe emptyNodeComments nodeComments aa_val
      ]
#endif
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
-- | Marks an AST node as never appearing in the AST.
--
-- Some AST node types are only used in the renaming or type-checking phase.
notUsedInParsedStage :: HasCallStack => a
notUsedInParsedStage =
  error
    "This AST should never appears in an AST. It only appears in the renaming or type checked stages."
#endif
-- | A 'NodeComment' with no comments.
emptyNodeComments :: NodeComments
emptyNodeComments = NodeComments [] [] []
