{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- | Comment handling around an AST node
module HIndent.Pretty.NodeComments
  ( CommentExtraction(..)
  , emptyNodeComments
  ) where

import Data.Maybe
import Data.Void
import GHC.Core.Coercion
import GHC.Data.BooleanFormula
import GHC.Hs
import GHC.Stack
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import HIndent.Ast.NodeComments
import HIndent.Pragma
import HIndent.Pretty.SigBindFamily
import HIndent.Pretty.Types
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
import GHC.Core.DataCon
#else
import GHC.Unit
#endif
-- | An interface to extract comments from an AST node.
class CommentExtraction a where
  nodeComments :: a -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (HsModule GhcPs) where
  nodeComments =
    nodeComments . filterOutEofAndPragmasFromAnn . hsmodAnn . hsmodExt
    where
      filterOutEofAndPragmasFromAnn EpAnn {..} =
        EpAnn {comments = filterOutEofAndPragmasFromComments comments, ..}
      filterOutEofAndPragmasFromComments comments =
        EpaCommentsBalanced
          { priorComments = filterOutEofAndPragmas $ priorComments comments
          , followingComments =
              filterOutEofAndPragmas $ getFollowingComments comments
          }
      filterOutEofAndPragmas = filter isNeitherEofNorPragmaComment
      isNeitherEofNorPragmaComment (L _ (EpaComment tok _)) = not $ isPragma tok
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance CommentExtraction (HsModule GhcPs) where
  nodeComments =
    nodeComments . filterOutEofAndPragmasFromAnn . hsmodAnn . hsmodExt
    where
      filterOutEofAndPragmasFromAnn EpAnn {..} =
        EpAnn {comments = filterOutEofAndPragmasFromComments comments, ..}
      filterOutEofAndPragmasFromAnn EpAnnNotUsed = EpAnnNotUsed
      filterOutEofAndPragmasFromComments comments =
        EpaCommentsBalanced
          { priorComments = filterOutEofAndPragmas $ priorComments comments
          , followingComments =
              filterOutEofAndPragmas $ getFollowingComments comments
          }
      filterOutEofAndPragmas = filter isNeitherEofNorPragmaComment
      isNeitherEofNorPragmaComment (L _ (EpaComment EpaEofComment _)) = False
      isNeitherEofNorPragmaComment (L _ (EpaComment tok _)) = not $ isPragma tok
#else
instance CommentExtraction HsModule where
  nodeComments = nodeComments . filterOutEofAndPragmasFromAnn . hsmodAnn
    where
      filterOutEofAndPragmasFromAnn EpAnn {..} =
        EpAnn {comments = filterOutEofAndPragmasFromComments comments, ..}
      filterOutEofAndPragmasFromAnn EpAnnNotUsed = EpAnnNotUsed
      filterOutEofAndPragmasFromComments comments =
        EpaCommentsBalanced
          { priorComments = filterOutEofAndPragmas $ priorComments comments
          , followingComments =
              filterOutEofAndPragmas $ getFollowingComments comments
          }
      filterOutEofAndPragmas = filter isNeitherEofNorPragmaComment
      isNeitherEofNorPragmaComment (L _ (EpaComment EpaEofComment _)) = False
      isNeitherEofNorPragmaComment (L _ (EpaComment tok _)) = not $ isPragma tok
#endif
instance CommentExtraction l => CommentExtraction (GenLocated l e) where
  nodeComments (L l _) = nodeComments l

instance CommentExtraction (HsDecl GhcPs) where
  nodeComments TyClD {} = emptyNodeComments
  nodeComments InstD {} = emptyNodeComments
  nodeComments DerivD {} = emptyNodeComments
  nodeComments ValD {} = emptyNodeComments
  nodeComments SigD {} = emptyNodeComments
  nodeComments KindSigD {} = emptyNodeComments
  nodeComments DefD {} = emptyNodeComments
  nodeComments ForD {} = emptyNodeComments
  nodeComments WarningD {} = emptyNodeComments
  nodeComments AnnD {} = emptyNodeComments
  nodeComments RuleD {} = emptyNodeComments
  nodeComments SpliceD {} = emptyNodeComments
  nodeComments DocD {} =
    error "Document comments should be treated as normal ones."
  nodeComments RoleAnnotD {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 0)
instance CommentExtraction (TyClDecl GhcPs) where
  nodeComments FamDecl {} = emptyNodeComments
  nodeComments SynDecl {..} = mconcat $ fmap nodeComments tcdSExt
  nodeComments DataDecl {..} = mconcat $ fmap nodeComments tcdDExt
  nodeComments ClassDecl {tcdCExt = (x, _, _)} = mconcat $ fmap nodeComments x
#elif MIN_VERSION_ghc_lib_parser(9, 6, 0)
instance CommentExtraction (TyClDecl GhcPs) where
  nodeComments FamDecl {} = emptyNodeComments
  nodeComments SynDecl {..} = nodeComments tcdSExt
  nodeComments DataDecl {..} = nodeComments tcdDExt
  nodeComments ClassDecl {tcdCExt = (x, _)} = nodeComments x
#else
instance CommentExtraction (TyClDecl GhcPs) where
  nodeComments FamDecl {} = emptyNodeComments
  nodeComments SynDecl {..} = nodeComments tcdSExt
  nodeComments DataDecl {..} = nodeComments tcdDExt
  nodeComments ClassDecl {tcdCExt = (x, _, _)} = nodeComments x
#endif
instance CommentExtraction (InstDecl GhcPs) where
  nodeComments = nodeCommentsInstDecl

nodeCommentsInstDecl :: InstDecl GhcPs -> NodeComments
nodeCommentsInstDecl ClsInstD {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9,4,1)
nodeCommentsInstDecl DataFamInstD {} = emptyNodeComments
#else
nodeCommentsInstDecl DataFamInstD {..} = nodeComments dfid_ext
#endif
nodeCommentsInstDecl TyFamInstD {} = emptyNodeComments

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
#if MIN_VERSION_ghc_lib_parser(9, 10, 0)
instance CommentExtraction (Sig GhcPs) where
  nodeComments (TypeSig x _ _) = nodeComments x
  nodeComments (PatSynSig x _ _) = nodeComments x
  nodeComments (ClassOpSig x _ _ _) = nodeComments x
  nodeComments (FixSig x _) = mconcat $ fmap nodeComments x
  nodeComments (InlineSig x _ _) = mconcat $ fmap nodeComments x
  nodeComments (SpecSig x _ _ _) = mconcat $ fmap nodeComments x
  nodeComments (SpecInstSig x _) = mconcat $ fmap nodeComments $ fst x
  nodeComments (MinimalSig x _) = mconcat $ fmap nodeComments $ fst x
  nodeComments (SCCFunSig x _ _) = mconcat $ fmap nodeComments $ fst x
  nodeComments (CompleteMatchSig x _ _) = mconcat $ fmap nodeComments $ fst x
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance CommentExtraction (Sig GhcPs) where
  nodeComments (TypeSig x _ _) = nodeComments x
  nodeComments (PatSynSig x _ _) = nodeComments x
  nodeComments (ClassOpSig x _ _ _) = nodeComments x
  nodeComments (FixSig x _) = nodeComments x
  nodeComments (InlineSig x _ _) = nodeComments x
  nodeComments (SpecSig x _ _ _) = nodeComments x
  nodeComments (SpecInstSig x _) = nodeComments $ fst x
  nodeComments (MinimalSig x _) = nodeComments $ fst x
  nodeComments (SCCFunSig x _ _) = nodeComments $ fst x
  nodeComments (CompleteMatchSig x _ _) = nodeComments $ fst x
#else
instance CommentExtraction (Sig GhcPs) where
  nodeComments (TypeSig x _ _) = nodeComments x
  nodeComments (PatSynSig x _ _) = nodeComments x
  nodeComments (ClassOpSig x _ _ _) = nodeComments x
  nodeComments IdSig {} = emptyNodeComments
  nodeComments (FixSig x _) = nodeComments x
  nodeComments (InlineSig x _ _) = nodeComments x
  nodeComments (SpecSig x _ _ _) = nodeComments x
  nodeComments (SpecInstSig x _ _) = nodeComments x
  nodeComments (MinimalSig x _ _) = nodeComments x
  nodeComments (SCCFunSig x _ _ _) = nodeComments x
  nodeComments (CompleteMatchSig x _ _ _) = nodeComments x
#endif
instance CommentExtraction (HsDataDefn GhcPs) where
  nodeComments HsDataDefn {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 0)
instance CommentExtraction (ClsInstDecl GhcPs) where
  nodeComments ClsInstDecl {cid_ext = (x, _, _)} =
    fromMaybe mempty $ fmap nodeComments x
#else
instance CommentExtraction (ClsInstDecl GhcPs) where
  nodeComments ClsInstDecl {cid_ext = (x, _)} = nodeComments x
#endif
instance CommentExtraction (MatchGroup GhcPs a) where
  nodeComments MG {} = emptyNodeComments

instance CommentExtraction (HsExpr GhcPs) where
  nodeComments = nodeCommentsHsExpr

instance CommentExtraction LambdaCase where
  nodeComments (LambdaCase x _) = nodeComments x

instance CommentExtraction DoOrMdo where
  nodeComments = const emptyNodeComments

instance CommentExtraction QualifiedDo where
  nodeComments = const emptyNodeComments

nodeCommentsHsExpr :: HsExpr GhcPs -> NodeComments
nodeCommentsHsExpr HsVar {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (HsUnboundVar x _) = fromMaybe mempty $ fmap nodeComments x
#else
nodeCommentsHsExpr (HsUnboundVar x _) = nodeComments x
#endif
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsExpr HsConLikeOut {} = emptyNodeComments
nodeCommentsHsExpr HsRecFld {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr HsOverLabel {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsHsExpr (HsOverLabel x _ _) = nodeComments x
#else
nodeCommentsHsExpr (HsOverLabel x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr HsIPVar {} = emptyNodeComments
#else
nodeCommentsHsExpr (HsIPVar x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr HsOverLit {} = emptyNodeComments
#else
nodeCommentsHsExpr (HsOverLit x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr HsLit {} = emptyNodeComments
#else
nodeCommentsHsExpr (HsLit x _) = nodeComments x
#endif
nodeCommentsHsExpr HsLam {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
-- No `HsLamCase` since 9.10.1.
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsExpr (HsLamCase x _ _) = nodeComments x
#else
nodeCommentsHsExpr (HsLamCase x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr HsApp {} = emptyNodeComments
#else
nodeCommentsHsExpr (HsApp x _ _) = nodeComments x
#endif
nodeCommentsHsExpr HsAppType {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (OpApp x _ _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsHsExpr (OpApp x _ _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (NegApp x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsHsExpr (NegApp x _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr HsPar {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsExpr (HsPar x _ _ _) = nodeComments x
#else
nodeCommentsHsExpr (HsPar x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr SectionL {} = emptyNodeComments
#else
nodeCommentsHsExpr (SectionL x _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr SectionR {} = emptyNodeComments
#else
nodeCommentsHsExpr (SectionR x _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (ExplicitTuple x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsHsExpr (ExplicitTuple x _ _) = nodeComments x
#endif
nodeCommentsHsExpr (ExplicitSum x _ _ _) = nodeComments x
nodeCommentsHsExpr (HsCase x _ _) = nodeComments x
nodeCommentsHsExpr (HsIf x _ _ _) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (HsMultiIf x _) = mconcat $ fmap nodeComments x
#else
nodeCommentsHsExpr (HsMultiIf x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr HsLet {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsExpr (HsLet x _ _ _ _) = nodeComments x
#else
nodeCommentsHsExpr (HsLet x _ _) = nodeComments x
#endif
nodeCommentsHsExpr (HsDo x _ _) = nodeComments x
nodeCommentsHsExpr (ExplicitList x _) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr RecordCon {..} = mconcat $ fmap nodeComments rcon_ext
#else
nodeCommentsHsExpr RecordCon {..} = nodeComments rcon_ext
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr RecordUpd {..} = mconcat $ fmap nodeComments rupd_ext
#else
nodeCommentsHsExpr RecordUpd {..} = nodeComments rupd_ext
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr HsGetField {} = emptyNodeComments
#else
nodeCommentsHsExpr HsGetField {..} = nodeComments gf_ext
#endif
nodeCommentsHsExpr HsProjection {..} = nodeComments proj_ext
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (ExprWithTySig x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsHsExpr (ExprWithTySig x _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (ArithSeq x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsHsExpr (ArithSeq x _ _) = nodeComments x
#endif
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
nodeCommentsHsExpr (HsBracket x _) = nodeComments x
nodeCommentsHsExpr HsRnBracketOut {} = notUsedInParsedStage
nodeCommentsHsExpr HsTcBracketOut {} = notUsedInParsedStage
#endif
#if !MIN_VERSION_ghc_lib_parser(9,6,1)
nodeCommentsHsExpr (HsSpliceE x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (HsProc x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsHsExpr (HsProc x _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (HsStatic x _) = mconcat $ fmap nodeComments x
#else
nodeCommentsHsExpr (HsStatic x _) = nodeComments x
#endif
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsExpr HsTick {} = emptyNodeComments
nodeCommentsHsExpr HsBinTick {} = emptyNodeComments
#endif
nodeCommentsHsExpr HsPragE {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsExpr HsRecSel {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (HsTypedBracket x _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (HsUntypedBracket x _) = mconcat $ fmap nodeComments x
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsExpr (HsTypedBracket x _) = nodeComments x
nodeCommentsHsExpr (HsUntypedBracket x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (HsTypedSplice x _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr HsUntypedSplice {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsHsExpr (HsTypedSplice (x, y) _) = nodeComments x <> nodeComments y
nodeCommentsHsExpr (HsUntypedSplice x _) = nodeComments x
#endif
instance CommentExtraction (HsSigType GhcPs) where
  nodeComments HsSig {} = emptyNodeComments

instance CommentExtraction HsSigType' where
  nodeComments (HsSigType' _ _ HsSig {}) = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (ConDecl GhcPs) where
  nodeComments ConDeclGADT {..} = mconcat $ fmap nodeComments $ snd con_g_ext
  nodeComments ConDeclH98 {..} = mconcat $ fmap nodeComments con_ext
#else
instance CommentExtraction (ConDecl GhcPs) where
  nodeComments ConDeclGADT {..} = nodeComments con_g_ext
  nodeComments ConDeclH98 {..} = nodeComments con_ext
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (Match GhcPs a) where
  nodeComments Match {..} = mconcat $ fmap nodeComments m_ext
#else
instance CommentExtraction (Match GhcPs a) where
  nodeComments Match {..} = nodeComments m_ext
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction
           (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  nodeComments LastStmt {} = emptyNodeComments
  nodeComments (BindStmt x _ _) = mconcat $ fmap nodeComments x
  nodeComments ApplicativeStmt {} = emptyNodeComments
  nodeComments BodyStmt {} = emptyNodeComments
  nodeComments (LetStmt x _) = mconcat $ fmap nodeComments x
  nodeComments ParStmt {} = emptyNodeComments
  nodeComments TransStmt {..} = mconcat $ fmap nodeComments trS_ext
  nodeComments RecStmt {..} = nodeComments recS_ext
#else
instance CommentExtraction
           (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  nodeComments LastStmt {} = emptyNodeComments
  nodeComments (BindStmt x _ _) = nodeComments x
  nodeComments ApplicativeStmt {} = emptyNodeComments
  nodeComments BodyStmt {} = emptyNodeComments
  nodeComments (LetStmt x _) = nodeComments x
  nodeComments ParStmt {} = emptyNodeComments
  nodeComments TransStmt {..} = nodeComments trS_ext
  nodeComments RecStmt {..} = nodeComments recS_ext
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction
           (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs))) where
  nodeComments LastStmt {} = emptyNodeComments
  nodeComments (BindStmt x _ _) = mconcat $ fmap nodeComments x
  nodeComments ApplicativeStmt {} = emptyNodeComments
  nodeComments BodyStmt {} = emptyNodeComments
  nodeComments (LetStmt x _) = mconcat $ fmap nodeComments x
  nodeComments ParStmt {} = emptyNodeComments
  nodeComments TransStmt {..} = mconcat $ fmap nodeComments trS_ext
  nodeComments RecStmt {..} = nodeComments recS_ext
#else
instance CommentExtraction
           (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs))) where
  nodeComments LastStmt {} = emptyNodeComments
  nodeComments (BindStmt x _ _) = nodeComments x
  nodeComments ApplicativeStmt {} = emptyNodeComments
  nodeComments BodyStmt {} = emptyNodeComments
  nodeComments (LetStmt x _) = nodeComments x
  nodeComments ParStmt {} = emptyNodeComments
  nodeComments TransStmt {..} = nodeComments trS_ext
  nodeComments RecStmt {..} = nodeComments recS_ext
#endif
instance CommentExtraction StmtLRInsideVerticalList where
  nodeComments (StmtLRInsideVerticalList x) = nodeComments x

-- | For pattern matching.
instance CommentExtraction
           (HsRecFields GhcPs (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  nodeComments HsRecFields {} = emptyNodeComments

-- | For record updates
instance CommentExtraction
           (HsRecFields GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  nodeComments HsRecFields {} = emptyNodeComments

instance CommentExtraction (HsType GhcPs) where
  nodeComments = nodeComments . HsType' HsTypeForNormalDecl HsTypeNoDir
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction HsType' where
  nodeComments (HsType' _ _ HsForAllTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ HsQualTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ (HsTyVar x _ _)) = mconcat $ fmap nodeComments x
  nodeComments (HsType' _ _ HsAppTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ HsAppKindTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ HsFunTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ (HsListTy x _)) = nodeComments x
  nodeComments (HsType' _ _ (HsTupleTy x _ _)) = nodeComments x
  nodeComments (HsType' _ _ (HsSumTy x _)) = nodeComments x
  nodeComments (HsType' _ _ HsOpTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ (HsParTy x _)) = nodeComments x
  nodeComments (HsType' _ _ (HsIParamTy x _ _)) = mconcat $ fmap nodeComments x
  nodeComments (HsType' _ _ HsStarTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ (HsKindSig x _ _)) = mconcat $ fmap nodeComments x
  nodeComments (HsType' _ _ HsSpliceTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ (HsDocTy x _ _)) = mconcat $ fmap nodeComments x
  nodeComments (HsType' _ _ (HsBangTy x _ _)) = mconcat $ fmap nodeComments x
  nodeComments (HsType' _ _ (HsRecTy x _)) = nodeComments x
  nodeComments (HsType' _ _ (HsExplicitListTy x _ _)) =
    mconcat $ fmap nodeComments x
  nodeComments (HsType' _ _ (HsExplicitTupleTy x _)) =
    mconcat $ fmap nodeComments x
  nodeComments (HsType' _ _ HsTyLit {}) = emptyNodeComments
  nodeComments (HsType' _ _ HsWildCardTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ XHsType {}) = emptyNodeComments
#else
instance CommentExtraction HsType' where
  nodeComments (HsType' _ _ HsForAllTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ HsQualTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ (HsTyVar x _ _)) = nodeComments x
  nodeComments (HsType' _ _ HsAppTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ HsAppKindTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ (HsFunTy x _ _ _)) = nodeComments x
  nodeComments (HsType' _ _ (HsListTy x _)) = nodeComments x
  nodeComments (HsType' _ _ (HsTupleTy x _ _)) = nodeComments x
  nodeComments (HsType' _ _ (HsSumTy x _)) = nodeComments x
  nodeComments (HsType' _ _ HsOpTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ (HsParTy x _)) = nodeComments x
  nodeComments (HsType' _ _ (HsIParamTy x _ _)) = nodeComments x
  nodeComments (HsType' _ _ HsStarTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ (HsKindSig x _ _)) = nodeComments x
  nodeComments (HsType' _ _ HsSpliceTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ (HsDocTy x _ _)) = nodeComments x
  nodeComments (HsType' _ _ (HsBangTy x _ _)) = nodeComments x
  nodeComments (HsType' _ _ (HsRecTy x _)) = nodeComments x
  nodeComments (HsType' _ _ (HsExplicitListTy x _ _)) = nodeComments x
  nodeComments (HsType' _ _ (HsExplicitTupleTy x _)) = nodeComments x
  nodeComments (HsType' _ _ HsTyLit {}) = emptyNodeComments
  nodeComments (HsType' _ _ HsWildCardTy {}) = emptyNodeComments
  nodeComments (HsType' _ _ XHsType {}) = emptyNodeComments
#endif
instance CommentExtraction (GRHSs GhcPs a) where
  nodeComments GRHSs {..} = NodeComments {..}
    where
      commentsBefore = priorComments grhssExt
      commentsOnSameLine = []
      commentsAfter = getFollowingComments grhssExt

instance CommentExtraction GRHSsExpr where
  nodeComments (GRHSsExpr {..}) = nodeComments grhssExpr
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (HsMatchContext (GenLocated SrcSpanAnnN RdrName)) where
  nodeComments = nodeCommentsMatchContext

nodeCommentsMatchContext ::
     HsMatchContext (GenLocated SrcSpanAnnN RdrName) -> NodeComments
nodeCommentsMatchContext FunRhs {} = emptyNodeComments
#if !MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsMatchContext LambdaExpr {} = emptyNodeComments
#endif
nodeCommentsMatchContext CaseAlt {} = emptyNodeComments
nodeCommentsMatchContext IfAlt {} = emptyNodeComments
nodeCommentsMatchContext ArrowMatchCtxt {} = emptyNodeComments
nodeCommentsMatchContext PatBindRhs {} = emptyNodeComments
nodeCommentsMatchContext PatBindGuards {} = emptyNodeComments
nodeCommentsMatchContext RecUpd {} = emptyNodeComments
nodeCommentsMatchContext StmtCtxt {} = emptyNodeComments
nodeCommentsMatchContext ThPatSplice {} = emptyNodeComments
nodeCommentsMatchContext ThPatQuote {} = emptyNodeComments
nodeCommentsMatchContext PatSyn {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9,4,1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsMatchContext LamCaseAlt {} = emptyNodeComments
#endif
#else
instance CommentExtraction (HsMatchContext GhcPs) where
  nodeComments = nodeCommentsMatchContext

nodeCommentsMatchContext :: HsMatchContext GhcPs -> NodeComments
nodeCommentsMatchContext FunRhs {} = emptyNodeComments
#if !MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsMatchContext LambdaExpr {} = emptyNodeComments
#endif
nodeCommentsMatchContext CaseAlt {} = emptyNodeComments
nodeCommentsMatchContext IfAlt {} = emptyNodeComments
nodeCommentsMatchContext ArrowMatchCtxt {} = emptyNodeComments
nodeCommentsMatchContext PatBindRhs {} = emptyNodeComments
nodeCommentsMatchContext PatBindGuards {} = emptyNodeComments
nodeCommentsMatchContext RecUpd {} = emptyNodeComments
nodeCommentsMatchContext StmtCtxt {} = emptyNodeComments
nodeCommentsMatchContext ThPatSplice {} = emptyNodeComments
nodeCommentsMatchContext ThPatQuote {} = emptyNodeComments
nodeCommentsMatchContext PatSyn {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9,4,1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsMatchContext LamCaseAlt {} = emptyNodeComments
#endif
#endif
instance CommentExtraction (ParStmtBlock GhcPs GhcPs) where
  nodeComments ParStmtBlock {} = emptyNodeComments

instance CommentExtraction ParStmtBlockInsideVerticalList where
  nodeComments (ParStmtBlockInsideVerticalList x) = nodeComments x

instance CommentExtraction RdrName where
  nodeComments Unqual {} = emptyNodeComments
  nodeComments Qual {} = emptyNodeComments
  nodeComments Orig {} = emptyNodeComments
  nodeComments Exact {} = emptyNodeComments

instance CommentExtraction (GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  nodeComments = nodeComments . GRHSExpr GRHSExprNormal

instance CommentExtraction GRHSExpr where
  nodeComments (GRHSExpr {grhsExpr = (GRHS x _ _)}) = nodeComments x

instance CommentExtraction GRHSProc where
  nodeComments (GRHSProc (GRHS x _ _)) = nodeComments x

instance CommentExtraction EpaCommentTok where
  nodeComments = const emptyNodeComments

instance CommentExtraction (SpliceDecl GhcPs) where
  nodeComments SpliceDecl {} = emptyNodeComments
#if !MIN_VERSION_ghc_lib_parser(9,6,1)
instance CommentExtraction (HsSplice GhcPs) where
  nodeComments (HsTypedSplice x _ _ _) = nodeComments x
  nodeComments (HsUntypedSplice x _ _ _) = nodeComments x
  nodeComments HsQuasiQuote {} = emptyNodeComments
  nodeComments HsSpliced {} = emptyNodeComments
#endif
instance CommentExtraction (Pat GhcPs) where
  nodeComments = nodeCommentsPat

instance CommentExtraction PatInsidePatDecl where
  nodeComments (PatInsidePatDecl x) = nodeComments x

nodeCommentsPat :: Pat GhcPs -> NodeComments
nodeCommentsPat WildPat {} = emptyNodeComments
nodeCommentsPat VarPat {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPat (LazyPat x _) = mconcat $ fmap nodeComments x
#else
nodeCommentsPat (LazyPat x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPat AsPat {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsPat (AsPat x _ _ _) = nodeComments x
#else
nodeCommentsPat (AsPat x _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPat ParPat {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsPat (ParPat x _ _ _) = nodeComments x
#else
nodeCommentsPat (ParPat x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPat (BangPat x _) = mconcat $ fmap nodeComments x
#else
nodeCommentsPat (BangPat x _) = nodeComments x
#endif
nodeCommentsPat (ListPat x _) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPat (TuplePat x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsPat (TuplePat x _ _) = nodeComments x
#endif
nodeCommentsPat (SumPat x _ _ _) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPat ConPat {..} = mconcat $ fmap nodeComments pat_con_ext
#else
nodeCommentsPat ConPat {..} = nodeComments pat_con_ext
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPat (ViewPat x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsPat (ViewPat x _ _) = nodeComments x
#endif
nodeCommentsPat SplicePat {} = emptyNodeComments
nodeCommentsPat LitPat {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPat (NPat x _ _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsPat (NPat x _ _ _) = nodeComments x
#endif
nodeCommentsPat (NPlusKPat x _ _ _ _ _) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPat (SigPat x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsPat (SigPat x _ _) = nodeComments x
#endif
instance CommentExtraction RecConPat where
  nodeComments (RecConPat x) = nodeComments x
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
instance CommentExtraction (HsBracket GhcPs) where
  nodeComments ExpBr {} = emptyNodeComments
  nodeComments PatBr {} = emptyNodeComments
  nodeComments DecBrL {} = emptyNodeComments
  nodeComments DecBrG {} = emptyNodeComments
  nodeComments TypBr {} = emptyNodeComments
  nodeComments VarBr {} = emptyNodeComments
  nodeComments TExpBr {} = emptyNodeComments
#endif
instance CommentExtraction SigBindFamily where
  nodeComments (Sig x) = nodeComments x
  nodeComments (Bind x) = nodeComments x
  nodeComments (TypeFamily x) = nodeComments x
  nodeComments (TyFamInst x) = nodeComments x
  nodeComments (DataFamInst x) = nodeComments x

instance CommentExtraction EpaComment where
  nodeComments EpaComment {} = emptyNodeComments
#if !MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction Anchor where
  nodeComments Anchor {} = emptyNodeComments

instance CommentExtraction (SrcAnn a) where
  nodeComments (SrcSpanAnn ep _) = nodeComments ep
#endif
instance CommentExtraction SrcSpan where
  nodeComments RealSrcSpan {} = emptyNodeComments
  nodeComments UnhelpfulSpan {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (EpAnn a) where
  nodeComments (EpAnn ann _ cs) = NodeComments {..}
    where
      commentsBefore = priorComments cs
      commentsOnSameLine = filter isCommentOnSameLine $ getFollowingComments cs
      commentsAfter =
        filter (not . isCommentOnSameLine) $ getFollowingComments cs
      isCommentOnSameLine (L comAnn _) =
        srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)
#else
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
instance CommentExtraction (HsLocalBindsLR GhcPs GhcPs) where
  nodeComments (HsValBinds x _) = nodeComments x
  nodeComments (HsIPBinds x _) = nodeComments x
  nodeComments EmptyLocalBinds {} = emptyNodeComments

instance CommentExtraction (HsValBindsLR GhcPs GhcPs) where
  nodeComments ValBinds {} = emptyNodeComments
  nodeComments XValBindsLR {} = notUsedInParsedStage
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (HsTupArg GhcPs) where
  nodeComments Present {} = emptyNodeComments
  nodeComments (Missing x) = nodeComments x
#else
instance CommentExtraction (HsTupArg GhcPs) where
  nodeComments (Present x _) = nodeComments x
  nodeComments (Missing x) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
instance CommentExtraction RecConField where
  nodeComments (RecConField x) = nodeComments x
#else
-- | For pattern matching against a record.
instance CommentExtraction
           (HsRecField' (FieldOcc GhcPs) (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  nodeComments HsRecField {..} = nodeComments hsRecFieldAnn

-- | For record updates.
instance CommentExtraction
           (HsRecField' (FieldOcc GhcPs) (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  nodeComments HsRecField {..} = nodeComments hsRecFieldAnn
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction
           (HsFieldBind
              (GenLocated SrcSpanAnnA (FieldOcc GhcPs))
              (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  nodeComments HsFieldBind {..} = mconcat $ fmap nodeComments hfbAnn

instance CommentExtraction
           (HsFieldBind
              (GenLocated SrcSpanAnnA (FieldOcc GhcPs))
              (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  nodeComments HsFieldBind {..} = mconcat $ fmap nodeComments hfbAnn
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
-- | For pattern matchings against records.
instance CommentExtraction
           (HsFieldBind
              (GenLocated (SrcAnn NoEpAnns) (FieldOcc GhcPs))
              (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  nodeComments HsFieldBind {..} = nodeComments hfbAnn

-- | For record updates.
instance CommentExtraction
           (HsFieldBind
              (GenLocated (SrcAnn NoEpAnns) (FieldOcc GhcPs))
              (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  nodeComments HsFieldBind {..} = nodeComments hfbAnn
#else
instance CommentExtraction RecConField where
  nodeComments (RecConField x) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance CommentExtraction (FieldOcc GhcPs) where
  nodeComments FieldOcc {} = emptyNodeComments
#else
instance CommentExtraction (FieldOcc GhcPs) where
  nodeComments FieldOcc {} = emptyNodeComments
#endif
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
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (ConDeclField GhcPs) where
  nodeComments ConDeclField {..} = mconcat $ fmap nodeComments cd_fld_ext
#else
instance CommentExtraction (ConDeclField GhcPs) where
  nodeComments ConDeclField {..} = nodeComments cd_fld_ext
#endif
instance CommentExtraction InfixExpr where
  nodeComments (InfixExpr x) = nodeComments x

instance CommentExtraction InfixApp where
  nodeComments InfixApp {} = emptyNodeComments

instance CommentExtraction (BooleanFormula a) where
  nodeComments Var {} = emptyNodeComments
  nodeComments And {} = emptyNodeComments
  nodeComments Or {} = emptyNodeComments
  nodeComments Parens {} = emptyNodeComments

instance CommentExtraction (FieldLabelStrings GhcPs) where
  nodeComments FieldLabelStrings {} = emptyNodeComments

instance CommentExtraction (AmbiguousFieldOcc GhcPs) where
  nodeComments Unambiguous {} = emptyNodeComments
  nodeComments Ambiguous {} = emptyNodeComments

instance CommentExtraction (ImportDecl GhcPs) where
  nodeComments ImportDecl {..} = nodeComments ideclExt
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance CommentExtraction XImportDeclPass where
  nodeComments XImportDeclPass {..} = nodeComments ideclAnn
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (HsDerivingClause GhcPs) where
  nodeComments HsDerivingClause {..} =
    mconcat $ fmap nodeComments deriv_clause_ext
#else
instance CommentExtraction (HsDerivingClause GhcPs) where
  nodeComments HsDerivingClause {..} = nodeComments deriv_clause_ext
#endif
instance CommentExtraction (DerivClauseTys GhcPs) where
  nodeComments DctSingle {} = emptyNodeComments
  nodeComments DctMulti {} = emptyNodeComments

instance CommentExtraction OverlapMode where
  nodeComments NoOverlap {} = emptyNodeComments
  nodeComments Overlappable {} = emptyNodeComments
  nodeComments Overlapping {} = emptyNodeComments
  nodeComments Overlaps {} = emptyNodeComments
  nodeComments Incoherent {} = emptyNodeComments

instance CommentExtraction StringLiteral where
  nodeComments StringLiteral {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
-- | This instance is for type family declarations inside a class declaration.
instance CommentExtraction (FamilyDecl GhcPs) where
  nodeComments FamilyDecl {..} = mconcat $ fmap nodeComments fdExt
#else
-- | This instance is for type family declarations inside a class declaration.
instance CommentExtraction (FamilyDecl GhcPs) where
  nodeComments FamilyDecl {..} = nodeComments fdExt
#endif
instance CommentExtraction (FamilyResultSig GhcPs) where
  nodeComments NoSig {} = emptyNodeComments
  nodeComments KindSig {} = emptyNodeComments
  nodeComments TyVarSig {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (HsTyVarBndr a GhcPs) where
  nodeComments (UserTyVar x _ _) = mconcat $ fmap nodeComments x
  nodeComments (KindedTyVar x _ _ _) = mconcat $ fmap nodeComments x
#else
instance CommentExtraction (HsTyVarBndr a GhcPs) where
  nodeComments (UserTyVar x _ _) = nodeComments x
  nodeComments (KindedTyVar x _ _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (InjectivityAnn GhcPs) where
  nodeComments (InjectivityAnn x _ _) = mconcat $ fmap nodeComments x
#else
instance CommentExtraction (InjectivityAnn GhcPs) where
  nodeComments (InjectivityAnn x _ _) = nodeComments x
#endif
instance CommentExtraction (ArithSeqInfo GhcPs) where
  nodeComments From {} = emptyNodeComments
  nodeComments FromThen {} = emptyNodeComments
  nodeComments FromTo {} = emptyNodeComments
  nodeComments FromThenTo {} = emptyNodeComments

instance CommentExtraction (HsForAllTelescope GhcPs) where
  nodeComments HsForAllVis {..} = nodeComments hsf_xvis
  nodeComments HsForAllInvis {..} = nodeComments hsf_xinvis

instance CommentExtraction InfixOp where
  nodeComments (InfixOp x) = nodeComments x

instance CommentExtraction PrefixOp where
  nodeComments (PrefixOp x) = nodeComments x

instance CommentExtraction Context where
  nodeComments Context {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance CommentExtraction HorizontalContext where
  nodeComments HorizontalContext {} = emptyNodeComments

instance CommentExtraction VerticalContext where
  nodeComments VerticalContext {} = emptyNodeComments
#else
instance CommentExtraction HorizontalContext where
  nodeComments HorizontalContext {} = emptyNodeComments

instance CommentExtraction VerticalContext where
  nodeComments VerticalContext {} = emptyNodeComments
#endif
-- Wrap a value of this type with 'ModulenameWithPrefix' to print it with
-- the "module " prefix.
instance CommentExtraction ModuleName where
  nodeComments = const emptyNodeComments

instance CommentExtraction ModuleNameWithPrefix where
  nodeComments ModuleNameWithPrefix {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (IE GhcPs) where
  nodeComments IEVar {} = emptyNodeComments
  nodeComments (IEThingAbs _ x _) = nodeComments x
  nodeComments (IEThingAll _ x _) = nodeComments x
  nodeComments (IEThingWith _ x _ _ _) = nodeComments x
  nodeComments (IEModuleContents (x, y) _) =
    mconcat $ maybeToList (fmap nodeComments x) <> fmap nodeComments y
  nodeComments IEGroup {} = emptyNodeComments
  nodeComments IEDoc {} = emptyNodeComments
  nodeComments IEDocNamed {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 8, 1)
instance CommentExtraction (IE GhcPs) where
  nodeComments IEVar {} = emptyNodeComments
  nodeComments (IEThingAbs (_, x) _) = nodeComments x
  nodeComments (IEThingAll (_, x) _) = nodeComments x
  nodeComments (IEThingWith (_, x) _ _ _) = nodeComments x
  nodeComments (IEModuleContents (_, x) _) = nodeComments x
  nodeComments IEGroup {} = emptyNodeComments
  nodeComments IEDoc {} = emptyNodeComments
  nodeComments IEDocNamed {} = emptyNodeComments
#else
instance CommentExtraction (IE GhcPs) where
  nodeComments IEVar {} = emptyNodeComments
  nodeComments (IEThingAbs x _) = nodeComments x
  nodeComments (IEThingAll x _) = nodeComments x
  nodeComments (IEThingWith x _ _ _) = nodeComments x
  nodeComments (IEModuleContents x _) = nodeComments x
  nodeComments IEGroup {} = emptyNodeComments
  nodeComments IEDoc {} = emptyNodeComments
  nodeComments IEDocNamed {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction
           (FamEqn GhcPs (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  nodeComments FamEqn {..} = mconcat $ fmap nodeComments feqn_ext
#else
instance CommentExtraction
           (FamEqn GhcPs (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  nodeComments FamEqn {..} = nodeComments feqn_ext
#endif
instance CommentExtraction FamEqn' where
  nodeComments FamEqn' {..} = nodeComments famEqn
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
-- | Pretty-print a data instance.
instance CommentExtraction (FamEqn GhcPs (HsDataDefn GhcPs)) where
  nodeComments FamEqn {..} = mconcat $ fmap nodeComments feqn_ext
#else
-- | Pretty-print a data instance.
instance CommentExtraction (FamEqn GhcPs (HsDataDefn GhcPs)) where
  nodeComments FamEqn {..} = nodeComments feqn_ext
#endif
-- | HsArg (LHsType GhcPs) (LHsType GhcPs)
#if MIN_VERSION_ghc_lib_parser(9,8,1)
instance CommentExtraction
           (HsArg
              GhcPs
              (GenLocated SrcSpanAnnA (HsType GhcPs))
              (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  nodeComments HsValArg {} = emptyNodeComments
  nodeComments HsTypeArg {} = emptyNodeComments
  nodeComments HsArgPar {} = emptyNodeComments
#else
instance CommentExtraction
           (HsArg
              (GenLocated SrcSpanAnnA (HsType GhcPs))
              (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  nodeComments HsValArg {} = emptyNodeComments
  nodeComments HsTypeArg {} = emptyNodeComments
  nodeComments HsArgPar {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance CommentExtraction (HsQuote GhcPs) where
  nodeComments ExpBr {} = emptyNodeComments
  nodeComments PatBr {} = emptyNodeComments
  nodeComments DecBrL {} = emptyNodeComments
  nodeComments DecBrG {} = emptyNodeComments
  nodeComments TypBr {} = emptyNodeComments
  nodeComments VarBr {} = emptyNodeComments
#endif

#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (WarnDecls GhcPs) where
  nodeComments Warnings {..} = mconcat $ fmap nodeComments $ fst wd_ext
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance CommentExtraction (WarnDecls GhcPs) where
  nodeComments Warnings {..} = nodeComments $ fst wd_ext
#else
instance CommentExtraction (WarnDecls GhcPs) where
  nodeComments Warnings {..} = nodeComments wd_ext
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (WarnDecl GhcPs) where
  nodeComments (Warning (_, x) _ _) = mconcat $ fmap nodeComments x
#else
instance CommentExtraction (WarnDecl GhcPs) where
  nodeComments (Warning x _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance CommentExtraction (WithHsDocIdentifiers StringLiteral GhcPs) where
  nodeComments WithHsDocIdentifiers {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance CommentExtraction (IEWrappedName GhcPs) where
  nodeComments IEName {} = emptyNodeComments
  nodeComments IEPattern {} = emptyNodeComments
  nodeComments IEType {} = emptyNodeComments
#else
-- | 'Pretty' for 'LIEWrappedName (IdP GhcPs)'
instance CommentExtraction (IEWrappedName RdrName) where
  nodeComments IEName {} = emptyNodeComments
  nodeComments IEPattern {} = emptyNodeComments
  nodeComments IEType {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance CommentExtraction (DotFieldOcc GhcPs) where
  nodeComments DotFieldOcc {..} = nodeComments dfoExt
#else
instance CommentExtraction (HsFieldLabel GhcPs) where
  nodeComments HsFieldLabel {..} = nodeComments hflExt
#endif

#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (RuleDecls GhcPs) where
  nodeComments HsRules {..} = mconcat $ fmap nodeComments $ fst rds_ext
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance CommentExtraction (RuleDecls GhcPs) where
  nodeComments HsRules {..} = nodeComments $ fst rds_ext
#else
instance CommentExtraction (RuleDecls GhcPs) where
  nodeComments HsRules {..} = nodeComments rds_ext
#endif

#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance CommentExtraction (RuleDecl GhcPs) where
  nodeComments HsRule {..} = nodeComments $ fst rd_ext
#else
instance CommentExtraction (RuleDecl GhcPs) where
  nodeComments HsRule {..} = nodeComments rd_ext
#endif
instance CommentExtraction OccName where
  nodeComments = const emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (DerivDecl GhcPs) where
  nodeComments DerivDecl {deriv_ext = (x, xs)} =
    mconcat $ maybeToList (fmap nodeComments x) <> fmap nodeComments xs
#else
instance CommentExtraction (DerivDecl GhcPs) where
  nodeComments DerivDecl {..} = nodeComments deriv_ext
#endif
-- | 'Pretty' for 'LHsSigWcType GhcPs'.
instance CommentExtraction
           (HsWildCardBndrs GhcPs (GenLocated SrcSpanAnnA (HsSigType GhcPs))) where
  nodeComments HsWC {} = emptyNodeComments

-- | 'Pretty' for 'LHsWcType'
instance CommentExtraction
           (HsWildCardBndrs GhcPs (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  nodeComments HsWC {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (StandaloneKindSig GhcPs) where
  nodeComments (StandaloneKindSig x _ _) = mconcat $ fmap nodeComments x
#else
instance CommentExtraction (StandaloneKindSig GhcPs) where
  nodeComments (StandaloneKindSig x _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (DefaultDecl GhcPs) where
  nodeComments (DefaultDecl x _) = mconcat $ fmap nodeComments x
#else
instance CommentExtraction (DefaultDecl GhcPs) where
  nodeComments (DefaultDecl x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (ForeignDecl GhcPs) where
  nodeComments ForeignImport {..} = mconcat $ fmap nodeComments fd_i_ext
  nodeComments ForeignExport {..} = mconcat $ fmap nodeComments fd_e_ext
#else
instance CommentExtraction (ForeignDecl GhcPs) where
  nodeComments ForeignImport {..} = nodeComments fd_i_ext
  nodeComments ForeignExport {..} = nodeComments fd_e_ext
#endif
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance CommentExtraction (ForeignImport GhcPs) where
  nodeComments CImport {} = emptyNodeComments
#else
instance CommentExtraction ForeignImport where
  nodeComments CImport {} = emptyNodeComments
#endif

#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance CommentExtraction (ForeignExport GhcPs) where
  nodeComments CExport {} = emptyNodeComments
#else
instance CommentExtraction ForeignExport where
  nodeComments CExport {} = emptyNodeComments
#endif
instance CommentExtraction CExportSpec where
  nodeComments CExportStatic {} = emptyNodeComments

instance CommentExtraction Safety where
  nodeComments PlaySafe = emptyNodeComments
  nodeComments PlayInterruptible = emptyNodeComments
  nodeComments PlayRisky = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance CommentExtraction (AnnDecl GhcPs) where
  nodeComments (HsAnnotation (x, _) _ _) = nodeComments x
#else
instance CommentExtraction (AnnDecl GhcPs) where
  nodeComments (HsAnnotation x _ _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (RoleAnnotDecl GhcPs) where
  nodeComments (RoleAnnotDecl x _ _) = mconcat $ fmap nodeComments x
#else
instance CommentExtraction (RoleAnnotDecl GhcPs) where
  nodeComments (RoleAnnotDecl x _ _) = nodeComments x
#endif
instance CommentExtraction Role where
  nodeComments Nominal = emptyNodeComments
  nodeComments Representational = emptyNodeComments
  nodeComments Phantom = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (TyFamInstDecl GhcPs) where
  nodeComments TyFamInstDecl {..} = mconcat $ fmap nodeComments tfid_xtn
#else
instance CommentExtraction (TyFamInstDecl GhcPs) where
  nodeComments TyFamInstDecl {..} = nodeComments tfid_xtn
#endif
instance CommentExtraction TopLevelTyFamInstDecl where
  nodeComments (TopLevelTyFamInstDecl x) = nodeComments x

instance CommentExtraction (DataFamInstDecl GhcPs) where
  nodeComments DataFamInstDecl {} = emptyNodeComments

instance CommentExtraction DataFamInstDecl' where
  nodeComments DataFamInstDecl' {..} = nodeComments dataFamInstDecl
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (PatSynBind GhcPs GhcPs) where
  nodeComments PSB {..} = mconcat $ fmap nodeComments psb_ext
#else
instance CommentExtraction (PatSynBind GhcPs GhcPs) where
  nodeComments PSB {..} = nodeComments psb_ext
#endif
-- | 'Pretty' for 'HsPatSynDetails'.
instance CommentExtraction
           (HsConDetails
              Void
              (GenLocated SrcSpanAnnN RdrName)
              [RecordPatSynField GhcPs]) where
  nodeComments PrefixCon {} = emptyNodeComments
  nodeComments RecCon {} = emptyNodeComments
  nodeComments InfixCon {} = emptyNodeComments

instance CommentExtraction (FixitySig GhcPs) where
  nodeComments FixitySig {} = emptyNodeComments

instance CommentExtraction Fixity where
  nodeComments Fixity {} = emptyNodeComments

instance CommentExtraction FixityDirection where
  nodeComments InfixL {} = emptyNodeComments
  nodeComments InfixR {} = emptyNodeComments
  nodeComments InfixN {} = emptyNodeComments

instance CommentExtraction InlinePragma where
  nodeComments InlinePragma {} = emptyNodeComments

instance CommentExtraction InlineSpec where
  nodeComments = nodeCommentsInlineSpec

nodeCommentsInlineSpec :: InlineSpec -> NodeComments
nodeCommentsInlineSpec Inline {} = emptyNodeComments
nodeCommentsInlineSpec Inlinable {} = emptyNodeComments
nodeCommentsInlineSpec NoInline {} = emptyNodeComments
nodeCommentsInlineSpec NoUserInlinePrag {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9,4,1)
nodeCommentsInlineSpec Opaque {} = emptyNodeComments
#endif
instance CommentExtraction (HsPatSynDir GhcPs) where
  nodeComments Unidirectional = emptyNodeComments
  nodeComments ImplicitBidirectional = emptyNodeComments
  nodeComments ExplicitBidirectional {} = emptyNodeComments

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
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance CommentExtraction (HsPragE GhcPs) where
  nodeComments (HsPragSCC (x, _) _) = nodeComments x
#else
instance CommentExtraction (HsPragE GhcPs) where
  nodeComments (HsPragSCC x _ _) = nodeComments x
#endif
instance CommentExtraction HsIPName where
  nodeComments HsIPName {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance CommentExtraction (HsTyLit GhcPs) where
  nodeComments HsNumTy {} = emptyNodeComments
  nodeComments HsStrTy {} = emptyNodeComments
  nodeComments HsCharTy {} = emptyNodeComments
#else
instance CommentExtraction HsTyLit where
  nodeComments HsNumTy {} = emptyNodeComments
  nodeComments HsStrTy {} = emptyNodeComments
  nodeComments HsCharTy {} = emptyNodeComments
#endif
instance CommentExtraction (HsPatSigType GhcPs) where
  nodeComments HsPS {..} = nodeComments hsps_ext

instance CommentExtraction (HsIPBinds GhcPs) where
  nodeComments IPBinds {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (IPBind GhcPs) where
  nodeComments (IPBind x _ _) = mconcat $ fmap nodeComments x
#else
instance CommentExtraction (IPBind GhcPs) where
  nodeComments (IPBind x _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (DerivStrategy GhcPs) where
  nodeComments (StockStrategy x) = mconcat $ fmap nodeComments x
  nodeComments (AnyclassStrategy x) = mconcat $ fmap nodeComments x
  nodeComments (NewtypeStrategy x) = mconcat $ fmap nodeComments x
  nodeComments (ViaStrategy x) = nodeComments x
#else
instance CommentExtraction (DerivStrategy GhcPs) where
  nodeComments (StockStrategy x) = nodeComments x
  nodeComments (AnyclassStrategy x) = nodeComments x
  nodeComments (NewtypeStrategy x) = nodeComments x
  nodeComments (ViaStrategy x) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction XViaStrategyPs where
  nodeComments (XViaStrategyPs x _) = mconcat $ fmap nodeComments x
#else
instance CommentExtraction XViaStrategyPs where
  nodeComments (XViaStrategyPs x _) = nodeComments x
#endif
instance CommentExtraction (RecordPatSynField GhcPs) where
  nodeComments RecordPatSynField {} = emptyNodeComments

instance CommentExtraction (HsCmdTop GhcPs) where
  nodeComments HsCmdTop {} = emptyNodeComments

instance CommentExtraction (HsCmd GhcPs) where
  nodeComments = nodeCommentsHsCmd

nodeCommentsHsCmd :: HsCmd GhcPs -> NodeComments
nodeCommentsHsCmd (HsCmdArrApp x _ _ _ _) = nodeComments x
nodeCommentsHsCmd (HsCmdArrForm x _ _ _ _) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsCmd HsCmdApp {} = emptyNodeComments
#else
nodeCommentsHsCmd (HsCmdApp x _ _) = nodeComments x
#endif
nodeCommentsHsCmd HsCmdLam {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsCmd HsCmdPar {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsCmd (HsCmdPar x _ _ _) = nodeComments x
#else
nodeCommentsHsCmd (HsCmdPar x _) = nodeComments x
#endif
nodeCommentsHsCmd (HsCmdCase x _ _) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
-- No HsCmdLamCase since 9.10.1.
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsCmd (HsCmdLamCase x _ _) = nodeComments x
#else
nodeCommentsHsCmd (HsCmdLamCase x _) = nodeComments x
#endif
nodeCommentsHsCmd (HsCmdIf x _ _ _ _) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsCmd HsCmdLet {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsCmd (HsCmdLet x _ _ _ _) = nodeComments x
#else
nodeCommentsHsCmd (HsCmdLet x _ _) = nodeComments x
#endif
nodeCommentsHsCmd (HsCmdDo x _) = nodeComments x

instance CommentExtraction ListComprehension where
  nodeComments ListComprehension {} = emptyNodeComments

instance CommentExtraction DoExpression where
  nodeComments DoExpression {} = emptyNodeComments

instance CommentExtraction LetIn where
  nodeComments LetIn {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (RuleBndr GhcPs) where
  nodeComments (RuleBndr x _) = mconcat $ fmap nodeComments x
  nodeComments (RuleBndrSig x _ _) = mconcat $ fmap nodeComments x
#else
instance CommentExtraction (RuleBndr GhcPs) where
  nodeComments (RuleBndr x _) = nodeComments x
  nodeComments (RuleBndrSig x _ _) = nodeComments x
#endif
instance CommentExtraction CCallConv where
  nodeComments = const emptyNodeComments

instance CommentExtraction HsSrcBang where
  nodeComments HsSrcBang {} = emptyNodeComments

instance CommentExtraction SrcUnpackedness where
  nodeComments SrcUnpack = emptyNodeComments
  nodeComments SrcNoUnpack = emptyNodeComments
  nodeComments NoSrcUnpack = emptyNodeComments

instance CommentExtraction SrcStrictness where
  nodeComments SrcLazy = emptyNodeComments
  nodeComments SrcStrict = emptyNodeComments
  nodeComments NoSrcStrict = emptyNodeComments

instance CommentExtraction (HsOuterSigTyVarBndrs GhcPs) where
  nodeComments HsOuterImplicit {} = emptyNodeComments
  nodeComments HsOuterExplicit {..} = nodeComments hso_xexplicit
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance CommentExtraction FieldLabelString where
  nodeComments = const emptyNodeComments
#endif

#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (HsUntypedSplice GhcPs) where
  nodeComments (HsUntypedSpliceExpr x _) = mconcat $ fmap nodeComments x
  nodeComments HsQuasiQuote {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance CommentExtraction (HsUntypedSplice GhcPs) where
  nodeComments (HsUntypedSpliceExpr x _) = nodeComments x
  nodeComments HsQuasiQuote {} = emptyNodeComments
#endif

#if MIN_VERSION_ghc_lib_parser(9,8,1)
instance CommentExtraction (LHsRecUpdFields GhcPs) where
  nodeComments RegularRecUpdFields {} = emptyNodeComments
  nodeComments OverloadedRecUpdFields {} = emptyNodeComments
#endif
instance CommentExtraction AddEpAnn where
  nodeComments (AddEpAnn _ x) = nodeComments x

instance CommentExtraction EpaLocation where
  nodeComments EpaSpan {} = emptyNodeComments
  nodeComments (EpaDelta _ x) = mconcat $ fmap nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (EpaLocation' NoComments) where
  nodeComments EpaSpan {} = emptyNodeComments
  nodeComments EpaDelta {} = emptyNodeComments
#endif
instance CommentExtraction AnnPragma where
  nodeComments AnnPragma {..} =
    mconcat $ fmap nodeComments $ apr_open : apr_close : apr_rest

instance CommentExtraction HsRuleAnn where
  nodeComments HsRuleAnn {..} =
    mconcat $ f ra_tyanns : f ra_tmanns : fmap nodeComments ra_rest
    where
      f (Just (x, y)) = mconcat $ fmap nodeComments [x, y]
      f Nothing = emptyNodeComments

instance CommentExtraction AnnFieldLabel where
  nodeComments AnnFieldLabel {afDot = Just x} = nodeComments x
  nodeComments AnnFieldLabel {afDot = Nothing} = emptyNodeComments

instance CommentExtraction EpAnnSumPat where
  nodeComments EpAnnSumPat {..} =
    mconcat
      $ fmap nodeComments sumPatParens
          <> fmap nodeComments sumPatVbarsBefore
          <> fmap nodeComments sumPatVbarsAfter
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction AnnList where
  nodeComments AnnList {..} = mconcat [a, b, c, d, e]
    where
      a = maybe mempty nodeComments al_anchor
      b = maybe mempty nodeComments al_open
      c = maybe mempty nodeComments al_close
      d = mconcat $ fmap nodeComments al_rest
      e = mconcat $ fmap nodeComments al_trailing

instance CommentExtraction TrailingAnn where
  nodeComments AddSemiAnn {..} = nodeComments ta_location
  nodeComments AddCommaAnn {..} = nodeComments ta_location
  nodeComments AddVbarAnn {..} = nodeComments ta_location
  nodeComments AddDarrowAnn {..} = nodeComments ta_location
  nodeComments AddDarrowUAnn {..} = nodeComments ta_location

instance CommentExtraction AnnParen where
  nodeComments AnnParen {..} = mconcat $ fmap nodeComments [ap_open, ap_close]
#endif
instance CommentExtraction AnnProjection where
  nodeComments AnnProjection {..} =
    mconcat $ fmap nodeComments [apOpen, apClose]

instance CommentExtraction AnnsIf where
  nodeComments AnnsIf {..} =
    mconcat
      $ fmap nodeComments
      $ aiIf
          : aiThen
          : aiElse
          : (maybeToList aiThenSemi <> maybeToList aiElseSemi)

instance CommentExtraction EpAnnHsCase where
  nodeComments EpAnnHsCase {..} =
    mconcat
      $ nodeComments hsCaseAnnCase
          : nodeComments hsCaseAnnOf
          : fmap nodeComments hsCaseAnnsRest

instance CommentExtraction AnnExplicitSum where
  nodeComments AnnExplicitSum {..} =
    mconcat
      $ fmap nodeComments
      $ aesOpen : aesBarsBefore <> aesBarsAfter <> [aesClose]

instance CommentExtraction EpAnnUnboundVar where
  nodeComments EpAnnUnboundVar {..} =
    mconcat
      $ fmap
          nodeComments
          [fst hsUnboundBackquotes, snd hsUnboundBackquotes, hsUnboundHole]

instance CommentExtraction AnnSig where
  nodeComments AnnSig {..} = mconcat $ fmap nodeComments $ asDcolon : asRest

-- | Marks an AST node as never appearing in the AST.
--
-- Some AST node types are only used in the renaming or type-checking phase.
notUsedInParsedStage :: HasCallStack => a
notUsedInParsedStage =
  error
    "This AST should never appears in an AST. It only appears in the renaming or type checked stages."

-- | A 'NodeComment' with no comments.
emptyNodeComments :: NodeComments
emptyNodeComments = NodeComments [] [] []
