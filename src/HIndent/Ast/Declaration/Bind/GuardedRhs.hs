{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Bind.GuardedRhs
  ( GuardedRhs
  , mkExprGuardedRhs
  , mkCmdGuardedRhs
  , mkPatternGuardedRhs
  , mkMultiWayIfGuardedRhs
  ) where

import Data.Foldable (toList)
import HIndent.Applicative (whenJust)
import HIndent.Ast.Guard
  ( Guard
  , mkCaseCmdGuard
  , mkCaseExprGuard
  , mkExprGuard
  , mkLambdaCmdGuard
  , mkLambdaExprGuard
  , mkMultiWayIfExprGuard
  )
import HIndent.Ast.WhereClause
  ( WhereClause
  , mkMatchWhereClause
  , mkPatternWhereClause
  , mkWhereClause
  )
import HIndent.Ast.WithComments (WithComments, mkWithCommentsFromGenLocated)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data GuardedRhs = GuardedRhs
  { guards :: [WithComments Guard]
  , whereClause :: Maybe (WithComments WhereClause)
  }

instance Pretty GuardedRhs where
  pretty GuardedRhs {..} = do
    mapM_ pretty guards
    whenJust whereClause $ \whereClause' ->
      indentedBlock $ newlinePrefixed [pretty whereClause']

mkExprGuardedRhs :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkExprGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LamAlt GHC.LamSingle
                                 , GHC.m_grhss = grhss
                                 } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkLambdaExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkExprGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LamAlt _
                                 , GHC.m_grhss = grhss
                                 } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkExprGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.FunRhs {}
                                 , GHC.m_grhss = grhss
                                 } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExprGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LamAlt GHC.LamSingle
                                 , GHC.m_grhss = grhss
                                 } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkLambdaExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkExprGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LamAlt GHC.LamCase
                                 , GHC.m_grhss = grhss
                                 } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkExprGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LamAlt GHC.LamCases
                                 , GHC.m_grhss = grhss
                                 } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkExprGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.FunRhs {}
                                 , GHC.m_grhss = grhss
                                 } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkExprGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LambdaExpr
                                 , GHC.m_grhss = grhss
                                 } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkLambdaExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkExprGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LamCaseAlt {}
                                 , GHC.m_grhss = grhss
                                 } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkExprGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.FunRhs {}
                                 , GHC.m_grhss = grhss
                                 } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
#else
mkExprGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LambdaExpr
                                 , GHC.m_grhss = grhss
                                 } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkLambdaExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkExprGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.FunRhs {}
                                 , GHC.m_grhss = grhss
                                 } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
#endif
mkExprGuardedRhs _ = error "`ghc-lib-parser` never generates this AST node."

mkCmdGuardedRhs :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCmdGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LamAlt GHC.LamSingle
                                , GHC.m_grhss = grhss
                                } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkLambdaCmdGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LamAlt _, GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseCmdGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseCmdGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCmdGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LamAlt GHC.LamSingle
                                , GHC.m_grhss = grhss
                                } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkLambdaCmdGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkCmdGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LamAlt GHC.LamCase
                                , GHC.m_grhss = grhss
                                } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseCmdGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkCmdGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LamAlt GHC.LamCases
                                , GHC.m_grhss = grhss
                                } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseCmdGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseCmdGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkCmdGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LambdaExpr
                                , GHC.m_grhss = grhss
                                } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkLambdaCmdGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkCmdGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LamCaseAlt {}
                                , GHC.m_grhss = grhss
                                } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseCmdGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseCmdGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
#else
mkCmdGuardedRhs match@GHC.Match { GHC.m_ctxt = GHC.LambdaExpr
                                , GHC.m_grhss = grhss
                                } =
  GuardedRhs
    { guards =
        fmap
          (fmap mkLambdaCmdGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseCmdGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkMatchWhereClause match
    }
#endif
mkCmdGuardedRhs _ = error "`ghc-lib-parser` never generates this AST node."

mkPatternGuardedRhs :: GHC.HsBind GHC.GhcPs -> GuardedRhs
mkPatternGuardedRhs bind@GHC.PatBind {GHC.pat_rhs = grhss} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkExprGuard . mkWithCommentsFromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = mkPatternWhereClause bind
    }
mkPatternGuardedRhs _ = error "This AST node should not appear."

mkMultiWayIfGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkMultiWayIfGuardedRhs grhss@GHC.GRHSs {..} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkMultiWayIfExprGuard . mkWithCommentsFromGenLocated)
          (toList grhssGRHSs)
    , whereClause = mkWhereClause grhss
    }
