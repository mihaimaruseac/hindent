{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Bind.GuardedRhs
  ( GuardedRhs(..)
  , mkGuardedRhs
  , mkCaseGuardedRhs
  , mkLambdaGuardedRhs
  , mkCaseCmdGuardedRhs
  , mkLambdaCmdGuardedRhs
  ) where

import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Guard
  ( Guard
  , mkCaseCmdGuard
  , mkCaseExprGuard
  , mkExprGuard
  , mkLambdaCmdGuard
  , mkLambdaExprGuard
  )
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data GuardedRhs
  = GuardedRhs
      { guards :: [WithComments Guard]
      , localBinds :: GHC.HsLocalBinds GHC.GhcPs
      }
  | CaseGuardedRhs
      { guards :: [WithComments Guard]
      , localBinds :: GHC.HsLocalBinds GHC.GhcPs
      }

instance CommentExtraction GuardedRhs where
  nodeComments _ = NodeComments [] [] []

instance Pretty GuardedRhs where
  pretty' GuardedRhs {..} = do
    mapM_ pretty guards
    case localBinds of
      GHC.HsValBinds epa lr ->
        indentedWithSpace 2
          $ newlinePrefixed
              [ string "where"
              , printCommentsAnd (GHC.L epa lr) (indentedWithSpace 2 . pretty)
              ]
      _ -> return ()
  pretty' CaseGuardedRhs {..} = do
    mapM_ pretty guards
    case localBinds of
      GHC.HsValBinds {} ->
        indentedBlock $ do
          newline
          string "where " |=> pretty localBinds
      _ -> return ()

mkGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = grhssLocalBinds
    }

mkCaseGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkCaseGuardedRhs GHC.GRHSs {..} =
  CaseGuardedRhs
    { guards = map (fmap mkCaseExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = grhssLocalBinds
    }

mkLambdaGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkLambdaGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkLambdaExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = grhssLocalBinds
    }

mkCaseCmdGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkCaseCmdGuardedRhs GHC.GRHSs {..} =
  CaseGuardedRhs
    { guards = map (fmap mkCaseCmdGuard . fromGenLocated) grhssGRHSs
    , localBinds = grhssLocalBinds
    }

mkLambdaCmdGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkLambdaCmdGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkLambdaCmdGuard . fromGenLocated) grhssGRHSs
    , localBinds = grhssLocalBinds
    }
