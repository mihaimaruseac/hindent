{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Bind.GuardedRhs
  ( GuardedRhs
  , mkGuardedRhs
  , mkCaseGuardedRhs
  , mkLambdaGuardedRhs
  , mkMultiWayIfGuardedRhs
  , mkCaseCmdGuardedRhs
  , mkLambdaCmdGuardedRhs
  ) where

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
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.WithComments
  ( WithComments
  , fromEpAnn
  , fromGenLocated
  , prettyWith
  )
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import qualified HIndent.Pretty.SigBindFamily as SBF
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as Bag
#endif
data GuardedRhs = GuardedRhs
  { guards :: [WithComments Guard]
  , localBinds :: Maybe (WithComments [WithComments SBF.SigBindFamily])
  }

instance CommentExtraction GuardedRhs where
  nodeComments _ = NodeComments [] [] []

instance Pretty GuardedRhs where
  pretty' GuardedRhs {..} = do
    mapM_ pretty guards
    whenJust localBinds $ \fams ->
      indentedBlock
        $ newlinePrefixed
            [ string "where"
            , prettyWith fams $ indentedBlock . lined . fmap pretty
            ]

mkGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = valueFamiliesWithComments grhssLocalBinds
    }

mkCaseGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkCaseGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkCaseExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = valueFamiliesWithComments grhssLocalBinds
    }

mkLambdaGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkLambdaGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkLambdaExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = valueFamiliesWithComments grhssLocalBinds
    }

mkMultiWayIfGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkMultiWayIfGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkMultiWayIfExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = valueFamiliesWithComments grhssLocalBinds
    }

mkCaseCmdGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkCaseCmdGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkCaseCmdGuard . fromGenLocated) grhssGRHSs
    , localBinds = valueFamiliesWithComments grhssLocalBinds
    }

mkLambdaCmdGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkLambdaCmdGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkLambdaCmdGuard . fromGenLocated) grhssGRHSs
    , localBinds = valueFamiliesWithComments grhssLocalBinds
    }

valueFamiliesWithComments ::
     GHC.HsLocalBinds GHC.GhcPs
  -> Maybe (WithComments [WithComments SBF.SigBindFamily])
valueFamiliesWithComments (GHC.HsValBinds ann valBinds) =
  Just $ fromEpAnn ann $ fromGenLocated <$> sigBindFamilies valBinds
valueFamiliesWithComments GHC.HsIPBinds {} = Nothing
valueFamiliesWithComments GHC.EmptyLocalBinds {} = Nothing
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
sigBindFamilies ::
     GHC.HsValBindsLR GHC.GhcPs GHC.GhcPs -> [GHC.LocatedA SBF.SigBindFamily]
sigBindFamilies (GHC.ValBinds _ bindBag sigs) =
  SBF.mkSortedLSigBindFamilyList sigs (Bag.bagToList bindBag) [] [] []
#else
sigBindFamilies ::
     GHC.HsValBindsLR GHC.GhcPs GHC.GhcPs -> [GHC.LocatedA SBF.SigBindFamily]
sigBindFamilies (GHC.ValBinds _ binds sigs) =
  SBF.mkSortedLSigBindFamilyList sigs binds [] [] []
#endif
sigBindFamilies GHC.XValBindsLR {} =
  error "`ghc-lib-parser` never generates this AST node."
