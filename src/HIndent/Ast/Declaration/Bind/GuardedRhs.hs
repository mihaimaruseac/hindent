{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
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

import HIndent.Ast.Guard
  ( Guard
  , mkCaseCmdGuard
  , mkCaseExprGuard
  , mkExprGuard
  , mkLambdaCmdGuard
  , mkLambdaExprGuard
  , mkMultiWayIfExprGuard
  )
import HIndent.Ast.LocalBinds
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Context
  = Plain
  | Case
  | Lambda
  | MultiWayIf
  deriving (Eq)

data GuardedRhs = GuardedRhs
  { context :: Context
  , guards :: [WithComments Guard]
  , localBinds :: WithComments LocalBinds
  }

instance CommentExtraction GuardedRhs where
  nodeComments _ = NodeComments [] [] []

instance Pretty GuardedRhs where
  pretty' GuardedRhs {..} = do
    mapM_ pretty guards
    let node = getNode localBinds
    case (valueLocalBinds node, context) of
      (Just _, Case)
        | hasLocalBinds node ->
          indentedBlock $ do
            newline
            prettyWith localBinds $ \case
              ValueLocalBinds vb -> string "where " |=> pretty vb
              _ -> return ()
      (Just _, _)
        | hasLocalBinds node ->
          indentedWithSpace 2
            $ newlinePrefixed
                [ string "where"
                , prettyWith localBinds $ \case
                    ValueLocalBinds vb -> indentedWithSpace 2 $ pretty vb
                    _ -> return ()
                ]
      _ -> return ()

mkGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { context = Plain
    , guards = map (fmap mkExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkLocalBindsWithComments grhssLocalBinds
    }

mkCaseGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkCaseGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { context = Case
    , guards = map (fmap mkCaseExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkLocalBindsWithComments grhssLocalBinds
    }

mkLambdaGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkLambdaGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { context = Lambda
    , guards = map (fmap mkLambdaExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkLocalBindsWithComments grhssLocalBinds
    }

mkMultiWayIfGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkMultiWayIfGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { context = MultiWayIf
    , guards = map (fmap mkMultiWayIfExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkLocalBindsWithComments grhssLocalBinds
    }

mkCaseCmdGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkCaseCmdGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { context = Case
    , guards = map (fmap mkCaseCmdGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkLocalBindsWithComments grhssLocalBinds
    }

mkLambdaCmdGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkLambdaCmdGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { context = Lambda
    , guards = map (fmap mkLambdaCmdGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkLocalBindsWithComments grhssLocalBinds
    }
