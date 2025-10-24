{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Monad (when)
import Data.Maybe (isJust)
import HIndent.Ast.Guard
  ( Guard
  , mkCaseCmdGuard
  , mkCaseExprGuard
  , mkExprGuard
  , mkLambdaCmdGuard
  , mkLambdaExprGuard
  , mkMultiWayIfExprGuard
  )
import HIndent.Ast.LocalBinds (ValueBindGroup, mkValueBindGroup)
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data GuardedRhs = GuardedRhs
  { guards :: [WithComments Guard]
  , localBinds :: WithComments (Maybe ValueBindGroup)
  }

instance CommentExtraction GuardedRhs where
  nodeComments _ = NodeComments [] [] []

instance Pretty GuardedRhs where
  pretty' GuardedRhs {..} = do
    mapM_ pretty guards
    let families = getNode localBinds
        NodeComments {..} = getComments localBinds
        hasComments =
          not
            (null commentsBefore
               && null commentsOnSameLine
               && null commentsAfter)
        shouldPrintWhere = isJust families
    when (shouldPrintWhere || hasComments) $ do
      indentSpaces <- getIndentSpaces
      indentedWithSpace indentSpaces
        $ newlinePrefixed
            [ string "where"
            , prettyWith localBinds $ \case
                Just group -> indentedWithSpace indentSpaces $ pretty group
                Nothing -> pure ()
            ]

mkGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkValueBindGroup grhssLocalBinds
    }

mkCaseGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkCaseGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkCaseExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkValueBindGroup grhssLocalBinds
    }

mkLambdaGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkLambdaGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkLambdaExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkValueBindGroup grhssLocalBinds
    }

mkMultiWayIfGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkMultiWayIfGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkMultiWayIfExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkValueBindGroup grhssLocalBinds
    }

mkCaseCmdGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkCaseCmdGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkCaseCmdGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkValueBindGroup grhssLocalBinds
    }

mkLambdaCmdGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkLambdaCmdGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkLambdaCmdGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkValueBindGroup grhssLocalBinds
    }
