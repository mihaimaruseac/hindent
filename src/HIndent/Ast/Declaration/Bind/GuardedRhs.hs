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

import Control.Monad (when)
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

data GuardedRhs = GuardedRhs
  { guards :: [WithComments Guard]
  , localBinds :: WithComments LocalBinds
  }

instance CommentExtraction GuardedRhs where
  nodeComments _ = NodeComments [] [] []

instance Pretty GuardedRhs where
  pretty' GuardedRhs {..} = do
    mapM_ pretty guards
    let node = getNode localBinds
        NodeComments {..} = getComments localBinds
        hasComments =
          not
            (null commentsBefore
               && null commentsOnSameLine
               && null commentsAfter)
        shouldPrintWhere =
          case node of
            ValueLocalBinds {sigBindFamilies = Just _} -> True
            ValueLocalBinds {sigBindFamilies = Nothing} -> False
            ImplicitParameterLocalBinds {} -> True
    when (shouldPrintWhere || hasComments) $ do
      indentSpaces <- getIndentSpaces
      indentedWithSpace indentSpaces
        $ newlinePrefixed
            [ string "where"
            , prettyWith localBinds $ \local ->
                indentedWithSpace indentSpaces $ pretty local
            ]

mkGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkLocalBindsWithComments grhssLocalBinds
    }

mkCaseGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkCaseGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkCaseExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkLocalBindsWithComments grhssLocalBinds
    }

mkLambdaGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkLambdaGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkLambdaExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkLocalBindsWithComments grhssLocalBinds
    }

mkMultiWayIfGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkMultiWayIfGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkMultiWayIfExprGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkLocalBindsWithComments grhssLocalBinds
    }

mkCaseCmdGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkCaseCmdGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkCaseCmdGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkLocalBindsWithComments grhssLocalBinds
    }

mkLambdaCmdGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkLambdaCmdGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards = map (fmap mkLambdaCmdGuard . fromGenLocated) grhssGRHSs
    , localBinds = mkLocalBindsWithComments grhssLocalBinds
    }
