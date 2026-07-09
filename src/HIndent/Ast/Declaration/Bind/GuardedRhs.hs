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
import HIndent.Ast.WhereClause (WhereClause, mkWhereClause)
import HIndent.Ast.WithComments
  ( WithComments
  , mkWithCommentsFromGenLocated
  , prettyWith
  )
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data GuardedRhs = GuardedRhs
  { guards :: [WithComments Guard]
  , whereClause :: Maybe (WithComments WhereClause)
  }

instance Pretty GuardedRhs where
  pretty GuardedRhs {..} = do
    mapM_ pretty guards
    whenJust whereClause $ \clause ->
      indentedBlock $ newlinePrefixed [prettyWith clause pretty]

mkGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkGuardedRhs grhss@GHC.GRHSs {..} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkExprGuard . mkWithCommentsFromGenLocated)
          (toList grhssGRHSs)
    , whereClause = mkWhereClause grhss
    }

mkCaseGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkCaseGuardedRhs grhss@GHC.GRHSs {..} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseExprGuard . mkWithCommentsFromGenLocated)
          (toList grhssGRHSs)
    , whereClause = mkWhereClause grhss
    }

mkLambdaGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkLambdaGuardedRhs grhss@GHC.GRHSs {..} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkLambdaExprGuard . mkWithCommentsFromGenLocated)
          (toList grhssGRHSs)
    , whereClause = mkWhereClause grhss
    }

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

mkCaseCmdGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkCaseCmdGuardedRhs grhss@GHC.GRHSs {..} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkCaseCmdGuard . mkWithCommentsFromGenLocated)
          (toList grhssGRHSs)
    , whereClause = mkWhereClause grhss
    }

mkLambdaCmdGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkLambdaCmdGuardedRhs grhss@GHC.GRHSs {..} =
  GuardedRhs
    { guards =
        fmap
          (fmap mkLambdaCmdGuard . mkWithCommentsFromGenLocated)
          (toList grhssGRHSs)
    , whereClause = mkWhereClause grhss
    }
