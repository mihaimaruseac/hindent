{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module HIndent.Ast.Guard
  ( Guard
  , mkExprGuard
  , mkCaseExprGuard
  , mkLambdaExprGuard
  , mkMultiWayIfExprGuard
  , mkCaseCmdGuard
  , mkLambdaCmdGuard
  ) where

import Control.Monad (unless)
import {-# SOURCE #-} HIndent.Ast.Expression
  ( GuardExpression
  , mkExpression
  , mkGuardExpression
  )
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments (WithComments, fromGenLocated)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data GuardContext
  = PlainGuard
  | CaseGuard
  | LambdaGuard
  | MultiWayIfGuard
  deriving (Eq)

data Guard
  = ExprGuard
      { guardContext :: GuardContext
      , conditions :: [GHC.GuardLStmt GHC.GhcPs]
      , expr :: WithComments GuardExpression
      }
  | CmdGuard
      { guardContext :: GuardContext
      , conditions :: [GHC.GuardLStmt GHC.GhcPs]
      , cmd :: GHC.LHsCmd GHC.GhcPs
      }

instance CommentExtraction Guard where
  nodeComments _ = NodeComments [] [] []

instance Pretty Guard where
  pretty' ExprGuard {..}
    | null conditions = do
      space
      string (contextSeparator guardContext)
      pretty expr
    | otherwise = do
      unless (guardContext == MultiWayIfGuard) newline
      (if guardContext == MultiWayIfGuard
         then id
         else indentedBlock) $ do
        string "| " |=> vCommaSep (fmap pretty conditions)
        space
        string (contextSeparator guardContext)
        pretty expr
  pretty' CmdGuard {..}
    | null conditions = do
      space
      string (contextSeparator guardContext)
      printCommentsAnd cmd $ \case
        GHC.HsCmdDo _ stmts ->
          let hor = space >> printCommentsAnd stmts (lined . fmap pretty)
              ver = do
                newline
                indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
           in hor <-|> ver
        x ->
          let hor = space >> pretty x
              ver = newline >> indentedBlock (pretty x)
           in hor <-|> ver
    | otherwise = do
      unless (guardContext == MultiWayIfGuard) newline
      (if guardContext == MultiWayIfGuard
         then id
         else indentedBlock) $ do
        string "| " |=> vCommaSep (fmap pretty conditions)
        space
        string (contextSeparator guardContext)
        printCommentsAnd cmd $ \case
          GHC.HsCmdDo _ stmts ->
            let hor = space >> printCommentsAnd stmts (lined . fmap pretty)
                ver = do
                  newline
                  indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
             in hor <-|> ver
          x ->
            let hor = space >> pretty x
                ver = newline >> indentedBlock (pretty x)
             in hor <-|> ver

contextSeparator :: GuardContext -> String
contextSeparator PlainGuard = "="
contextSeparator CaseGuard = "->"
contextSeparator LambdaGuard = "->"
contextSeparator MultiWayIfGuard = "->"

mkExprGuard :: GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Guard
mkExprGuard (GHC.GRHS _ conditions resultExpr) =
  ExprGuard
    { guardContext = PlainGuard
    , expr = mkGuardExpression . mkExpression <$> fromGenLocated resultExpr
    , ..
    }

mkCaseExprGuard :: GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Guard
mkCaseExprGuard (GHC.GRHS _ conditions resultExpr) =
  ExprGuard
    { guardContext = CaseGuard
    , expr = mkGuardExpression . mkExpression <$> fromGenLocated resultExpr
    , ..
    }

mkLambdaExprGuard :: GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Guard
mkLambdaExprGuard (GHC.GRHS _ conditions resultExpr) =
  ExprGuard
    { guardContext = LambdaGuard
    , expr = mkGuardExpression . mkExpression <$> fromGenLocated resultExpr
    , ..
    }

mkMultiWayIfExprGuard :: GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Guard
mkMultiWayIfExprGuard (GHC.GRHS _ conditions resultExpr) =
  ExprGuard
    { guardContext = MultiWayIfGuard
    , expr = mkGuardExpression . mkExpression <$> fromGenLocated resultExpr
    , ..
    }

mkCaseCmdGuard :: GHC.GRHS GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Guard
mkCaseCmdGuard (GHC.GRHS _ conditions cmd) =
  CmdGuard {guardContext = CaseGuard, ..}

mkLambdaCmdGuard :: GHC.GRHS GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Guard
mkLambdaCmdGuard (GHC.GRHS _ conditions cmd) =
  CmdGuard {guardContext = LambdaGuard, ..}
