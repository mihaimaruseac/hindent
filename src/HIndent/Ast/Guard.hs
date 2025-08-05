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
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Module.Name (mkModuleName)
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types (DoOrMdo(..), QualifiedDo(..))
import HIndent.Printer ()

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
      , expr :: GHC.LHsExpr GHC.GhcPs
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
      case GHC.unLoc expr of
        GHC.HsDo _ (GHC.DoExpr m) stmts ->
          printCommentsAnd expr $ \_ -> do
            space
            pretty (QualifiedDo (fmap mkModuleName m) Do)
            newline
            indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
        GHC.HsDo _ (GHC.MDoExpr m) stmts ->
          printCommentsAnd expr $ \_ -> do
            space
            pretty (QualifiedDo (fmap mkModuleName m) Mdo)
            newline
            indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
        GHC.OpApp _ (GHC.L _ (GHC.HsDo _ GHC.DoExpr {} _)) _ _ ->
          space >> pretty expr
        GHC.OpApp _ (GHC.L _ (GHC.HsDo _ GHC.MDoExpr {} _)) _ _ ->
          space >> pretty expr
        _ ->
          let hor = space >> pretty expr
              ver = newline >> indentedBlock (pretty expr)
           in hor <-|> ver
    | otherwise = do
      unless (guardContext == MultiWayIfGuard) newline
      (if guardContext == MultiWayIfGuard
         then id
         else indentedBlock) $ do
        string "| " |=> vCommaSep (fmap pretty conditions)
        space
        string (contextSeparator guardContext)
        printCommentsAnd expr $ \case
          GHC.HsDo _ (GHC.DoExpr m) stmts -> do
            space
            pretty (QualifiedDo (fmap mkModuleName m) Do)
            newline
            indentedBlock (printCommentsAnd stmts (lined . fmap pretty))
          GHC.HsDo _ (GHC.MDoExpr m) stmts -> do
            space
            pretty (QualifiedDo (fmap mkModuleName m) Mdo)
            newline
            indentedBlock (printCommentsAnd stmts (lined . fmap pretty))
          x ->
            let hor = space >> pretty x
                ver = newline >> indentedBlock (pretty x)
             in hor <-|> ver
  pretty' CmdGuard {..}
    | null conditions = do
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
mkExprGuard (GHC.GRHS _ conditions expr) =
  ExprGuard {guardContext = PlainGuard, ..}

mkCaseExprGuard :: GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Guard
mkCaseExprGuard (GHC.GRHS _ conditions expr) =
  ExprGuard {guardContext = CaseGuard, ..}

mkLambdaExprGuard :: GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Guard
mkLambdaExprGuard (GHC.GRHS _ conditions expr) =
  ExprGuard {guardContext = LambdaGuard, ..}

mkMultiWayIfExprGuard :: GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Guard
mkMultiWayIfExprGuard (GHC.GRHS _ conditions expr) =
  ExprGuard {guardContext = MultiWayIfGuard, ..}

mkCaseCmdGuard :: GHC.GRHS GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Guard
mkCaseCmdGuard (GHC.GRHS _ conditions cmd) =
  CmdGuard {guardContext = CaseGuard, ..}

mkLambdaCmdGuard :: GHC.GRHS GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Guard
mkLambdaCmdGuard (GHC.GRHS _ conditions cmd) =
  CmdGuard {guardContext = LambdaGuard, ..}
