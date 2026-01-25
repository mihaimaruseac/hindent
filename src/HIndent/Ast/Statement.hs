{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Statement
  ( ExprStatement
  , CmdStatement
  , mkExprStatement
  , mkCmdStatement
  ) where

import qualified GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Ast.Cmd (Cmd, mkCmd)
import {-# SOURCE #-} HIndent.Ast.Expression (Expression, mkExpression)
import HIndent.Ast.LocalBinds (LocalBinds, mkLocalBinds)
import HIndent.Ast.Pattern (Pattern, mkPattern)
import HIndent.Ast.WithComments (WithComments, fromGenLocated, prettyWith)
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments (CommentExtraction(..), emptyNodeComments)

type ExprStatement = Statement Expression

type CmdStatement = Statement Cmd

data Statement a
  = Expression (WithComments a)
  | Binding
      { lhsPattern :: WithComments Pattern
      , rhs :: WithComments a
      }
  | LetBinding (WithComments LocalBinds)
  | Parallel [[WithComments (Statement a)]]
  | Transform
      { steps :: [WithComments (Statement a)]
      , using :: WithComments a
      }
  | Recursive
      { block :: WithComments [WithComments (Statement a)]
      }

instance CommentExtraction (Statement a) where
  nodeComments _ = emptyNodeComments

instance Pretty a => Pretty (Statement a) where
  pretty' (Expression expr) = pretty expr
  pretty' Binding {..} = do
    pretty lhsPattern
    string " <-"
    hor <-|> ver
    where
      hor = space >> pretty rhs
      ver = newline >> indentedBlock (pretty rhs)
  pretty' (LetBinding binds) = string "let " |=> pretty binds
  pretty' (Parallel blocks)
    | any ((> 1) . length) blocks =
      vBarSep $ fmap (vCommaSep . fmap pretty) blocks
    | otherwise = hvBarSep $ fmap (hvCommaSep . fmap pretty) blocks
  pretty' Transform {..} =
    vCommaSep $ fmap pretty steps ++ [string "then " >> pretty using]
  pretty' Recursive {..} =
    string "rec " |=> prettyWith block (lined . fmap pretty)

mkExprStatement ::
     GHC.StmtLR GHC.GhcPs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> ExprStatement
mkExprStatement (GHC.LastStmt _ expr _ _) =
  Expression $ mkExpression <$> fromGenLocated expr
mkExprStatement (GHC.BindStmt _ pat expr) =
  Binding
    { lhsPattern = mkPattern <$> fromGenLocated pat
    , rhs = mkExpression <$> fromGenLocated expr
    }
mkExprStatement (GHC.BodyStmt _ body _ _) =
  Expression $ mkExpression <$> fromGenLocated body
mkExprStatement (GHC.LetStmt _ binds) =
  case mkLocalBinds binds of
    Just localBinds -> LetBinding localBinds
    Nothing ->
      error
        "`ghc-lib-parser` never generates a `LetStmt` without bindings in the parsed AST."
mkExprStatement (GHC.ParStmt _ blocks _ _) =
  Parallel
    $ fmap
        (\(GHC.ParStmtBlock _ stmts _ _) ->
           fmap (fmap mkExprStatement . fromGenLocated) stmts)
        blocks
mkExprStatement GHC.TransStmt {..} =
  Transform
    { steps = fmap (fmap mkExprStatement . fromGenLocated) trS_stmts
    , using = mkExpression <$> fromGenLocated trS_using
    }
mkExprStatement GHC.RecStmt {..} =
  Recursive
    { block =
        fmap
          (fmap (fmap mkExprStatement . fromGenLocated))
          (fromGenLocated recS_stmts)
    }
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkExprStatement GHC.ApplicativeStmt {} =
  error "`ghc-lib-parser` never generates this AST node."
#endif
mkCmdStatement ::
     GHC.StmtLR GHC.GhcPs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> CmdStatement
mkCmdStatement (GHC.LastStmt _ cmd _ _) =
  Expression $ mkCmd <$> fromGenLocated cmd
mkCmdStatement (GHC.BindStmt _ pat cmd) =
  Binding
    { lhsPattern = mkPattern <$> fromGenLocated pat
    , rhs = mkCmd <$> fromGenLocated cmd
    }
mkCmdStatement (GHC.BodyStmt _ cmd _ _) =
  Expression $ mkCmd <$> fromGenLocated cmd
mkCmdStatement (GHC.LetStmt _ binds) =
  case mkLocalBinds binds of
    Just localBinds -> LetBinding localBinds
    Nothing ->
      error
        "`ghc-lib-parser` never generates a `LetStmt` without bindings in the parsed AST."
mkCmdStatement GHC.ParStmt {} =
  error "`ghc-lib-parser` never generates this AST node."
mkCmdStatement GHC.TransStmt {} =
  error "`ghc-lib-parser` never generates this AST node."
mkCmdStatement GHC.RecStmt {} =
  error "`ghc-lib-parser` never generates this AST node."
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCmdStatement GHC.ApplicativeStmt {} =
  error "`ghc-lib-parser` never generates this AST node."
#endif
