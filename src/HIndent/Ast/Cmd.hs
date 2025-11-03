{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Cmd
  ( Cmd
  , CmdDoBlock
  , mkCmd
  , mkCmdDoBlock
  , mkCmdFromHsCmdTop
  ) where

import Data.Maybe (fromMaybe)
import qualified GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Ast.Expression (Expression, mkExpression)
import HIndent.Ast.LocalBinds (LocalBinds, mkLocalBinds)
import {-# SOURCE #-} HIndent.Ast.MatchGroup (MatchGroup, mkCmdMatchGroup)
import HIndent.Ast.Statement (CmdStatement, mkCmdStatement)
import HIndent.Ast.WithComments
  ( WithComments
  , flattenComments
  , fromGenLocated
  , prettyWith
  )
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments (CommentExtraction(..), emptyNodeComments)

data ArrowKind
  = Higher
  | First

data ArrowDirection
  = FunctionThenArgument
  | ArgumentThenFunction

data Cmd
  = ArrowApp
      { arrowKind :: ArrowKind
      , arrowDirection :: ArrowDirection
      , function :: WithComments Expression
      , argument :: WithComments Expression
      }
  | ArrowForm
      { function :: WithComments Expression
      , arguments :: [WithComments Cmd]
      }
  | CmdApp
      { cmd :: WithComments Cmd
      , argument :: WithComments Expression
      }
  | Lambda
      { matches :: MatchGroup
      }
  | LambdaCase
      { usesCases :: Bool
      , matches :: MatchGroup
      }
  | Case
      { scrutinee :: WithComments Expression
      , matches :: MatchGroup
      }
  | If
      { predicate :: WithComments Expression
      , thenBranch :: WithComments Cmd
      , elseBranch :: WithComments Cmd
      }
  | Let
      { localBinds :: WithComments LocalBinds
      , inCommand :: WithComments Cmd
      }
  | DoBlock
      { statements :: WithComments [WithComments CmdStatement]
      }
  | Parenthesized (WithComments Cmd)

instance CommentExtraction Cmd where
  nodeComments _ = emptyNodeComments

instance Pretty Cmd where
  pretty' ArrowApp {..} =
    case arrowDirection of
      FunctionThenArgument ->
        spaced [pretty function, string operator, pretty argument]
      ArgumentThenFunction ->
        spaced [pretty argument, string operator, pretty function]
    where
      operator =
        case (arrowKind, arrowDirection) of
          (Higher, FunctionThenArgument) -> "-<<"
          (Higher, ArgumentThenFunction) -> ">>-"
          (First, FunctionThenArgument) -> "-<"
          (First, ArgumentThenFunction) -> ">-"
  pretty' ArrowForm {..} =
    bananaBrackets $ spaced $ pretty function : fmap pretty arguments
  pretty' CmdApp {..} = spaced [pretty cmd, pretty argument]
  pretty' Lambda {..} = pretty matches
  pretty' LambdaCase {..} = do
    string
      $ if usesCases
          then "\\cases"
          else "\\case"
    newline
    indentedBlock $ pretty matches
  pretty' Case {..} = do
    spaced [string "case", pretty scrutinee, string "of"]
    newline
    indentedBlock $ pretty matches
  pretty' If {..} = do
    string "if "
    pretty predicate
    newline
    indentedBlock
      $ lined
          [ string "then " >> pretty thenBranch
          , string "else " >> pretty elseBranch
          ]
  pretty' Let {..} =
    lined
      [string "let " |=> pretty localBinds, string " in " |=> pretty inCommand]
  pretty' DoBlock {..} = do
    string "do"
    newline
    indentedBlock $ prettyWith statements $ lined . fmap pretty
  pretty' (Parenthesized cmd) = parens $ pretty cmd

mkCmd :: GHC.HsCmd GHC.GhcPs -> Cmd
mkCmd (GHC.HsCmdArrApp _ f arg appKind isFwd) =
  ArrowApp
    { arrowKind =
        case appKind of
          GHC.HsHigherOrderApp -> Higher
          GHC.HsFirstOrderApp -> First
    , arrowDirection =
        if isFwd
          then FunctionThenArgument
          else ArgumentThenFunction
    , function = mkExpression <$> fromGenLocated f
    , argument = mkExpression <$> fromGenLocated arg
    }
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCmd (GHC.HsCmdArrForm _ f _ args) =
  ArrowForm
    { function = mkExpression <$> fromGenLocated f
    , arguments =
        fmap (flattenComments . fmap mkCmdFromHsCmdTop . fromGenLocated) args
    }
#else
mkCmd (GHC.HsCmdArrForm _ f _ _ args) =
  ArrowForm
    { function = mkExpression <$> fromGenLocated f
    , arguments =
        fmap (flattenComments . fmap mkCmdFromHsCmdTop . fromGenLocated) args
    }
#endif
mkCmd (GHC.HsCmdApp _ cmd arg) =
  CmdApp
    { cmd = mkCmd <$> fromGenLocated cmd
    , argument = mkExpression <$> fromGenLocated arg
    }
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCmd (GHC.HsCmdLam _ GHC.LamSingle matches) =
  Lambda {matches = mkCmdMatchGroup matches}
mkCmd (GHC.HsCmdLam _ GHC.LamCase matches) =
  LambdaCase {usesCases = False, matches = mkCmdMatchGroup matches}
mkCmd (GHC.HsCmdLam _ GHC.LamCases matches) =
  LambdaCase {usesCases = True, matches = mkCmdMatchGroup matches}
#else
mkCmd (GHC.HsCmdLam _ matches) = Lambda {matches = mkCmdMatchGroup matches}
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCmd (GHC.HsCmdPar _ _ cmd _) = Parenthesized $ mkCmd <$> fromGenLocated cmd
#else
mkCmd (GHC.HsCmdPar _ cmd) = Parenthesized $ mkCmd <$> fromGenLocated cmd
#endif
mkCmd (GHC.HsCmdCase _ expr matches) =
  Case
    { scrutinee = mkExpression <$> fromGenLocated expr
    , matches = mkCmdMatchGroup matches
    }
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCmd (GHC.HsCmdLamCase _ _ matches) =
  LambdaCase {usesCases = False, matches = mkCmdMatchGroup matches}
#elif !MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCmd (GHC.HsCmdLamCase _ matches) =
  LambdaCase {usesCases = False, matches = mkCmdMatchGroup matches}
#endif
mkCmd (GHC.HsCmdIf _ _ predicate thenCmd elseCmd) =
  If
    { predicate = mkExpression <$> fromGenLocated predicate
    , thenBranch = mkCmd <$> fromGenLocated thenCmd
    , elseBranch = mkCmd <$> fromGenLocated elseCmd
    }
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCmd (GHC.HsCmdLet _ _ binds _ cmd) =
  Let
    { localBinds =
        fromMaybe
          (error "`ghc-lib-parser` never generates an empty `HsCmdLet` node.")
          $ mkLocalBinds binds
    , inCommand = mkCmd <$> fromGenLocated cmd
    }
#else
mkCmd (GHC.HsCmdLet _ binds cmd) =
  Let
    { localBinds =
        fromMaybe
          (error "`ghc-lib-parser` never generates an empty `HsCmdLet` node.")
          $ mkLocalBinds binds
    , inCommand = mkCmd <$> fromGenLocated cmd
    }
#endif
mkCmd (GHC.HsCmdDo _ stmts) =
  DoBlock
    { statements =
        fmap (fmap mkCmdStatement . fromGenLocated) <$> fromGenLocated stmts
    }
mkCmd _ = error "`ghc-lib-parser` never generates this AST node."

mkCmdFromHsCmdTop :: GHC.HsCmdTop GHC.GhcPs -> WithComments Cmd
mkCmdFromHsCmdTop (GHC.HsCmdTop _ cmd) = mkCmd <$> fromGenLocated cmd

newtype CmdDoBlock =
  CmdDoBlock Cmd

instance CommentExtraction CmdDoBlock where
  nodeComments _ = emptyNodeComments

instance Pretty CmdDoBlock where
  pretty' (CmdDoBlock DoBlock {statements = stmts}) =
    prettyWith stmts $ lined . fmap pretty
  pretty' (CmdDoBlock _) =
    error "mkCmdDoBlock must be used before pretty-printing CmdDoBlock"

mkCmdDoBlock :: Cmd -> Maybe CmdDoBlock
mkCmdDoBlock cmd
  | DoBlock {} <- cmd = Just (CmdDoBlock cmd)
  | otherwise = Nothing
