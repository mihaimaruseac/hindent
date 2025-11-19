{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module HIndent.Ast.Expression
  ( Expression
  , GuardExpression
  , mkExpression
  , mkGuardExpression
  ) where

import Control.Monad
import Control.Monad.RWS (gets)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Monoid (First(..))
import qualified GHC.Hs as GHC
import qualified GHC.Types.Basic as GHC
import qualified GHC.Types.Fixity as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Cmd (Cmd, CmdDoBlock, mkCmdDoBlock, mkCmdFromHsCmdTop)
import HIndent.Ast.Expression.Bracket (Bracket, mkBracket)
import HIndent.Ast.Expression.FieldSelector (FieldSelector, mkFieldSelector)
import qualified HIndent.Ast.Expression.ListComprehension as LC
import HIndent.Ast.Expression.OverloadedLabel
  ( OverloadedLabel
  , mkOverloadedLabel
  )
import HIndent.Ast.Expression.Pragmatic (ExpressionPragma, mkExpressionPragma)
import HIndent.Ast.Expression.RangeExpression
  ( RangeExpression
  , mkRangeExpression
  )
import HIndent.Ast.Expression.RecordConstructionField
  ( RecordConstructionFields
  , mkRecordConstructionFields
  )
import HIndent.Ast.Expression.RecordUpdateField
  ( RecordUpdateFields
  , mkRecordUpdateFields
  )
import HIndent.Ast.Guard (Guard, mkMultiWayIfExprGuard)
import HIndent.Ast.LocalBinds (LocalBinds, mkLocalBinds)
import HIndent.Ast.MatchGroup (MatchGroup, hasMatches, mkExprMatchGroup)
import HIndent.Ast.Module.Name (mkModuleName)
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.Pattern
import HIndent.Ast.Statement (ExprStatement, mkExprStatement)
import HIndent.Ast.Type
import HIndent.Ast.Type.ImplicitParameterName
  ( ImplicitParameterName
  , mkImplicitParameterName
  )
import HIndent.Ast.WithComments
import HIndent.CabalFile ()
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types (DoOrMdo(..), QualifiedDo(..))
import HIndent.Printer
import qualified Language.Haskell.Syntax.Basic as HS
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
import Data.Maybe
import HIndent.Ast.Expression.Splice (Splice, mkSplice, mkTypedSplice)
import HIndent.Fixity (fixities)
import qualified Language.Haskell.GhclibParserEx.GHC.Hs.Expr as GHC
#else
import qualified GHC.Types.Name.Reader as NameReader
import HIndent.Ast.Expression.Splice (Splice, mkSplice)
#endif
data Expression
  = Variable (WithComments PrefixName)
  | UnboundVariable PrefixName
  | OverloadedLabel OverloadedLabel
  | ImplicitParameter ImplicitParameterName
  | OverloadedLiteral (GHC.HsOverLit GHC.GhcPs)
  | Literal (GHC.HsLit GHC.GhcPs)
  | Lambda MatchGroup
  | LambdaCase
      { usesCases :: Bool
      , matches :: MatchGroup
      }
  | Application (NonEmpty (WithComments Expression))
  | TypeApplication
      { expression :: WithComments Expression
      , typeArg :: WithComments Type
      }
  | OperatorApplication
      { firstOperand :: WithComments Expression
      , operatorOperandPairs :: NonEmpty
          (WithComments InfixExpr, WithComments Expression)
      }
  | Negation (WithComments Expression)
  | Parenthesized (WithComments Expression)
  | LeftSection
      { left :: WithComments Expression
      , operator :: WithComments InfixExpr
      }
  | RightSection
      { operator :: WithComments InfixExpr
      , right :: WithComments Expression
      }
  | Tuple
      { arguments :: [WithComments (Maybe (WithComments Expression))]
      , isBoxed :: Bool
      }
  | Sum
      { position :: GHC.ConTag
      , arity :: HS.SumWidth
      , expression :: WithComments Expression
      }
  | CaseExpression
      { scrutinee :: WithComments Expression
      , matches :: MatchGroup
      }
  | IfExpression
      { predicate :: WithComments Expression
      , thenBranch :: WithComments Expression
      , elseBranch :: WithComments Expression
      }
  | MultiIf [WithComments Guard]
  | LetBinding
      { bindings :: WithComments LocalBinds
      , expression :: WithComments Expression
      }
  | DoBlock
      { qualifiedDo :: QualifiedDo
      , statements :: WithComments [WithComments ExprStatement]
      }
  | ListComprehension (WithComments LC.ListComprehension)
  | ListLiteral [WithComments Expression]
  | RecordConstruction
      { name :: WithComments PrefixName
      , fields :: RecordConstructionFields
      }
  | RecordUpdate RecordUpdateFields
  | FieldProjection
      { expression :: WithComments Expression
      , selector :: WithComments FieldSelector
      }
  | Projection (NonEmpty (WithComments FieldSelector))
  | TypeSignature
      { expression :: WithComments Expression
      , signature :: WithComments Type
      }
  | ArithmeticSequence RangeExpression
  | TypedQuotation (WithComments Expression)
  | UntypedQuotation Bracket
  | Splice Splice
  | ProcExpression
      { pat :: WithComments Pattern
      , cmd :: WithComments Cmd
      }
  | ProcDo
      { pat :: WithComments Pattern
      , block :: CmdDoBlock
      }
  | StaticExpression (WithComments Expression)
  | PragmaticExpression
      { pragma :: WithComments ExpressionPragma
      , expression :: WithComments Expression
      }

instance CommentExtraction Expression where
  nodeComments _ = NodeComments [] [] []

instance Pretty Expression where
  pretty' (Variable name) = pretty name
  pretty' (UnboundVariable name) = pretty name
  pretty' (OverloadedLabel label) = pretty label
  pretty' (ImplicitParameter name) = pretty name
  pretty' (OverloadedLiteral lit) = pretty lit
  pretty' (Literal lit) = pretty lit
  pretty' (Lambda matches) = pretty matches
  pretty' LambdaCase {..} = do
    string
      $ if usesCases
          then "\\cases"
          else "\\case"
    if not $ hasMatches matches
      then string " {}"
      else do
        newline
        indentedBlock $ pretty matches
  pretty' (Application (headExpr :| argList)) = horizontal <-|> vertical
    where
      horizontal = spaced $ pretty <$> headExpr : argList
      vertical = do
        col <- gets psColumn
        indentSpaces <- getIndentSpaces
        pretty headExpr
        col' <- gets psColumn
        let headLength =
              col'
                - col
                - if col == 0
                    then indentSpaces
                    else 0
            hangFirstArg = headLength + 1 <= indentSpaces
        if hangFirstArg
          then space
          else newline
        spaces' <- getIndentSpaces
        indentedWithSpace spaces' $ lined $ fmap pretty argList
  pretty' TypeApplication {..} = do
    pretty expression
    string " @"
    pretty typeArg
  pretty' OperatorApplication {..} = horizontal <-|> vertical
    where
      horizontal = do
        pretty firstOperand
        space
        spaced
          $ concatMap (\(op, r) -> [pretty op, pretty r]) operatorOperandPairs
      vertical = do
        pretty firstOperand
        prettyOpAndRhs $ NonEmpty.toList operatorOperandPairs
        where
          prettyOpAndRhs [] = pure ()
          prettyOpAndRhs [(o, r)]
            | shouldBeInline (getNode r) = space >> spaced [pretty o, pretty r]
          prettyOpAndRhs ((o, r):xs) = do
            newline
            indentedBlock $ (pretty o >> space) |=> pretty r
            prettyOpAndRhs xs
          shouldBeInline DoBlock {} = True
          shouldBeInline Lambda {} = True
          shouldBeInline LambdaCase {} = True
          shouldBeInline _ = False
  pretty' (Negation expr) = string "-" >> pretty expr
  pretty' (Parenthesized expr) = parens $ pretty expr
  pretty' LeftSection {..} = spaced [pretty left, pretty operator]
  pretty' RightSection {..} = (pretty operator >> space) |=> pretty right
  pretty' Tuple {..} = horizontal <-|> vertical
    where
      horizontal = parH $ prettyArg pretty <$> arguments
      vertical =
        parV
          $ prefixedLined ","
          $ prettyArg (\expr -> space |=> pretty expr) <$> arguments
      prettyArg f arg = prettyWith arg (maybe (pure ()) f)
      (parH, parV) =
        if isBoxed
          then (hTuple, parens)
          else (hUnboxedTuple, unboxedParens)
  pretty' Sum {..} = do
    string "(#"
    forM_ [1 .. arity] $ \idx -> do
      if idx == position
        then space >> pretty expression >> space
        else space
      when (idx < arity) $ string "|"
    string "#)"
  pretty' CaseExpression {..} = do
    string "case " |=> do
      pretty scrutinee
      string " of"
    if not $ hasMatches matches
      then string " {}"
      else do
        newline
        indentedBlock $ pretty matches
  pretty' IfExpression {..} = do
    string "if " |=> pretty predicate
    indentedBlock
      $ newlinePrefixed [branch "then " thenBranch, branch "else " elseBranch]
    where
      branch str exprWithComments =
        prettyWith exprWithComments $ \expr ->
          case expr of
            DoBlock {} -> do
              string str
              pretty expr
            _ -> string str |=> pretty expr
  pretty' (MultiIf multiIfClauses) =
    string "if " |=> lined (fmap pretty multiIfClauses)
  pretty' LetBinding {..} =
    lined
      [string "let " |=> pretty bindings, string " in " |=> pretty expression]
  pretty' DoBlock {..} = do
    pretty qualifiedDo
    newline
    indentedBlock $ prettyWith statements (lined . fmap pretty)
  pretty' (ListComprehension listComprehension) = pretty listComprehension
  pretty' (ListLiteral listItems) = horizontal <-|> vertical
    where
      horizontal = brackets $ hCommaSep $ fmap pretty listItems
      vertical = vList $ fmap pretty listItems
  pretty' RecordConstruction {..} = horizontal <-|> vertical
    where
      horizontal = spaced [pretty name, pretty fields]
      vertical = do
        pretty name
        (space >> pretty fields) <-|> (newline >> indentedBlock (pretty fields))
  pretty' (RecordUpdate fields) = pretty fields
  pretty' FieldProjection {..} = do
    pretty expression
    dot
    pretty selector
  pretty' (Projection projectionFields) =
    parens $ forM_ projectionFields prettyProjectionField
    where
      prettyProjectionField projectionField = do
        string "."
        pretty projectionField
  pretty' TypeSignature {..} =
    spaced [pretty expression, string "::", pretty signature]
  pretty' (ArithmeticSequence rangeExpression) = pretty rangeExpression
  pretty' (TypedQuotation expr) = typedBrackets $ pretty expr
  pretty' (UntypedQuotation bracket) = pretty bracket
  pretty' ProcExpression {..} = hor <-|> ver
    where
      hor = spaced [string "proc", pretty pat, string "->", pretty cmd]
      ver = do
        spaced [string "proc", pretty pat, string "->"]
        newline
        indentedBlock $ pretty cmd
  pretty' ProcDo {..} = do
    spaced [string "proc", pretty pat, string "-> do"]
    newline
    indentedBlock $ pretty block
  pretty' (StaticExpression expr) = spaced [string "static", pretty expr]
  pretty' PragmaticExpression {..} = spaced [pretty pragma, pretty expression]
  pretty' (Splice splice') = pretty splice'

mkExpression :: GHC.HsExpr GHC.GhcPs -> Expression
mkExpression (GHC.HsVar _ name) =
  Variable $ mkPrefixName <$> fromGenLocated name
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
mkExpression (GHC.HsUnboundVar _ name) = UnboundVariable $ mkPrefixName name
#else
mkExpression (GHC.HsUnboundVar _ name) =
  UnboundVariable $ mkPrefixName $ NameReader.mkRdrUnqual name
#endif
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkExpression (GHC.HsOverLabel _ label) =
  OverloadedLabel $ mkOverloadedLabel label
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkExpression (GHC.HsOverLabel _ _ label) =
  OverloadedLabel $ mkOverloadedLabel label
#else
mkExpression (GHC.HsOverLabel _ label) =
  OverloadedLabel $ mkOverloadedLabel label
#endif
mkExpression (GHC.HsIPVar _ name) =
  ImplicitParameter $ mkImplicitParameterName name
mkExpression (GHC.HsOverLit _ lit) = OverloadedLiteral lit
mkExpression (GHC.HsLit _ lit) = Literal lit
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExpression (GHC.HsEmbTy _ _) =
  error "`ghc-lib-parser` never generates this AST node."
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExpression (GHC.HsLam _ GHC.LamSingle matches) =
  Lambda $ mkExprMatchGroup matches
mkExpression (GHC.HsLam _ GHC.LamCases matches) =
  LambdaCase {usesCases = True, matches = mkExprMatchGroup matches}
mkExpression (GHC.HsLam _ GHC.LamCase matches) =
  LambdaCase {usesCases = False, matches = mkExprMatchGroup matches}
#else
mkExpression (GHC.HsLam _ matches) = Lambda $ mkExprMatchGroup matches
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkExpression (GHC.HsLamCase _ GHC.LamCases matches) =
  LambdaCase {usesCases = True, matches = mkExprMatchGroup matches}
mkExpression (GHC.HsLamCase _ GHC.LamCase matches) =
  LambdaCase {usesCases = False, matches = mkExprMatchGroup matches}
#else
mkExpression (GHC.HsLamCase _ _ matches) =
  LambdaCase {usesCases = False, matches = mkExprMatchGroup matches}
#endif
#endif
mkExpression (GHC.HsApp _ function argument) =
  case getNode f of
    Application (h :| as) -> Application $ h :| push as
    _ -> Application $ f :| [a]
  where
    f = mkExpression <$> fromGenLocated function
    a = mkExpression <$> fromGenLocated argument
    push [] = [a]
    push (x:xs) = go x xs
      where
        go final [] = [addComments (getComments f) final, a]
        go current (y:ys) = current : go y ys
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExpression (GHC.HsAppType _ fun typeArg) =
  TypeApplication
    { expression = mkExpression <$> fromGenLocated fun
    , typeArg = mkTypeFromLHsWcType typeArg
    }
#else
mkExpression (GHC.HsAppType _ fun _ typeArg) =
  TypeApplication
    { expression = mkExpression <$> fromGenLocated fun
    , typeArg = mkTypeFromLHsWcType typeArg
    }
#endif
mkExpression (GHC.OpApp _ lhs op rhs) = OperatorApplication {..}
  where
    (firstOperand, operatorOperandPairs) =
      case fixityDir operatorFixity of
        GHC.InfixL -> build leftChain
        GHC.InfixR -> build rightChain
        GHC.InfixN
          | GHC.L _ (GHC.OpApp _ _ o _) <- lhs
          , isSameAssoc o -> build leftChain
          | otherwise -> build rightChain
    operatorFixity = findFixity op
    build (x:xs) = (mkExpression <$> fromGenLocated x, f xs)
      where
        f [l, o] =
          NonEmpty.singleton
            ( InfixExpr . mkExpression <$> fromGenLocated l
            , mkExpression <$> fromGenLocated o)
        f (l:o:rs) =
          ( InfixExpr . mkExpression <$> fromGenLocated l
          , mkExpression <$> fromGenLocated o)
            <| f rs
        f _ = error "Malformed operator chain"
    build _ = error "Empty operator chain"
    findFixity o =
      fromMaybe GHC.defaultFixity $ lookup (GHC.varToStr o) fixities
    leftChain = reverse $ rhs : op : collect lhs
      where
        collect :: GHC.LHsExpr GHC.GhcPs -> [GHC.LHsExpr GHC.GhcPs]
        collect (GHC.L _ (GHC.OpApp _ l o r))
          | isSameAssoc o = r : o : collect l
        collect x = [x]
    rightChain = lhs : op : collect rhs
      where
        collect :: GHC.LHsExpr GHC.GhcPs -> [GHC.LHsExpr GHC.GhcPs]
        collect (GHC.L _ (GHC.OpApp _ l o r))
          | isSameAssoc o = l : o : collect r
        collect x = [x]
    isSameAssoc (findFixity -> fixity) =
      fixityLevel fixity == level && fixityDir fixity == dir
    level = fixityLevel operatorFixity
    dir = fixityDir operatorFixity
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
    fixityLevel (GHC.Fixity lvl _) = lvl
    
    fixityDir (GHC.Fixity _ direction) = direction
#else
    fixityLevel (GHC.Fixity _ lvl _) = lvl
    
    fixityDir (GHC.Fixity _ _ direction) = direction
#endif
mkExpression (GHC.NegApp _ expr _) =
  Negation $ mkExpression <$> fromGenLocated expr
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExpression (GHC.HsPar _ expr) =
  Parenthesized $ mkExpression <$> fromGenLocated expr
#elif MIN_VERSION_ghc_lib_parser(9, 8, 1)
mkExpression (GHC.HsPar _ _ expr _) =
  Parenthesized $ mkExpression <$> fromGenLocated expr
#else
mkExpression (GHC.HsPar _ expr) =
  Parenthesized $ mkExpression <$> fromGenLocated expr
#endif
mkExpression (GHC.SectionL _ l r) =
  LeftSection
    { left = mkExpression <$> fromGenLocated l
    , operator = InfixExpr . mkExpression <$> fromGenLocated r
    }
mkExpression (GHC.SectionR _ l r) =
  RightSection
    { operator = InfixExpr . mkExpression <$> fromGenLocated l
    , right = mkExpression <$> fromGenLocated r
    }
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExpression (GHC.ExplicitTuple _ tupleArgs boxity) =
  Tuple
    { arguments = mkTupleArgument <$> tupleArgs
    , isBoxed =
        case boxity of
          GHC.Boxed -> True
          GHC.Unboxed -> False
    }
  where
    mkTupleArgument (GHC.Present _ expr) =
      mkWithComments $ Just $ mkExpression <$> fromGenLocated expr
    mkTupleArgument (GHC.Missing ann) = fromEpAnn ann Nothing
#else
mkExpression (GHC.ExplicitTuple _ tupleArgs boxity) =
  Tuple
    { arguments = mkTupleArgument <$> tupleArgs
    , isBoxed =
        case boxity of
          GHC.Boxed -> True
          GHC.Unboxed -> False
    }
  where
    mkTupleArgument (GHC.Present ann expr) =
      fromEpAnn ann $ Just $ mkExpression <$> fromGenLocated expr
    mkTupleArgument (GHC.Missing ann) = fromEpAnn ann Nothing
#endif
mkExpression (GHC.ExplicitSum _ position arity expr) =
  Sum {expression = mkExpression <$> fromGenLocated expr, ..}
mkExpression (GHC.HsCase _ scrut matches) =
  CaseExpression
    { scrutinee = mkExpression <$> fromGenLocated scrut
    , matches = mkExprMatchGroup matches
    }
mkExpression (GHC.HsIf _ predicateExpr thenExpr elseExpr) =
  IfExpression
    { predicate = mkExpression <$> fromGenLocated predicateExpr
    , thenBranch = mkExpression <$> fromGenLocated thenExpr
    , elseBranch = mkExpression <$> fromGenLocated elseExpr
    }
mkExpression (GHC.HsMultiIf _ clauses) =
  MultiIf (fmap (fmap mkMultiWayIfExprGuard . fromGenLocated) clauses)
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExpression (GHC.HsLet _ localBinds body) =
  case mkLocalBinds localBinds of
    Nothing ->
      error
        "`ghc-lib-parser` never generates a `HsLet` node with empty bindings."
    Just bindings ->
      LetBinding {expression = mkExpression <$> fromGenLocated body, ..}
#elif MIN_VERSION_ghc_lib_parser(9, 8, 1)
mkExpression (GHC.HsLet _ _ localBinds _ body) =
  case mkLocalBinds localBinds of
    Nothing ->
      error
        "`ghc-lib-parser` never generates a `HsLet` node with empty bindings."
    Just bindings ->
      LetBinding {expression = mkExpression <$> fromGenLocated body, ..}
#else
mkExpression (GHC.HsLet _ localBinds body) =
  case mkLocalBinds localBinds of
    Nothing ->
      error
        "`ghc-lib-parser` never generates a `HsLet` node with empty bindings."
    Just bindings ->
      LetBinding {expression = mkExpression <$> fromGenLocated body, ..}
#endif
mkExpression (GHC.HsDo _ GHC.ListComp statements) =
  ListComprehension
    $ LC.mkListComprehension . fmap (fmap mkExprStatement . fromGenLocated)
        <$> fromGenLocated statements
mkExpression (GHC.HsDo _ GHC.MonadComp statements) =
  ListComprehension
    $ LC.mkListComprehension . fmap (fmap mkExprStatement . fromGenLocated)
        <$> fromGenLocated statements
mkExpression (GHC.HsDo _ (GHC.DoExpr moduleName) statements) =
  DoBlock
    { qualifiedDo = QualifiedDo (fmap mkModuleName moduleName) Do
    , statements =
        fmap (fmap mkExprStatement . fromGenLocated)
          <$> fromGenLocated statements
    }
mkExpression (GHC.HsDo _ (GHC.MDoExpr moduleName) statements) =
  DoBlock
    { qualifiedDo = QualifiedDo (fmap mkModuleName moduleName) Mdo
    , statements =
        fmap (fmap mkExprStatement . fromGenLocated)
          <$> fromGenLocated statements
    }
mkExpression (GHC.HsDo _ GHC.GhciStmtCtxt {} _) =
  error "`ghc-lib-parser` never generates this AST node."
mkExpression (GHC.ExplicitList _ items) =
  ListLiteral (fmap (fmap mkExpression . fromGenLocated) items)
mkExpression (GHC.RecordCon { GHC.rcon_con = conName
                            , GHC.rcon_flds = recordFields
                            }) =
  RecordConstruction
    { name = mkPrefixName <$> fromGenLocated conName
    , fields = mkRecordConstructionFields recordFields
    }
mkExpression (GHC.RecordUpd { GHC.rupd_expr = recordExpr
                            , GHC.rupd_flds = updates
                            }) =
  RecordUpdate
    $ mkRecordUpdateFields (mkExpression <$> fromGenLocated recordExpr) updates
mkExpression (GHC.HsGetField {GHC.gf_expr = fieldExpr, GHC.gf_field = selector}) =
  FieldProjection
    { expression = mkExpression <$> fromGenLocated fieldExpr
    , selector = fmap mkFieldSelector (fromGenLocated selector)
    }
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkExpression (GHC.HsProjection {GHC.proj_flds = fields}) =
  Projection $ fmap (mkWithComments . mkFieldSelector) fields
#else
mkExpression (GHC.HsProjection {GHC.proj_flds = fields}) =
  Projection $ fmap (fmap mkFieldSelector . fromGenLocated) fields
#endif
mkExpression (GHC.ExprWithTySig _ signatureExpr ty) =
  TypeSignature
    { expression = mkExpression <$> fromGenLocated signatureExpr
    , signature =
        flattenComments
          $ mkTypeFromHsSigType <$> fromGenLocated (GHC.hswc_body ty)
    }
mkExpression (GHC.ArithSeq _ _ info) =
  ArithmeticSequence $ mkRangeExpression info
mkExpression (GHC.HsTypedBracket _ typedExpr) =
  TypedQuotation $ mkExpression <$> fromGenLocated typedExpr
mkExpression (GHC.HsUntypedBracket _ content) =
  UntypedQuotation $ mkBracket content
#if MIN_VERSION_ghc_lib_parser(9, 12, 2)
mkExpression GHC.HsForAll {} =
  error "`ghc-lib-parser` never generates this AST node."
mkExpression GHC.HsQual {} =
  error "`ghc-lib-parser` never generates this AST node."
mkExpression GHC.HsFunArr {} =
  error "`ghc-lib-parser` never generates this AST node."
#endif
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkExpression (GHC.HsTypedSplice _ expr) = Splice $ mkTypedSplice expr
mkExpression (GHC.HsUntypedSplice _ splice) = Splice $ mkSplice splice
#else
mkExpression (GHC.HsSpliceE _ splice) = Splice $ mkSplice splice
mkExpression (GHC.HsTypedSplice _ expr) = Splice $ mkSplice expr
mkExpression (GHC.HsUntypedSplice _ splice) = Splice $ mkSplice splice
#endif
mkExpression (GHC.HsProc _ pat command) =
  case getFirst $ foldMap (First . mkCmdDoBlock) cmd of
    Just block -> ProcDo {pat = mkPattern <$> fromGenLocated pat, block}
    Nothing -> ProcExpression {pat = mkPattern <$> fromGenLocated pat, cmd}
  where
    cmd = flattenComments $ mkCmdFromHsCmdTop <$> fromGenLocated command
mkExpression (GHC.HsStatic _ staticExpr) =
  StaticExpression $ mkExpression <$> fromGenLocated staticExpr
mkExpression (GHC.HsPragE _ pragma pragmaticExpr) =
  PragmaticExpression
    { expression = mkExpression <$> fromGenLocated pragmaticExpr
    , pragma = mkExpressionPragma pragma
    }
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkExpression (GHC.HsRecSel _ _) =
  error "`ghc-lib-parser` never generates this AST node."
#endif
newtype InfixExpr =
  InfixExpr Expression

instance CommentExtraction InfixExpr where
  nodeComments _ = NodeComments [] [] []

instance Pretty InfixExpr where
  pretty' (InfixExpr (Variable name)) = pretty $ mkPrefixAsInfix <$> name
  pretty' (InfixExpr (UnboundVariable name)) = pretty $ mkPrefixAsInfix name
  pretty' (InfixExpr (Parenthesized inner)) = pretty $ fmap InfixExpr inner
  pretty' (InfixExpr x) = pretty x

data GuardExpression
  = GuardWithDo Expression
  | GuardWithAppAndDo Expression
  | GuardExpression Expression

instance CommentExtraction GuardExpression where
  nodeComments (GuardWithDo expr) = nodeComments expr
  nodeComments (GuardWithAppAndDo expr) = nodeComments expr
  nodeComments (GuardExpression expr) = nodeComments expr

instance Pretty GuardExpression where
  pretty' (GuardWithDo expr) = space >> pretty expr
  pretty' (GuardWithAppAndDo expr) = space >> pretty expr
  pretty' (GuardExpression expr) = horizontal <-|> vertical
    where
      horizontal = space >> pretty expr
      vertical = newline >> indentedBlock (pretty expr)

mkGuardExpression :: Expression -> GuardExpression
mkGuardExpression expr@DoBlock {} = GuardWithDo expr
mkGuardExpression expr
  | OperatorApplication {..} <- expr
  , DoBlock {} <- getNode firstOperand = GuardWithAppAndDo expr
  | otherwise = GuardExpression expr
