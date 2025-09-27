{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HIndent.Ast.Expression
  ( Expression
  , InfixExpr
  , DoBlockExpression
  , GuardExpression
  , mkExpression
  , mkDoBlockExpression
  , mkGuardExpression
  ) where

import Control.Monad
import Control.Monad.RWS (gets)
import Data.List.NonEmpty (NonEmpty(..))
import qualified GHC.Hs as GHC
import qualified GHC.Types.Basic as GHC
import qualified GHC.Types.Fixity as Fixity
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Expression.Bracket (Bracket, mkBracket)
import HIndent.Ast.Expression.OverloadedLabel
  ( OverloadedLabel
  , mkOverloadedLabel
  )
import HIndent.Ast.Expression.RangeExpression
  ( RangeExpression
  , mkRangeExpression
  )
import HIndent.Ast.Guard (mkMultiWayIfExprGuard)
import HIndent.Ast.Module.Name (ModuleName, mkModuleName)
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.Pattern
import HIndent.Ast.Type
import HIndent.Ast.Type.ImplicitParameterName
  ( ImplicitParameterName
  , mkImplicitParameterName
  )
import HIndent.Ast.WithComments
import HIndent.CabalFile ()
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty, printCommentsAnd)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
import HIndent.Printer
import qualified Language.Haskell.Syntax.Basic as HS
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
import HIndent.Ast.Expression.Splice (Splice, mkSplice, mkSpliceExpression)
#else
import qualified GHC.Types.Name.Reader as NameReader
import HIndent.Ast.Expression.Splice (Splice, mkSplice)
#endif
newtype InfixExpr =
  InfixExpr (WithComments Expression)

data Expression
  = Variable (WithComments PrefixName)
  | UnboundVariable PrefixName
  | OverloadedLabel OverloadedLabel
  | ImplicitParameter ImplicitParameterName
  | OverloadedLiteral (GHC.HsOverLit GHC.GhcPs)
  | Literal (GHC.HsLit GHC.GhcPs)
  | Lambda (GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
  | LambdaCase
      { usesCases :: Bool
      , matches :: GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
      }
  | Application
      { function :: WithComments Expression
      , argument :: WithComments Expression
      }
  | TypeApplication
      { expression :: WithComments Expression
      , typeArg :: GHC.LHsWcType (GHC.NoGhcTc GHC.GhcPs)
      }
  | OperatorApplication
      { left :: WithComments Expression
      , operator :: WithComments Expression
      , right :: WithComments Expression
      }
  | Negation (WithComments Expression)
  | Parenthesized (WithComments Expression)
  | LeftSection
      { left :: WithComments Expression
      , operator :: WithComments Expression
      }
  | RightSection
      { operator :: WithComments Expression
      , right :: WithComments Expression
      }
  | Tuple
      { arguments :: [GHC.HsTupArg GHC.GhcPs]
      , boxity :: GHC.Boxity
      }
  | Sum
      { position :: GHC.ConTag
      , arity :: HS.SumWidth
      , expression :: WithComments Expression
      }
  | CaseExpression
      { scrutinee :: WithComments Expression
      , matches :: GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
      }
  | IfExpression
      { predicate :: WithComments Expression
      , thenBranch :: WithComments Expression
      , elseBranch :: WithComments Expression
      }
  | MultiIf [GHC.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)]
  | LetBinding
      { bindings :: GHC.HsLocalBinds GHC.GhcPs
      , expression :: WithComments Expression
      }
  | DoBlock
      { doModuleName :: Maybe ModuleName
      , doKind :: DoOrMdo
      , statements :: GHC.XRec GHC.GhcPs [GHC.ExprLStmt GHC.GhcPs]
      }
  | ListComprehensionExpression
      { listCompHead :: GHC.ExprLStmt GHC.GhcPs
      , listCompRest :: NonEmpty (GHC.ExprLStmt GHC.GhcPs)
      , listCompStatements :: GHC.XRec GHC.GhcPs [GHC.ExprLStmt GHC.GhcPs]
      }
  | ListLiteral [WithComments Expression]
  | RecordConstruction
      { name :: WithComments PrefixName
      , binds :: GHC.HsRecordBinds GHC.GhcPs
      }
  | RecordUpdate
      { expression :: WithComments Expression
      , updates :: GHC.LHsRecUpdFields GHC.GhcPs
      }
  | FieldProjection
      { expression :: WithComments Expression
      , selector :: GHC.XRec GHC.GhcPs (GHC.DotFieldOcc GHC.GhcPs)
      }
  | Projection (NonEmpty (GHC.XRec GHC.GhcPs (GHC.DotFieldOcc GHC.GhcPs)))
  | TypeSignature
      { expression :: WithComments Expression
      , signature :: WithComments Type
      }
  | ArithmeticSequence RangeExpression
  | TypedQuotation (WithComments Expression)
  | UntypedQuotation Bracket
  | TypedSplice Splice
  | UntypedSplice Splice
  | Splice Splice
  | ProcExpression
      { pat :: WithComments Pattern
      , cmd :: GHC.LHsCmdTop GHC.GhcPs
      }
  | StaticExpression (WithComments Expression)
  | PragmaticExpression
      { pragma :: GHC.HsPragE GHC.GhcPs
      , expression :: WithComments Expression
      }

instance CommentExtraction InfixExpr where
  nodeComments (InfixExpr exprWith) = nodeComments exprWith

instance Pretty InfixExpr where
  pretty' (InfixExpr exprWith) =
    case getNode exprWith of
      Variable nameWith -> pretty (mkPrefixAsInfix <$> nameWith)
      UnboundVariable name -> pretty $ mkPrefixAsInfix name
      Parenthesized inner -> pretty (InfixExpr inner)
      exprNode -> pretty exprNode

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
  pretty' LambdaCase {usesCases, matches} = do
    string
      $ if usesCases
          then "\\cases"
          else "\\case"
    if null $ GHC.unLoc $ GHC.mg_alts matches
      then string " {}"
      else do
        newline
        indentedBlock $ pretty matches
  pretty' Application {..} = horizontal <-|> vertical
    where
      horizontal = spaced [pretty function, pretty argument]
      vertical =
        case flattenApplications function <> [argument] of
          [] -> error "Invalid function application."
          (fExpr:argsExprs) -> do
            col <- gets psColumn
            spaces <- getIndentSpaces
            pretty fExpr
            col' <- gets psColumn
            let diff =
                  col'
                    - col
                    - if col == 0
                        then spaces
                        else 0
            if diff + 1 <= spaces
              then space
              else newline
            spaces' <- getIndentSpaces
            indentedWithSpace spaces' $ lined $ fmap pretty argsExprs
      flattenApplications ::
           WithComments Expression -> [WithComments Expression]
      flattenApplications exprWith =
        case getNode exprWith of
          Application {function = funcExpr, argument = argExpr} ->
            let outerComments = getComments exprWith
                argWith = addComments outerComments argExpr
             in flattenApplications funcExpr <> [argWith]
          _ -> [exprWith]
  pretty' TypeApplication {..} = do
    pretty expression
    string " @"
    pretty typeArg
  pretty' OperatorApplication {..} = horizontal <-|> vertical
    where
      horizontal =
        spaced [pretty left, pretty (InfixExpr operator), pretty right]
      vertical =
        case fixityDirection operatorFixity of
          Fixity.InfixL -> leftAssoc
          Fixity.InfixR -> rightAssoc
          Fixity.InfixN -> noAssoc
      leftAssoc = prettyOps allOperandsAndOperatorsLeftAssoc
      rightAssoc = prettyOps allOperandsAndOperatorsRightAssoc
      noAssoc
        | OperatorApplication {operator = op'} <- getNode left
        , isSameAssoc op' = leftAssoc
        | otherwise = rightAssoc
      prettyOps [lExpr, opExpr, rExpr]
        | Just (qualifier, doKind, stmts) <- matchDoBlock rExpr = do
          spaced
            [ pretty lExpr
            , pretty (InfixExpr opExpr)
            , pretty $ QualifiedDo qualifier doKind
            ]
          newline
          indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
      prettyOps [lExpr, opExpr, rExpr]
        | isLambdaLike rExpr =
          spaced [pretty lExpr, pretty (InfixExpr opExpr), pretty rExpr]
      prettyOps (lExpr:rest) = do
        pretty lExpr
        newline
        indentedBlock $ go rest
        where
          go (opExpr:expr:rems) = do
            (pretty (InfixExpr opExpr) >> space) |=> pretty expr
            unless (null rems) $ newline >> go rems
          go _ =
            error
              "The number of the sum of operands and operators should be odd."
      prettyOps _ = error "Too short list."
      findFixity opExpr =
        maybe Fixity.defaultFixity prefixAsInfixFixity (operatorPrefix opExpr)
      allOperandsAndOperatorsLeftAssoc =
        reverse $ right : operator : collectLeft left
      allOperandsAndOperatorsRightAssoc = left : operator : collectRight right
      collectLeft exprWith =
        case getNode exprWith of
          OperatorApplication {left = l', operator = op', right = r'}
            | isSameAssoc op' -> r' : op' : collectLeft l'
          _ -> [exprWith]
      collectRight exprWith =
        case getNode exprWith of
          OperatorApplication {left = l', operator = op', right = r'}
            | isSameAssoc op' -> l' : op' : collectRight r'
          _ -> [exprWith]
      isSameAssoc opExpr =
        let fixity = findFixity opExpr
         in fixityPrecedence fixity == currentPrecedence
              && fixityDirection fixity == currentDirection
      operatorFixity = findFixity operator
      currentPrecedence = fixityPrecedence operatorFixity
      currentDirection = fixityDirection operatorFixity
      matchDoBlock exprWith =
        case getNode exprWith of
          DoBlock {doModuleName, doKind, statements = stmts} ->
            Just (doModuleName, doKind, stmts)
          _ -> Nothing
      operatorPrefix exprWith =
        case getNode exprWith of
          Variable nameWith -> Just $ mkPrefixAsInfix (getNode nameWith)
          UnboundVariable name -> Just $ mkPrefixAsInfix name
          Parenthesized inner -> operatorPrefix inner
          _ -> Nothing
      isLambdaLike exprWith =
        case getNode exprWith of
          Lambda {} -> True
          LambdaCase {} -> True
          _ -> False
  pretty' (Negation expr) = string "-" >> pretty expr
  pretty' (Parenthesized expr) = parens $ pretty expr
  pretty' LeftSection {..} = spaced [pretty left, pretty (InfixExpr operator)]
  pretty' RightSection {..} =
    (pretty (InfixExpr operator) >> space) |=> pretty right
  pretty' Tuple {arguments, boxity} = horizontal <-|> vertical
    where
      horizontal = parH $ fmap pretty arguments
      vertical =
        parV
          $ prefixedLined ","
          $ fmap (\e -> unless (isMissing e) (space |=> pretty e)) arguments
      isMissing GHC.Missing {} = True
      isMissing _ = False
      parH =
        case boxity of
          GHC.Boxed -> hTuple
          GHC.Unboxed -> hUnboxedTuple
      parV =
        case boxity of
          GHC.Boxed -> parens
          GHC.Unboxed -> unboxedParens
  pretty' Sum {..} = do
    string "(#"
    forM_ [1 .. arity] $ \idx -> do
      if idx == position
        then string " " >> pretty expression >> string " "
        else string " "
      when (idx < arity) $ string "|"
    string "#)"
  pretty' CaseExpression {scrutinee, matches} = do
    string "case " |=> do
      pretty scrutinee
      string " of"
    if null $ GHC.unLoc $ GHC.mg_alts matches
      then string " {}"
      else do
        newline
        indentedBlock $ pretty matches
  pretty' IfExpression {predicate, thenBranch, elseBranch} = do
    string "if " |=> pretty predicate
    indentedBlock
      $ newlinePrefixed [branch "then " thenBranch, branch "else " elseBranch]
    where
      branch :: String -> WithComments Expression -> Printer ()
      branch str exprWithComments =
        prettyWith exprWithComments $ \expressionNode ->
          case mkDoBlockExpression expressionNode of
            Just doBlockExpression -> do
              string str
              pretty doBlockExpression
            Nothing -> fallback expressionNode
        where
          fallback exprNode = string str |=> pretty exprNode
  pretty' (MultiIf multiIfClauses) =
    string "if "
      |=> lined
            (fmap
               (pretty . fmap mkMultiWayIfExprGuard . fromGenLocated)
               multiIfClauses)
  pretty' LetBinding {..} =
    lined
      [string "let " |=> pretty bindings, string " in " |=> pretty expression]
  pretty' DoBlock {..} =
    case statements of
      GHC.L l xs ->
        pretty $ GHC.L l $ DoExpression xs (QualifiedDo doModuleName doKind)
  pretty' ListComprehensionExpression { listCompHead = lhs
                                      , listCompRest = rest
                                      , listCompStatements = GHC.L l _
                                      } =
    pretty $ GHC.L l $ ListComprehension lhs rest
  pretty' (ListLiteral listItems) = horizontal <-|> vertical
    where
      horizontal = brackets $ hCommaSep $ fmap pretty listItems
      vertical = vList $ fmap pretty listItems
  pretty' RecordConstruction {..} = horizontal <-|> vertical
    where
      horizontal = spaced [pretty name, pretty binds]
      vertical = do
        pretty name
        (space >> pretty binds) <-|> (newline >> indentedBlock (pretty binds))
  pretty' RecordUpdate {..} = horizontal <-|> vertical
    where
      fields = mkRecordUpdateFields updates
      horizontal =
        spaced [pretty expression, hFields (fmap renderFieldHorizontal fields)]
      vertical = do
        pretty expression
        newline
        indentedBlock
          $ hFields (fmap renderFieldHorizontal fields)
              <-|> vFields (fmap renderFieldVertical fields)
      renderFieldHorizontal field =
        prettyWith field $ \RecordUpdateFieldDetails {..} ->
          case recordUpdateFieldValue of
            Nothing -> pretty recordUpdateFieldLabel
            Just value -> do
              pretty recordUpdateFieldLabel
              string " = "
              pretty value
      renderFieldVertical field =
        prettyWith field $ \RecordUpdateFieldDetails {..} ->
          case recordUpdateFieldValue of
            Nothing -> pretty recordUpdateFieldLabel
            Just value -> do
              pretty recordUpdateFieldLabel
              string " ="
              newline
              indentedBlock $ pretty value
  pretty' FieldProjection {..} = do
    pretty expression
    dot
    pretty selector
  pretty' (Projection projectionFields) =
    parens
      $ forM_ projectionFields
      $ \x -> do
          string "."
          pretty x
  pretty' TypeSignature {..} = do
    pretty expression
    string " :: "
    pretty signature
  pretty' (ArithmeticSequence rangeExpression) = pretty rangeExpression
  pretty' (TypedQuotation expr) = typedBrackets $ pretty expr
  pretty' (UntypedQuotation bracket) = pretty bracket
  pretty' ProcExpression {..} =
    case cmd of
      x@(GHC.L _ (GHC.HsCmdTop _ (GHC.L _ (GHC.HsCmdDo _ stmts)))) -> do
        spaced [string "proc", pretty pat, string "-> do"]
        newline
        indentedBlock
          $ printCommentsAnd
              x
              (const (printCommentsAnd stmts (lined . fmap pretty)))
      _ -> hor <-|> ver
    where
      hor = spaced [string "proc", pretty pat, string "->", pretty cmd]
      ver = do
        spaced [string "proc", pretty pat, string "->"]
        newline
        indentedBlock (pretty cmd)
  pretty' (StaticExpression expr) = spaced [string "static", pretty expr]
  pretty' PragmaticExpression {..} = spaced [pretty pragma, pretty expression]
  pretty' (TypedSplice splice') = pretty splice'
  pretty' (UntypedSplice splice') = pretty splice'
  pretty' (Splice splice') = pretty splice'

type RecordUpdateField = WithComments RecordUpdateFieldDetails

data RecordUpdateFieldDetails = RecordUpdateFieldDetails
  { recordUpdateFieldLabel :: RecordUpdateLabel
  , recordUpdateFieldValue :: Maybe (WithComments Expression)
  }

type RecordUpdateLabel = WithComments RecordUpdateLabelDetails

data RecordUpdateLabelDetails
  = RecordUpdateFieldOccLabel PrefixName
  | RecordUpdateAmbiguousFieldOccLabel PrefixName
  | RecordUpdateFieldLabelStringsLabel (GHC.FieldLabelStrings GHC.GhcPs)

instance CommentExtraction RecordUpdateLabelDetails where
  nodeComments _ = NodeComments [] [] []

instance Pretty RecordUpdateLabelDetails where
  pretty' (RecordUpdateFieldOccLabel name) = pretty name
  pretty' (RecordUpdateAmbiguousFieldOccLabel name) = pretty name
  pretty' (RecordUpdateFieldLabelStringsLabel labels) = pretty labels

mkRecordUpdateFields :: GHC.LHsRecUpdFields GHC.GhcPs -> [RecordUpdateField]
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkRecordUpdateFields GHC.RegularRecUpdFields {recUpdFields} =
  fmap (mkRecordUpdateFieldFromFieldBindWith wrapFieldOcc) recUpdFields
mkRecordUpdateFields GHC.OverloadedRecUpdFields {olRecUpdFields} =
  fmap
    (mkRecordUpdateFieldFromFieldBindWith wrapFieldLabelStrings)
    olRecUpdFields
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkRecordUpdateFields GHC.RegularRecUpdFields {recUpdFields} =
  fmap (mkRecordUpdateFieldFromFieldBindWith wrapAmbiguousField) recUpdFields
mkRecordUpdateFields GHC.OverloadedRecUpdFields {olRecUpdFields} =
  fmap mkRecordUpdateFieldFromRecProj olRecUpdFields
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkRecordUpdateFields =
  fmap (mkRecordUpdateFieldFromFieldBindWith wrapFieldOcc) . either id id
#else
mkRecordUpdateFields = fmap mkRecordUpdateFieldFromRecField . either id id
#endif
mkRecordUpdateFieldFromFieldBindWith ::
     CommentExtraction l
  => (GHC.XRec GHC.GhcPs label -> RecordUpdateLabel)
  -> GHC.GenLocated
       l
       (GHC.HsFieldBind (GHC.XRec GHC.GhcPs label) (GHC.LHsExpr GHC.GhcPs))
  -> RecordUpdateField
mkRecordUpdateFieldFromFieldBindWith wrapLabel fieldBind =
  fmap toDetails (fromGenLocated fieldBind)
  where
    toDetails GHC.HsFieldBind {..} =
      RecordUpdateFieldDetails
        { recordUpdateFieldLabel = wrapLabel hfbLHS
        , recordUpdateFieldValue =
            if hfbPun
              then Nothing
              else Just (mkExpressionWithComments hfbRHS)
        }
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkRecordUpdateFieldFromRecField field = fmap toDetails (fromGenLocated field)
  where
    toDetails GHC.HsRecField {..} =
      RecordUpdateFieldDetails
        { recordUpdateFieldLabel = wrapFieldOcc hsRecFieldLbl
        , recordUpdateFieldValue =
            if hsRecPun
              then Nothing
              else Just (mkExpressionWithComments hsRecFieldArg)
        }
#endif
#if MIN_VERSION_ghc_lib_parser(9, 6, 1) && !MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkRecordUpdateFieldFromRecProj ::
     GHC.LHsRecUpdProj GHC.GhcPs -> RecordUpdateField
mkRecordUpdateFieldFromRecProj =
  mkRecordUpdateFieldFromFieldBindWith wrapFieldLabelStrings
#endif
mkExpressionWithComments :: GHC.LHsExpr GHC.GhcPs -> WithComments Expression
mkExpressionWithComments = fmap mkExpression . fromGenLocated
#if !MIN_VERSION_ghc_lib_parser(9, 6, 1) || MIN_VERSION_ghc_lib_parser(9, 12, 1)
wrapFieldOcc :: GHC.XRec GHC.GhcPs (GHC.FieldOcc GHC.GhcPs) -> RecordUpdateLabel
wrapFieldOcc =
  fmap (RecordUpdateFieldOccLabel . fieldOccToPrefixName) . fromGenLocated

fieldOccToPrefixName :: GHC.FieldOcc GHC.GhcPs -> PrefixName
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
fieldOccToPrefixName GHC.FieldOcc {foLabel} = mkPrefixName (GHC.unLoc foLabel)
#else
fieldOccToPrefixName GHC.FieldOcc {rdrNameFieldOcc} =
  mkPrefixName rdrNameFieldOcc
#endif
#endif
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
wrapAmbiguousField ::
     GHC.XRec GHC.GhcPs (GHC.AmbiguousFieldOcc GHC.GhcPs) -> RecordUpdateLabel
wrapAmbiguousField =
  fmap (RecordUpdateAmbiguousFieldOccLabel . ambiguousFieldOccToPrefixName)
    . fromGenLocated
#endif
wrapFieldLabelStrings ::
     GHC.XRec GHC.GhcPs (GHC.FieldLabelStrings GHC.GhcPs) -> RecordUpdateLabel
wrapFieldLabelStrings = fmap RecordUpdateFieldLabelStringsLabel . fromGenLocated
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
ambiguousFieldOccToPrefixName :: GHC.AmbiguousFieldOcc GHC.GhcPs -> PrefixName
ambiguousFieldOccToPrefixName (GHC.Unambiguous _ name) =
  mkPrefixName (GHC.unLoc name)
ambiguousFieldOccToPrefixName (GHC.Ambiguous _ name) =
  mkPrefixName (GHC.unLoc name)
#endif
mkExpression :: GHC.HsExpr GHC.GhcPs -> Expression
mkExpression (GHC.HsVar _ name) = Variable (withPrefixName name)
mkExpression (GHC.HsUnboundVar _ name) =
  UnboundVariable (mkUnboundPrefixName name)
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkExpression (GHC.HsOverLabel _ label) =
  OverloadedLabel (mkOverloadedLabel label)
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkExpression (GHC.HsOverLabel _ _ label) =
  OverloadedLabel (mkOverloadedLabel label)
#else
mkExpression (GHC.HsOverLabel _ label) =
  OverloadedLabel (mkOverloadedLabel label)
#endif
mkExpression (GHC.HsIPVar _ name) =
  ImplicitParameter (mkImplicitParameterName name)
mkExpression (GHC.HsOverLit _ lit) = OverloadedLiteral lit
mkExpression (GHC.HsLit _ lit) = Literal lit
#if __GLASGOW_HASKELL__ < 908
mkExpression (GHC.HsEmbTy _ _) =
  error "`ghc-lib-parser` never generates this AST node."
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExpression (GHC.HsLam _ GHC.LamSingle matches) = Lambda matches
mkExpression (GHC.HsLam _ GHC.LamCases matches) =
  LambdaCase {usesCases = True, ..}
mkExpression (GHC.HsLam _ GHC.LamCase matches) =
  LambdaCase {usesCases = False, ..}
#else
mkExpression (GHC.HsLam _ matches) = Lambda matches
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkExpression (GHC.HsLamCase _ GHC.LamCases matches) =
  LambdaCase {usesCases = True, ..}
mkExpression (GHC.HsLamCase _ GHC.LamCase matches) =
  LambdaCase {usesCases = False, ..}
#else
mkExpression (GHC.HsLamCase _ _ matches) = LambdaCase {usesCases = False, ..}
#endif
#endif
mkExpression (GHC.HsApp _ function argument) =
  Application
    { function = mkExpression <$> fromGenLocated function
    , argument = mkExpression <$> fromGenLocated argument
    }
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExpression (GHC.HsAppType _ fun ty) =
  TypeApplication
    {expression = mkExpression <$> fromGenLocated fun, typeArg = ty}
#else
mkExpression (GHC.HsAppType _ fun _ ty) =
  TypeApplication
    {expression = mkExpression <$> fromGenLocated fun, typeArg = ty}
#endif
mkExpression (GHC.OpApp _ l op r) =
  OperatorApplication
    { left = mkExpression <$> fromGenLocated l
    , operator = mkExpression <$> fromGenLocated op
    , right = mkExpression <$> fromGenLocated r
    }
mkExpression (GHC.NegApp _ expr' _) =
  Negation (mkExpression <$> fromGenLocated expr')
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExpression (GHC.HsPar _ expr') =
  Parenthesized (mkExpression <$> fromGenLocated expr')
#elif MIN_VERSION_ghc_lib_parser(9, 8, 1)
mkExpression (GHC.HsPar _ _ expr' _) =
  Parenthesized (mkExpression <$> fromGenLocated expr')
#else
mkExpression (GHC.HsPar _ expr') =
  Parenthesized (mkExpression <$> fromGenLocated expr')
#endif
mkExpression (GHC.SectionL _ l r) =
  LeftSection
    { left = mkExpression <$> fromGenLocated l
    , operator = mkExpression <$> fromGenLocated r
    }
mkExpression (GHC.SectionR _ l r) =
  RightSection
    { operator = mkExpression <$> fromGenLocated l
    , right = mkExpression <$> fromGenLocated r
    }
mkExpression (GHC.ExplicitTuple _ args boxity) =
  Tuple {arguments = args, boxity = boxity}
mkExpression (GHC.ExplicitSum _ position arity expr') =
  Sum
    { position = position
    , arity = arity
    , expression = mkExpression <$> fromGenLocated expr'
    }
mkExpression (GHC.HsCase _ scrut matches) =
  CaseExpression
    {scrutinee = mkExpression <$> fromGenLocated scrut, matches = matches}
mkExpression (GHC.HsIf _ pred' then' else') =
  IfExpression
    { predicate = mkExpression <$> fromGenLocated pred'
    , thenBranch = mkExpression <$> fromGenLocated then'
    , elseBranch = mkExpression <$> fromGenLocated else'
    }
mkExpression (GHC.HsMultiIf _ clauses) = MultiIf clauses
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExpression (GHC.HsLet _ binds body) =
  LetBinding
    {bindings = binds, expression = mkExpression <$> fromGenLocated body}
#elif MIN_VERSION_ghc_lib_parser(9, 8, 1)
mkExpression (GHC.HsLet _ _ binds _ body) =
  LetBinding
    {bindings = binds, expression = mkExpression <$> fromGenLocated body}
#else
mkExpression (GHC.HsLet _ binds body) =
  LetBinding
    {bindings = binds, expression = mkExpression <$> fromGenLocated body}
#endif
mkExpression (GHC.HsDo _ GHC.ListComp (GHC.L _ [])) =
  error "List comprehension requires at least two statements."
mkExpression (GHC.HsDo _ GHC.ListComp (GHC.L _ [_])) =
  error "List comprehension requires at least two statements."
mkExpression (GHC.HsDo _ GHC.ListComp located@(GHC.L _ (lhs:rhs:rhss))) =
  ListComprehensionExpression
    { listCompHead = lhs
    , listCompRest = rhs :| rhss
    , listCompStatements = located
    }
mkExpression (GHC.HsDo _ GHC.MonadComp (GHC.L _ [])) =
  error "List comprehension requires at least two statements."
mkExpression (GHC.HsDo _ GHC.MonadComp (GHC.L _ [_])) =
  error "List comprehension requires at least two statements."
mkExpression (GHC.HsDo _ GHC.MonadComp located@(GHC.L _ (lhs:rhs:rhss))) =
  ListComprehensionExpression
    { listCompHead = lhs
    , listCompRest = rhs :| rhss
    , listCompStatements = located
    }
mkExpression (GHC.HsDo _ flavor stmts) =
  case flavor of
    GHC.DoExpr moduleName ->
      DoBlock
        { doModuleName = fmap mkModuleName moduleName
        , doKind = Do
        , statements = stmts
        }
    GHC.MDoExpr moduleName ->
      DoBlock
        { doModuleName = fmap mkModuleName moduleName
        , doKind = Mdo
        , statements = stmts
        }
    GHC.GhciStmtCtxt {} ->
      error "`ghc-lib-parser` never generates this AST node."
mkExpression (GHC.ExplicitList _ items) =
  ListLiteral (fmap (fmap mkExpression . fromGenLocated) items)
mkExpression (GHC.RecordCon {GHC.rcon_con = conName, GHC.rcon_flds = binds}) =
  RecordConstruction {name = fmap mkPrefixName (fromGenLocated conName), binds}
mkExpression (GHC.RecordUpd {GHC.rupd_expr = expr', GHC.rupd_flds = updates}) =
  RecordUpdate {expression = mkExpression <$> fromGenLocated expr', updates}
mkExpression (GHC.HsGetField {GHC.gf_expr = expr', GHC.gf_field = selector}) =
  FieldProjection {expression = mkExpression <$> fromGenLocated expr', selector}
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkExpression (GHC.HsProjection {GHC.proj_flds = fields}) =
  Projection (GHC.noLocA <$> fields)
#else
mkExpression (GHC.HsProjection {GHC.proj_flds = fields}) = Projection fields
#endif
mkExpression (GHC.ExprWithTySig _ expr' ty) =
  TypeSignature
    { expression = mkExpression <$> fromGenLocated expr'
    , signature = mkSignatureType ty
    }
mkExpression (GHC.ArithSeq _ _ info) =
  ArithmeticSequence (mkRangeExpression info)
mkExpression (GHC.HsTypedBracket _ expr') =
  TypedQuotation (mkExpression <$> fromGenLocated expr')
mkExpression (GHC.HsUntypedBracket _ content) =
  UntypedQuotation (mkBracket content)
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkExpression (GHC.HsTypedSplice _ expr') =
  TypedSplice (mkSpliceExpression expr')
mkExpression (GHC.HsUntypedSplice _ splice') = UntypedSplice (mkSplice splice')
#else
mkExpression (GHC.HsSpliceE _ splice') = Splice (mkSplice splice')
mkExpression (GHC.HsTypedSplice _ expr') = TypedSplice (mkSplice expr')
mkExpression (GHC.HsUntypedSplice _ splice') = UntypedSplice (mkSplice splice')
#endif
mkExpression (GHC.HsProc _ pat body) =
  ProcExpression {pat = fmap mkPattern (fromGenLocated pat), cmd = body}
mkExpression (GHC.HsStatic _ expr') =
  StaticExpression (mkExpression <$> fromGenLocated expr')
mkExpression (GHC.HsPragE _ pragma expr') =
  PragmaticExpression
    {pragma = pragma, expression = mkExpression <$> fromGenLocated expr'}
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkExpression (GHC.HsRecSel _ _) =
  error "`ghc-lib-parser` never generates this AST node."
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1) && __GLASGOW_HASKELL__ >= 910
mkExpression _ =
  error "mkExpression: unsupported expression variant encountered."
#endif
withPrefixName :: GHC.LIdP GHC.GhcPs -> WithComments PrefixName
withPrefixName = fmap mkPrefixName . fromGenLocated

mkUnboundPrefixName :: GHC.IdP GHC.GhcPs -> PrefixName
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
mkUnboundPrefixName = mkPrefixName
#else
mkUnboundPrefixName occ = mkPrefixName (NameReader.mkRdrUnqual occ)
#endif
mkSignatureType :: GHC.LHsSigWcType (GHC.NoGhcTc GHC.GhcPs) -> WithComments Type
mkSignatureType ty =
  flattenComments $ mkTypeFromHsSigType <$> fromGenLocated (GHC.hswc_body ty)

fixityPrecedence :: Fixity.Fixity -> Int
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
fixityPrecedence (Fixity.Fixity precedence _) = precedence
#else
fixityPrecedence (Fixity.Fixity _ precedence _) = precedence
#endif
fixityDirection :: Fixity.Fixity -> Fixity.FixityDirection
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
fixityDirection (Fixity.Fixity _ direction) = direction
#else
fixityDirection (Fixity.Fixity _ _ direction) = direction
#endif
data DoBlockExpression = DoBlockExpression
  { moduleName :: Maybe ModuleName
  , statements :: GHC.XRec GHC.GhcPs [GHC.ExprLStmt GHC.GhcPs]
  , kind :: DoOrMdo
  }

instance CommentExtraction DoBlockExpression where
  nodeComments _ = NodeComments [] [] []

instance Pretty DoBlockExpression where
  pretty' DoBlockExpression {..} = do
    pretty (QualifiedDo moduleName kind)
    newline
    indentedBlock $ printCommentsAnd statements (lined . fmap pretty)

mkDoBlockExpression :: Expression -> Maybe DoBlockExpression
mkDoBlockExpression DoBlock {..} =
  Just
    DoBlockExpression
      {moduleName = doModuleName, statements = statements, kind = doKind}
mkDoBlockExpression _ = Nothing

newtype GuardExpression =
  GuardExpression (WithComments Expression)

instance CommentExtraction GuardExpression where
  nodeComments (GuardExpression exprWithComments) =
    nodeComments exprWithComments

instance Pretty GuardExpression where
  pretty' (GuardExpression exprWithComments) =
    prettyWith exprWithComments (prettyGuardExpression exprWithComments)

mkGuardExpression :: WithComments Expression -> GuardExpression
mkGuardExpression = GuardExpression

prettyGuardExpression :: WithComments Expression -> Expression -> Printer ()
prettyGuardExpression exprWithComments expressionNode =
  case mkDoBlockExpression expressionNode of
    Just doBlockExpression -> do
      space
      pretty doBlockExpression
    Nothing
      | isDoLeftOperator -> space >> pretty exprWithComments
      | otherwise ->
        let horizontal = space >> pretty exprWithComments
            vertical = newline >> indentedBlock (pretty exprWithComments)
         in horizontal <-|> vertical
  where
    isDoLeftOperator =
      case expressionNode of
        OperatorApplication {left = leftExpr} ->
          case getNode leftExpr of
            DoBlock {doKind = Do} -> True
            DoBlock {doKind = Mdo} -> True
            _ -> False
        _ -> False
