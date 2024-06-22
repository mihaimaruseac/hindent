{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module HIndent.Ast.Expression
  ( Expression
  , mkExpression
  ) where

import Control.Monad
import Control.Monad.RWS
import Data.List.NonEmpty hiding (reverse)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified GHC.Data.FastString as GHC
import GHC.Stack
import qualified GHC.Types.Basic as GHC
import qualified GHC.Types.Fixity as GHC
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Unit as GHC
import HIndent.Ast.Expression.Bracket
import {-# SOURCE #-} HIndent.Ast.Expression.Record.Field
import HIndent.Ast.Expression.Record.Field.Label
import HIndent.Ast.Expression.Splice
import HIndent.Ast.Expression.Variable
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import HIndent.Fixity
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types hiding
  ( Case
  , Cases
  , Do
  , LambdaCase
  , ListComprehension
  , Mdo
  )
import qualified HIndent.Pretty.Types as Pretty
import HIndent.Printer
import qualified Language.Haskell.GhclibParserEx.GHC.Hs.Expr as GHC

data Expression
  = Variable (WithComments Variable)
  | Literal (GHC.HsLit GHC.GhcPs)
  | OverloadedLabel GHC.FastString
  | OverloadedLiteral (GHC.HsOverLit GHC.GhcPs)
  | ImplicitParameter GHC.HsIPName
  | Lambda (GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
  | LambdaCase (GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
  | LambdaCases (GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
  | Negation (WithComments Expression)
  | FunctionApplication
      { f :: WithComments Expression
      , args :: [WithComments Expression]
      }
  | OperatorApplication
      { lhs :: GHC.LHsExpr GHC.GhcPs
      , op :: GHC.LHsExpr GHC.GhcPs
      , rhs :: GHC.LHsExpr GHC.GhcPs
      }
  | TypeApplication
      { v :: GHC.LHsExpr GHC.GhcPs
      , ty :: GHC.LHsWcType GHC.GhcPs
      }
  | Parentheses (WithComments Expression)
  | SectionLeft
      { l :: GHC.LHsExpr GHC.GhcPs
      , o :: GHC.LHsExpr GHC.GhcPs
      }
  | SectionRight
      { o :: GHC.LHsExpr GHC.GhcPs
      , r :: GHC.LHsExpr GHC.GhcPs
      }
  | Tuple
      { elements :: [GHC.HsTupArg GHC.GhcPs]
      , boxity :: GHC.Boxity
      }
  | UnboxedSum
      { position :: Int
      , numElems :: Int
      , expr :: WithComments Expression
      }
  | Case
      { cond :: WithComments Expression
      , arms :: GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
      }
  | If
      { cond :: WithComments Expression
      , t :: WithComments Expression
      , f :: WithComments Expression
      }
  | MultiWayIf [GHC.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)]
  | LetIn
      { binds :: GHC.HsLocalBinds GHC.GhcPs
      , expr :: WithComments Expression
      }
  | List [WithComments Expression]
  | ListComprehension
      (GHC.GenLocated GHC.SrcSpanAnnL (NonEmpty (GHC.ExprLStmt GHC.GhcPs)))
  | Do
      { statements :: GHC.GenLocated GHC.SrcSpanAnnL [GHC.ExprLStmt GHC.GhcPs]
      , moduleName :: Maybe GHC.ModuleName
      , doType :: DoOrMdo
      }
  | RecordConstructor
      { name :: GHC.XRec GHC.GhcPs (GHC.ConLikeP GHC.GhcPs)
      , fields :: GHC.HsRecordBinds GHC.GhcPs
      }
  | RecordUpdate
      { base :: WithComments Expression
      , updaters :: [WithComments RecordField]
      }
  | GetField (WithComments Expression) (WithComments FieldLabel)
  | Projection (NonEmpty (WithComments FieldLabel))
  | WithSignature (WithComments Expression) (GHC.LHsSigWcType GHC.GhcPs)
  | Sequence (GHC.ArithSeqInfo GHC.GhcPs)
  | Splice Splice
  | Bracket Bracket
  | Static (WithComments Expression)
  | Expression (GHC.HsExpr GHC.GhcPs)

instance CommentExtraction Expression where
  nodeComments Variable {} = NodeComments [] [] []
  nodeComments Literal {} = NodeComments [] [] []
  nodeComments OverloadedLabel {} = NodeComments [] [] []
  nodeComments OverloadedLiteral {} = NodeComments [] [] []
  nodeComments ImplicitParameter {} = NodeComments [] [] []
  nodeComments Lambda {} = NodeComments [] [] []
  nodeComments LambdaCase {} = NodeComments [] [] []
  nodeComments LambdaCases {} = NodeComments [] [] []
  nodeComments Negation {} = NodeComments [] [] []
  nodeComments FunctionApplication {} = NodeComments [] [] []
  nodeComments OperatorApplication {} = NodeComments [] [] []
  nodeComments TypeApplication {} = NodeComments [] [] []
  nodeComments Parentheses {} = NodeComments [] [] []
  nodeComments SectionLeft {} = NodeComments [] [] []
  nodeComments SectionRight {} = NodeComments [] [] []
  nodeComments Tuple {} = NodeComments [] [] []
  nodeComments UnboxedSum {} = NodeComments [] [] []
  nodeComments Case {} = NodeComments [] [] []
  nodeComments If {} = NodeComments [] [] []
  nodeComments MultiWayIf {} = NodeComments [] [] []
  nodeComments LetIn {} = NodeComments [] [] []
  nodeComments List {} = NodeComments [] [] []
  nodeComments ListComprehension {} = NodeComments [] [] []
  nodeComments Do {} = NodeComments [] [] []
  nodeComments RecordConstructor {} = NodeComments [] [] []
  nodeComments RecordUpdate {} = NodeComments [] [] []
  nodeComments GetField {} = NodeComments [] [] []
  nodeComments Projection {} = NodeComments [] [] []
  nodeComments WithSignature {} = NodeComments [] [] []
  nodeComments Sequence {} = NodeComments [] [] []
  nodeComments Splice {} = NodeComments [] [] []
  nodeComments Bracket {} = NodeComments [] [] []
  nodeComments Static {} = NodeComments [] [] []
  nodeComments Expression {} = NodeComments [] [] []

instance Pretty Expression where
  pretty' (Variable x) = pretty x
  pretty' (Literal x) = pretty x
  pretty' (OverloadedLabel x) = string "#" >> string (GHC.unpackFS x)
  pretty' (OverloadedLiteral x) = pretty x
  pretty' (ImplicitParameter x) = string "?" >> pretty x
  pretty' (Lambda x) = pretty x
  pretty' (LambdaCase x) = pretty $ Pretty.LambdaCase x Pretty.Case
  pretty' (LambdaCases x) = pretty $ Pretty.LambdaCase x Pretty.Cases
  pretty' (Negation x) = string "-" >> pretty x
  pretty' FunctionApplication {..} = horizontal <-|> vertical
    where
      horizontal = spaced $ pretty f : fmap pretty args
      vertical = do
        col <- gets psColumn
        spaces <- getIndentSpaces
        pretty f
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
        indentedWithSpace spaces' $ lined $ fmap pretty args
  pretty' OperatorApplication {..} = horizontal <-|> vertical
    where
      horizontal =
        spaced
          [ pretty $ fmap mkExpression lhs
          , pretty (InfixExpr op)
          , pretty $ fmap mkExpression rhs
          ]
      vertical =
        case findFixity op of
          GHC.Fixity _ _ GHC.InfixL -> leftAssoc
          GHC.Fixity _ _ GHC.InfixR -> rightAssoc
          GHC.Fixity _ _ GHC.InfixN -> noAssoc
      leftAssoc = prettyOps allOperantsAndOperatorsLeftAssoc
      rightAssoc = prettyOps allOperantsAndOperatorsRightAssoc
      noAssoc
        | GHC.L _ (GHC.OpApp _ _ o _) <- lhs
        , isSameAssoc o = leftAssoc
        | otherwise = rightAssoc
      prettyOps [l, o, r@(GHC.L _ (GHC.HsDo {}))] = do
        spaced
          [ pretty $ fmap mkExpression l
          , pretty $ InfixExpr o
          , pretty $ fmap mkExpression r
          ]
      prettyOps [l, o, r@(GHC.L _ GHC.HsLam {})] = do
        spaced
          [ pretty $ fmap mkExpression l
          , pretty $ InfixExpr o
          , pretty $ fmap mkExpression r
          ]
      prettyOps [l, o, r@(GHC.L _ GHC.HsLamCase {})] = do
        spaced
          [ pretty $ fmap mkExpression l
          , pretty $ InfixExpr o
          , pretty $ fmap mkExpression r
          ]
      prettyOps (l:xs) = do
        pretty $ fmap mkExpression l
        newline
        indentedBlock $ f xs
        where
          f (o:r:rems) = do
            (pretty (InfixExpr o) >> space) |=> pretty (fmap mkExpression r)
            unless (null rems) $ do
              newline
              f rems
          f _ =
            error
              "The number of the sum of operants and operators should be odd."
      prettyOps _ = error "Too short list."
      findFixity o =
        fromMaybe GHC.defaultFixity $ lookup (GHC.varToStr o) fixities
      allOperantsAndOperatorsLeftAssoc = reverse $ rhs : op : collect lhs
        where
          collect :: GHC.LHsExpr GHC.GhcPs -> [GHC.LHsExpr GHC.GhcPs]
          collect (GHC.L _ (GHC.OpApp _ l o r))
            | isSameAssoc o = r : o : collect l
          collect x = [x]
      allOperantsAndOperatorsRightAssoc = lhs : op : collect rhs
        where
          collect :: GHC.LHsExpr GHC.GhcPs -> [GHC.LHsExpr GHC.GhcPs]
          collect (GHC.L _ (GHC.OpApp _ l o r))
            | isSameAssoc o = l : o : collect r
          collect x = [x]
      isSameAssoc (findFixity -> GHC.Fixity _ lv d) = lv == level && d == dir
      GHC.Fixity _ level dir = findFixity op
  pretty' TypeApplication {..} =
    pretty (fmap mkExpression v) >> string " @" >> pretty ty
  pretty' (Parentheses x) = parens $ pretty x
  pretty' SectionLeft {..} =
    spaced [pretty $ fmap mkExpression l, pretty $ InfixExpr o]
  pretty' SectionRight {..} =
    (pretty (InfixExpr o) >> space) |=> pretty (fmap mkExpression r)
  pretty' Tuple {..} = horizontal <-|> vertical
    where
      horizontal = parH $ fmap pretty elements
      vertical =
        parV
          $ prefixedLined ","
          $ fmap (\e -> unless (isMissing e) (space |=> pretty e)) elements
      isMissing GHC.Missing {} = True
      isMissing _ = False
      (parH, parV) =
        case boxity of
          GHC.Boxed -> (hTuple, parens)
          GHC.Unboxed -> (hUnboxedTuple, unboxedParens)
  pretty' UnboxedSum {..} = do
    string "(#"
    forM_ [1 .. numElems] $ \idx -> do
      if idx == position
        then space >> pretty expr >> space
        else space
      when (idx < numElems) $ string "|"
    string "#)"
  pretty' Case {..} = do
    string "case " |=> do
      pretty cond
      string " of"
    if null $ GHC.unLoc $ GHC.mg_alts arms
      then string " {}"
      else do
        newline
        indentedBlock $ pretty arms
  pretty' If {..} = do
    string "if " |=> pretty cond
    indentedBlock $ newlinePrefixed [branch "then " t, branch "else " f]
    where
      branch :: String -> WithComments Expression -> Printer ()
      branch str e =
        case getNode e of
          Do {..} -> doStmt (QualifiedDo moduleName doType) statements
          _ -> string str |=> pretty e
        where
          doStmt qDo stmts = do
            string str
            pretty qDo
            newline
            indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
  pretty' (MultiWayIf guards) =
    string "if "
      |=> lined (fmap (pretty . fmap (GRHSExpr GRHSExprMultiWayIf)) guards)
  pretty' LetIn {..} =
    lined [string "let " |=> pretty binds, string " in " |=> pretty expr]
  pretty' (List xs) = horizontal <-|> vertical
    where
      horizontal = brackets $ hCommaSep $ fmap pretty xs
      vertical = vList $ fmap pretty xs
  pretty' (ListComprehension (GHC.L l (lhs :| rhs))) =
    pretty $ GHC.L l $ Pretty.ListComprehension lhs rhs
  pretty' Do {statements = GHC.L l stmts, ..} =
    pretty $ GHC.L l $ DoExpression stmts $ QualifiedDo moduleName doType
  pretty' RecordConstructor {..} = horizontal <-|> vertical
    where
      horizontal = spaced [pretty name, pretty fields]
      vertical = pretty name >> newline >> indentedBlock (pretty fields)
  pretty' RecordUpdate {..} = hor <-|> ver
    where
      hor = spaced [pretty base, hFields $ fmap pretty updaters]
      ver = do
        pretty base
        newline
        indentedBlock
          $ hFields (fmap pretty updaters) <-|> vFields (fmap pretty updaters)
  pretty' (GetField e f) = pretty e >> string "." >> pretty f
  pretty' (Projection fields) = parens $ forM_ fields $ \x -> dot >> pretty x
  pretty' (WithSignature e ty) =
    spaced [pretty e, string "::", pretty $ GHC.hswc_body ty]
  pretty' (Sequence x) = pretty x
  pretty' (Splice x) = pretty x
  pretty' (Bracket x) = pretty x
  pretty' (Static x) = string "static " >> pretty x
  pretty' (Expression x) = prettyHsExpr x

prettyHsExpr :: GHC.HsExpr GHC.GhcPs -> Printer ()
prettyHsExpr (GHC.HsProc _ pat x@(GHC.L _ (GHC.HsCmdTop _ (GHC.L _ (GHC.HsCmdDo _ xs))))) = do
  spaced [string "proc", pretty pat, string "-> do"]
  newline
  indentedBlock
    $ printCommentsAnd x (const (printCommentsAnd xs (lined . fmap pretty)))
prettyHsExpr (GHC.HsProc _ pat body) = hor <-|> ver
  where
    hor = spaced [string "proc", pretty pat, string "->", pretty body]
    ver = do
      spaced [string "proc", pretty pat, string "->"]
      newline
      indentedBlock (pretty body)
prettyHsExpr (GHC.HsPragE _ p x) =
  spaced [pretty p, pretty $ fmap mkExpression x]
prettyHsExpr _ = undefined

mkExpression :: GHC.HsExpr GHC.GhcPs -> Expression
mkExpression (GHC.HsVar _ x) = Variable $ mkVariable <$> fromGenLocated x
mkExpression (GHC.HsUnboundVar _ x) = Variable $ mkWithComments $ mkVariable x
mkExpression (GHC.HsLit _ x) = Literal x
mkExpression (GHC.HsOverLit _ x) = OverloadedLiteral x
mkExpression (GHC.HsIPVar _ x) = ImplicitParameter x
mkExpression (GHC.HsLam _ x) = Lambda x
mkExpression (GHC.NegApp _ x _) =
  Negation $ fromGenLocated $ fmap mkExpression x
mkExpression (GHC.HsApp _ l r) = FunctionApplication {..}
  where
    f :| args =
      fromGenLocated . fmap mkExpression
        <$> NE.reverse (r :| reverse (flatten l))
    flatten :: GHC.LHsExpr GHC.GhcPs -> [GHC.LHsExpr GHC.GhcPs]
    flatten (GHC.L (GHC.SrcSpanAnn (GHC.EpAnn _ _ cs) _) (GHC.HsApp _ l' r')) =
      flatten l' ++ [insertComments cs r']
    flatten x = [x]
    insertComments cs (GHC.L s@GHC.SrcSpanAnn {GHC.ann = e@GHC.EpAnn {comments = cs'}} r') =
      GHC.L (s {GHC.ann = e {GHC.comments = cs <> cs'}}) r'
    insertComments _ x = x
mkExpression (GHC.OpApp _ lhs op rhs) = OperatorApplication {..}
mkExpression (GHC.SectionL _ l o) = SectionLeft {..}
mkExpression (GHC.SectionR _ o r) = SectionRight {..}
mkExpression (GHC.ExplicitTuple _ elements boxity) = Tuple {..}
mkExpression (GHC.ExplicitSum _ position numElems e) = UnboxedSum {..}
  where
    expr = fromGenLocated $ fmap mkExpression e
mkExpression (GHC.HsCase _ c arms) = Case {..}
  where
    cond = fromGenLocated $ fmap mkExpression c
mkExpression (GHC.HsIf _ c t' f') = If {..}
  where
    cond = fromGenLocated $ fmap mkExpression c
    t = fromGenLocated $ fmap mkExpression t'
    f = fromGenLocated $ fmap mkExpression f'
mkExpression (GHC.HsMultiIf _ guards) = MultiWayIf guards
mkExpression (GHC.ExplicitList _ xs) = List xs'
  where
    xs' = fmap (fromGenLocated . fmap mkExpression) xs
mkExpression (GHC.HsDo _ GHC.ListComp {} (GHC.L _ [])) =
  error "Not enough arguments are passed to create a list comprehension."
mkExpression (GHC.HsDo _ GHC.ListComp {} (GHC.L l (lhs:rhs))) =
  ListComprehension $ GHC.L l $ lhs :| rhs
mkExpression (GHC.HsDo _ GHC.MonadComp {} (GHC.L _ [])) =
  error "Not enough arguments are passed to create a list comprehension."
mkExpression (GHC.HsDo _ GHC.MonadComp {} (GHC.L l (lhs:rhs))) =
  ListComprehension $ GHC.L l $ lhs :| rhs
mkExpression (GHC.HsDo _ (GHC.DoExpr moduleName) statements) =
  Do {doType = Pretty.Do, ..}
mkExpression (GHC.HsDo _ (GHC.MDoExpr moduleName) statements) =
  Do {doType = Pretty.Mdo, ..}
mkExpression (GHC.HsDo _ GHC.GhciStmtCtxt {} _) =
  error "We're not using GHCi, are we?"
mkExpression (GHC.RecordCon _ name fields) = RecordConstructor {..}
mkExpression (GHC.HsGetField _ e f) =
  GetField
    (fromGenLocated $ fmap mkExpression e)
    (fromGenLocated $ fmap mkFieldLabel f)
mkExpression (GHC.HsProjection _ fields) =
  Projection $ fmap (fmap mkFieldLabel . fromGenLocated) fields
mkExpression (GHC.ExprWithTySig _ e ty) =
  WithSignature (fromGenLocated $ fmap mkExpression e) ty
mkExpression (GHC.ArithSeq _ _ x) = Sequence x
#if MIN_VERSION_ghc_lib_parser(9, 8, 0)
mkExpression (GHC.RecordUpd _ b GHC.RegularRecUpdFields {..}) =
  RecordUpdate {..}
  where
    base = fromGenLocated $ fmap mkExpression b
    updaters = fmap (fmap mkRecordField . fromGenLocated) recUpdFields
mkExpression (GHC.RecordUpd _ b GHC.OverloadedRecUpdFields {..}) =
  RecordUpdate {..}
  where
    base = fromGenLocated $ fmap mkExpression b
    updaters = fmap (fmap mkRecordField . fromGenLocated) olRecUpdFields
#elif MIN_VERSION_ghc_lib_parser(9, 4, 0)
mkExpression (GHC.RecordUpd _ b us) = RecordUpdate {..}
  where
    base = fromGenLocated $ fmap mkExpression b
    updaters =
      either
        (fmap (fmap mkRecordField . fromGenLocated))
        (fmap (fmap mkRecordField . fromGenLocated))
        us
#else
mkExpression (GHC.RecordUpd _ b us) = RecordUpdate {..}
  where
    base = fromGenLocated $ fmap mkExpression b
    updaters =
      either
        (fmap (fmap mkRecordField . fromGenLocated))
        (fmap (fmap mkRecordField . fromGenLocated))
        us
#endif
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
mkExpression (GHC.HsTypedSplice _ x) =
  Splice $ mkSplice $ fromGenLocated $ fmap mkExpression x
mkExpression (GHC.HsUntypedSplice _ x) = Splice $ mkSplice x
mkExpression (GHC.HsOverLabel _ _ x) = OverloadedLabel x
mkExpression (GHC.HsAppType _ v _ ty) = TypeApplication {..}
#else
mkExpression (GHC.HsSpliceE _ x) = Splice $ mkSplice x
mkExpression (GHC.HsOverLabel _ x) = OverloadedLabel x
mkExpression (GHC.HsAppType _ v ty) = TypeApplication {..}
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 0)
mkExpression (GHC.HsTypedBracket _ inner) =
  Bracket $ mkBracket $ fromGenLocated $ fmap mkExpression inner
mkExpression (GHC.HsUntypedBracket _ inner) = Bracket $ mkBracket inner
mkExpression (GHC.HsLamCase _ GHC.LamCase matches) = LambdaCase matches
mkExpression (GHC.HsLamCase _ GHC.LamCases matches) = LambdaCases matches
mkExpression (GHC.HsPar _ _ e _) = Parentheses expr
  where
    expr = fromGenLocated $ fmap mkExpression e
mkExpression (GHC.HsLet _ _ binds _ e) = LetIn {..}
  where
    expr = fromGenLocated $ fmap mkExpression e
mkExpression GHC.HsRecSel {} = notGeneratedByParser
#else
mkExpression (GHC.HsBracket _ inner) = Bracket $ mkBracket inner
mkExpression (GHC.HsLamCase _ x) = LambdaCase x
mkExpression (GHC.HsPar _ e) = Parentheses expr
  where
    expr = fromGenLocated $ fmap mkExpression e
mkExpression (GHC.HsLet _ binds e) = LetIn {..}
  where
    expr = fromGenLocated $ fmap mkExpression e
mkExpression (GHC.HsDo _ GHC.ArrowExpr {} _) = notGeneratedByParser
mkExpression (GHC.HsDo _ GHC.PatGuard {} _) = notGeneratedByParser
mkExpression (GHC.HsDo _ GHC.ParStmtCtxt {} _) = notGeneratedByParser
mkExpression (GHC.HsDo _ GHC.TransStmtCtxt {} _) = notGeneratedByParser
mkExpression GHC.HsRnBracketOut {} = notGeneratedByParser
mkExpression GHC.HsTcBracketOut {} = notGeneratedByParser
mkExpression GHC.HsConLikeOut {} = notGeneratedByParser
mkExpression GHC.HsRecFld {} = notGeneratedByParser
mkExpression GHC.HsTick {} = forHpc
mkExpression GHC.HsBinTick {} = forHpc
#endif
mkExpression (GHC.HsStatic _ x) = Static $ fromGenLocated $ fmap mkExpression x
mkExpression x = Expression x

notGeneratedByParser :: HasCallStack => a
notGeneratedByParser = error "`ghc-lib-parser` never generates this AST node."
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
-- | Marks an AST node as it is used only for Haskell Program Coverage.
forHpc :: HasCallStack => a
forHpc = error "This AST type is for the use of Haskell Program Coverage."
#endif
