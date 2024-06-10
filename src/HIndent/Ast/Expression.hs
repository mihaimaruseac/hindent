{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression
  ( Expression
  , mkExpression
  ) where

import Control.Monad.RWS
import Data.List.NonEmpty
import qualified GHC.Data.FastString as GHC
import GHC.Stack
import qualified GHC.Types.Basic as GHC
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Unit as GHC
import HIndent.Ast.Expression.Bracket
import {-# SOURCE #-} HIndent.Ast.Expression.Record.Field
import HIndent.Ast.Expression.Record.Field.Label
import HIndent.Ast.Expression.Splice
import HIndent.Ast.Expression.Variable
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import HIndent.GhcLibParserWrapper.GHC.Hs
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types hiding
  ( Case
  , Cases
  , Do
  , LambdaCase
  , LetIn
  , ListComprehension
  , Mdo
  )
import qualified HIndent.Pretty.Types as Pretty
import HIndent.Printer

data Expression
  = Variable (WithComments Variable)
  | Literal (GHC.HsLit GHC.GhcPs)
  | OverloadedLabel GHC.FastString
  | OverloadedLiteral (GHC.HsOverLit GHC.GhcPs)
  | ImplicitParameter GHC.HsIPName
  | Lambda (GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
  | LambdaCase (GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
  | LambdaCases (GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
  | Negation (GHC.LHsExpr GHC.GhcPs)
  | Application
      { l :: GHC.LHsExpr GHC.GhcPs
      , r :: GHC.LHsExpr GHC.GhcPs
      }
  | OperatorApplication
      { l :: GHC.LHsExpr GHC.GhcPs
      , o :: GHC.LHsExpr GHC.GhcPs
      , r :: GHC.LHsExpr GHC.GhcPs
      }
  | TypeApplication
      { v :: GHC.LHsExpr GHC.GhcPs
      , ty :: GHC.LHsWcType GHC.GhcPs
      }
  | Parentheses (GHC.LHsExpr GHC.GhcPs)
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
      , expr :: GHC.LHsExpr GHC.GhcPs
      }
  | Case
      { cond :: GHC.LHsExpr GHC.GhcPs
      , arms :: GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
      }
  | If
      { cond :: GHC.LHsExpr GHC.GhcPs
      , t :: GHC.LHsExpr GHC.GhcPs
      , f :: GHC.LHsExpr GHC.GhcPs
      }
  | MultiWayIf [GHC.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)]
  | LetIn
      { binds :: GHC.HsLocalBinds GHC.GhcPs
      , expr :: GHC.LHsExpr GHC.GhcPs
      }
  | List [GHC.LHsExpr GHC.GhcPs]
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
      { base :: GHC.LHsExpr GHC.GhcPs
      , updaters :: [WithComments RecordField]
      }
  | GetField (GHC.LHsExpr GHC.GhcPs) (WithComments FieldLabel)
  | Projection (NonEmpty (WithComments FieldLabel))
  | WithSignature (GHC.LHsExpr GHC.GhcPs) (GHC.LHsSigWcType GHC.GhcPs)
  | Sequence (GHC.ArithSeqInfo GHC.GhcPs)
  | Splice Splice
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
  nodeComments Application {} = NodeComments [] [] []
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
  pretty' (Negation x) = string "-" >> pretty (fmap mkExpression x)
  pretty' Application {..} = horizontal <-|> vertical
    where
      horizontal =
        spaced [pretty $ fmap mkExpression l, pretty $ fmap mkExpression r]
      vertical = do
        let (f, args) =
              case flatten l ++ [r] of
                [] -> error "Invalid function application."
                (f':args') -> (f', args')
        col <- gets psColumn
        spaces <- getIndentSpaces
        pretty $ fmap mkExpression f
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
        indentedWithSpace spaces'
          $ lined
          $ fmap (pretty . fmap mkExpression) args
      flatten :: GHC.LHsExpr GHC.GhcPs -> [GHC.LHsExpr GHC.GhcPs]
      flatten (GHC.L (GHC.SrcSpanAnn (GHC.EpAnn _ _ cs) _) (GHC.HsApp _ l' r')) =
        flatten l' ++ [insertComments cs r']
      flatten x = [x]
      insertComments ::
           GHC.EpAnnComments -> GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
      insertComments cs (GHC.L s@GHC.SrcSpanAnn {GHC.ann = e@GHC.EpAnn {comments = cs'}} r') =
        GHC.L (s {GHC.ann = e {GHC.comments = cs <> cs'}}) r'
      insertComments _ x = x
  pretty' OperatorApplication {..} = pretty $ InfixApp l o r
  pretty' TypeApplication {..} =
    pretty (fmap mkExpression v) >> string " @" >> pretty ty
  pretty' (Parentheses x) = parens $ pretty $ fmap mkExpression x
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
        then string " " >> pretty (fmap mkExpression expr) >> string " "
        else string " "
      when (idx < numElems) $ string "|"
    string "#)"
  pretty' Case {..} = do
    string "case " |=> do
      pretty $ fmap mkExpression cond
      string " of"
    if null $ GHC.unLoc $ GHC.mg_alts arms
      then string " {}"
      else do
        newline
        indentedBlock $ pretty arms
  pretty' If {..} = do
    string "if " |=> pretty (fmap mkExpression cond)
    indentedBlock $ newlinePrefixed [branch "then " t, branch "else " f]
    where
      branch :: String -> GHC.LHsExpr GHC.GhcPs -> Printer ()
      branch str e =
        case e of
          (GHC.L _ (GHC.HsDo _ (GHC.DoExpr m) xs)) ->
            doStmt (QualifiedDo m Pretty.Do) xs
          (GHC.L _ (GHC.HsDo _ (GHC.MDoExpr m) xs)) ->
            doStmt (QualifiedDo m Pretty.Mdo) xs
          _ -> string str |=> pretty (fmap mkExpression e)
        where
          doStmt qDo stmts = do
            string str
            pretty qDo
            newline
            indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
  pretty' (MultiWayIf guards) =
    string "if "
      |=> lined (fmap (pretty . fmap (GRHSExpr GRHSExprMultiWayIf)) guards)
  pretty' LetIn {..} = pretty $ Pretty.LetIn binds expr
  pretty' (List xs) = horizontal <-|> vertical
    where
      horizontal = brackets $ hCommaSep $ fmap (pretty . fmap mkExpression) xs
      vertical = vList $ fmap (pretty . fmap mkExpression) xs
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
      hor =
        spaced [pretty $ fmap mkExpression base, hFields $ fmap pretty updaters]
      ver = do
        pretty $ fmap mkExpression base
        newline
        indentedBlock
          $ (hFields $ fmap pretty updaters)
              <-|> (vFields $ fmap pretty updaters)
  pretty' (GetField e f) =
    pretty (fmap mkExpression e) >> string "." >> pretty f
  pretty' (Projection fields) = parens $ forM_ fields $ \x -> dot >> pretty x
  pretty' (WithSignature e ty) =
    spaced
      [pretty $ fmap mkExpression e, string "::", pretty $ GHC.hswc_body ty]
  pretty' (Sequence x) = pretty x
  pretty' (Splice x) = pretty x
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
prettyHsExpr (GHC.HsStatic _ x) =
  spaced [string "static", pretty $ fmap mkExpression x]
prettyHsExpr (GHC.HsPragE _ p x) =
  spaced [pretty p, pretty $ fmap mkExpression x]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr GHC.HsRecSel {} = notGeneratedByParser
prettyHsExpr (GHC.HsTypedBracket _ inner) =
  typedBrackets $ pretty $ fmap mkExpression inner
prettyHsExpr (GHC.HsUntypedBracket _ inner) = pretty $ fmap mkExpression inner
#else
prettyHsExpr GHC.HsConLikeOut {} = notGeneratedByParser
prettyHsExpr GHC.HsRecFld {} = notGeneratedByParser
prettyHsExpr (GHC.HsDo _ GHC.ArrowExpr {} _) = notGeneratedByParser
prettyHsExpr (GHC.HsDo _ GHC.PatGuard {} _) = notGeneratedByParser
prettyHsExpr (GHC.HsDo _ GHC.ParStmtCtxt {} _) = notGeneratedByParser
prettyHsExpr (GHC.HsDo _ GHC.TransStmtCtxt {} _) = notGeneratedByParser
prettyHsExpr GHC.HsTick {} = forHpc
prettyHsExpr GHC.HsBinTick {} = forHpc
prettyHsExpr (GHC.HsBracket _ inner) = pretty $ mkBracket inner
prettyHsExpr GHC.HsRnBracketOut {} = notGeneratedByParser
prettyHsExpr GHC.HsTcBracketOut {} = notGeneratedByParser
#endif
#if MIN_VERSION_ghc_lib_parser(9,6,1)
prettyHsExpr (GHC.HsTypedSplice _ x) = string "$$" >> pretty x
prettyHsExpr (GHC.HsUntypedSplice _ x) = pretty $ mkSplice x
#endif
prettyHsExpr _ = undefined

mkExpression :: GHC.HsExpr GHC.GhcPs -> Expression
mkExpression (GHC.HsVar _ x) = Variable $ fmap mkVariable $ fromGenLocated x
mkExpression (GHC.HsUnboundVar _ x) =
  Variable $ fmap mkVariable $ mkWithComments x
mkExpression (GHC.HsLit _ x) = Literal x
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkExpression (GHC.HsOverLabel _ _ x) = OverloadedLabel x
#else
mkExpression (GHC.HsOverLabel _ x) = OverloadedLabel x
#endif
mkExpression (GHC.HsOverLit _ x) = OverloadedLiteral x
mkExpression (GHC.HsIPVar _ x) = ImplicitParameter x
mkExpression (GHC.HsLam _ x) = Lambda x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
mkExpression (GHC.HsLamCase _ GHC.LamCase matches) = LambdaCase matches
mkExpression (GHC.HsLamCase _ GHC.LamCases matches) = LambdaCases matches
#else
mkExpression (GHC.HsLamCase _ x) = LambdaCase x
#endif
mkExpression (GHC.NegApp _ x _) = Negation x
mkExpression (GHC.HsApp _ l r) = Application {..}
mkExpression (GHC.OpApp _ l o r) = OperatorApplication {..}
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkExpression (GHC.HsAppType _ v _ ty) = TypeApplication {..}
#else
mkExpression (GHC.HsAppType _ v ty) = TypeApplication {..}
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkExpression (GHC.HsPar _ _ expr _) = Parentheses expr
#else
mkExpression (GHC.HsPar _ expr) = Parentheses expr
#endif
mkExpression (GHC.SectionL _ l o) = SectionLeft {..}
mkExpression (GHC.SectionR _ o r) = SectionRight {..}
mkExpression (GHC.ExplicitTuple _ elements boxity) = Tuple {..}
mkExpression (GHC.ExplicitSum _ position numElems expr) = UnboxedSum {..}
mkExpression (GHC.HsCase _ cond arms) = Case {..}
mkExpression (GHC.HsIf _ cond t f) = If {..}
mkExpression (GHC.HsMultiIf _ guards) = MultiWayIf guards
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkExpression (GHC.HsLet _ _ binds _ expr) = LetIn {..}
#else
mkExpression (GHC.HsLet _ binds expr) = LetIn {..}
#endif
mkExpression (GHC.ExplicitList _ xs) = List xs
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
mkExpression (GHC.RecordUpd _ base us) = RecordUpdate {..}
  where
    updaters =
      either
        (fmap (fmap mkRecordField . fromGenLocated))
        (fmap (fmap mkRecordField . fromGenLocated))
        us
mkExpression (GHC.HsGetField _ e f) =
  GetField e $ fmap mkFieldLabel $ fromGenLocated f
mkExpression (GHC.HsProjection _ fields) =
  Projection $ fmap (fmap mkFieldLabel . fromGenLocated) fields
mkExpression (GHC.ExprWithTySig _ e ty) = WithSignature e ty
mkExpression (GHC.ArithSeq _ _ x) = Sequence x
#if !MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkExpression (GHC.HsSpliceE _ x) = Splice $ mkSplice x
#endif
mkExpression x = Expression x

notGeneratedByParser :: HasCallStack => a
notGeneratedByParser = error "`ghc-lib-parser` never generates this AST node."
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
-- | Marks an AST node as it is used only for Haskell Program Coverage.
forHpc :: HasCallStack => a
forHpc = error "This AST type is for the use of Haskell Program Coverage."
#endif
