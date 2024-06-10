{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Pretty printing.
--
-- Some instances define top-level functions to handle CPP.
--
-- Some value constructors never appear in an AST. GHC has three stages for
-- using an AST: parsing, renaming, and type checking, and GHC uses these
-- constructors only in remaining and type checking.
module HIndent.Pretty
  ( Pretty(..)
  , pretty
  , printCommentsAnd
  ) where

import Control.Monad
import Control.Monad.RWS
import Data.Maybe
import Data.Void
import qualified GHC.Data.Bag as GHC
import qualified GHC.Data.FastString as GHC
import qualified GHC.Hs as GHC
import GHC.Stack
import qualified GHC.Types.Basic as GHC
import qualified GHC.Types.Fixity as GHC
import qualified GHC.Types.Name as GHC
import qualified GHC.Types.Name.Reader as GHC
import qualified GHC.Types.SourceText as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Data.Body
import HIndent.Ast.Declaration.Data.Record.Field
import HIndent.Ast.Declaration.Family.Type
import HIndent.Ast.Declaration.Signature
import HIndent.Ast.Expression
import HIndent.Ast.Expression.Splice
import HIndent.Ast.NodeComments
import HIndent.Ast.Operator.Infix
import HIndent.Ast.Operator.Prefix
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import HIndent.Config
import HIndent.Fixity
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import qualified HIndent.Pretty.SigBindFamily as SBF
import HIndent.Pretty.Types
import HIndent.Printer
import qualified Language.Haskell.GhclibParserEx.GHC.Hs.Expr as GHC
import Text.Show.Unicode
#if MIN_VERSION_ghc_lib_parser(9,6,1)
import qualified Data.Foldable as NonEmpty
import qualified GHC.Core.DataCon as GHC
#endif
#if !MIN_VERSION_ghc_lib_parser(9,6,1)
import qualified GHC.Unit as GHC
#endif
-- | This function pretty-prints the given AST node with comments.
pretty :: Pretty a => a -> Printer ()
pretty p = do
  printCommentsBefore p
  pretty' p
  printCommentOnSameLine p
  printCommentsAfter p

-- | Prints comments included in the location information and then the
-- AST node body.
printCommentsAnd ::
     (CommentExtraction l)
  => GHC.GenLocated l e
  -> (e -> Printer ())
  -> Printer ()
printCommentsAnd (GHC.L l e) f = do
  printCommentsBefore l
  f e
  printCommentOnSameLine l
  printCommentsAfter l

-- | Prints comments that are before the given AST node.
printCommentsBefore :: CommentExtraction a => a -> Printer ()
printCommentsBefore p =
  forM_ (commentsBefore $ nodeComments p) $ \(GHC.L loc c) -> do
    let col = fromIntegral $ GHC.srcSpanStartCol (GHC.anchor loc) - 1
    indentedWithFixedLevel col $ pretty c
    newline

-- | Prints comments that are on the same line as the given AST node.
printCommentOnSameLine :: CommentExtraction a => a -> Printer ()
printCommentOnSameLine (commentsOnSameLine . nodeComments -> (c:cs)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithFixedLevel
           (fromIntegral $ GHC.srcSpanStartCol $ GHC.anchor $ GHC.getLoc c)
           $ spaced
           $ fmap pretty
           $ c : cs
    else spacePrefixed $ fmap pretty $ c : cs
  eolCommentsArePrinted
printCommentOnSameLine _ = return ()

-- | Prints comments that are after the given AST node.
printCommentsAfter :: CommentExtraction a => a -> Printer ()
printCommentsAfter p =
  case commentsAfter $ nodeComments p of
    [] -> return ()
    xs -> do
      isThereCommentsOnSameLine <- gets psEolComment
      unless isThereCommentsOnSameLine newline
      forM_ xs $ \(GHC.L loc c) -> do
        let col = fromIntegral $ GHC.srcSpanStartCol (GHC.anchor loc) - 1
        indentedWithFixedLevel col $ pretty c
        eolCommentsArePrinted

-- | Pretty print including comments.
--
-- 'FastString' does not implement this class because it may contain @\n@s
-- and each type that may contain a 'FastString' value needs their own
-- handlings.
class CommentExtraction a =>
      Pretty a
  where
  pretty' :: a -> Printer ()

-- Do nothing if there are no pragmas, module headers, imports, or
-- declarations. Otherwise, extra blank lines will be inserted if only
-- comments are present in the source code. See
-- https://github.com/mihaimaruseac/hindent/issues/586#issuecomment-1374992624.
instance (CommentExtraction l, Pretty e) => Pretty (GHC.GenLocated l e) where
  pretty' (GHC.L _ e) = pretty e

instance Pretty
           (GHC.MatchGroup
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' GHC.MG {..} = printCommentsAnd mg_alts (lined . fmap pretty)

instance Pretty
           (GHC.MatchGroup
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs))) where
  pretty' GHC.MG {..} = printCommentsAnd mg_alts (lined . fmap pretty)

instance Pretty LambdaCase where
  pretty' (LambdaCase matches caseOrCases) = do
    case caseOrCases of
      Case -> string "\\case"
      Cases -> string "\\cases"
    if null $ GHC.unLoc $ GHC.mg_alts matches
      then string " {}"
      else do
        newline
        indentedBlock $ pretty matches

instance Pretty (GHC.HsSigType GHC.GhcPs) where
  pretty' = pretty' . HsSigType' HsTypeForNormalDecl HsTypeNoDir

instance Pretty HsSigType' where
  pretty' (HsSigTypeInsideDeclSig GHC.HsSig {..}) =
    case sig_bndrs of
      GHC.HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap (pretty . fmap mkTypeVariable . fromGenLocated) xs
        dot
        case GHC.unLoc sig_body of
          GHC.HsQualTy {..} ->
            printCommentsAnd sig_body $ \_ ->
              let hor = do
                    space
                    pretty $ HorizontalContext hst_ctxt
                  ver = do
                    newline
                    pretty $ VerticalContext hst_ctxt
               in do
                    hor <-|> ver
                    newline
                    prefixed "=> "
                      $ prefixedLined "-> "
                      $ pretty <$> flatten hst_body
          _ ->
            let hor = space >> pretty (fmap HsTypeInsideDeclSig sig_body)
                ver =
                  newline >> prefixedLined "-> " (pretty <$> flatten sig_body)
             in hor <-|> ver
      _ -> pretty $ fmap HsTypeInsideDeclSig sig_body
    where
      flatten :: GHC.LHsType GHC.GhcPs -> [GHC.LHsType GHC.GhcPs]
      flatten (GHC.L _ (GHC.HsFunTy _ _ l r)) = flatten l ++ flatten r
      flatten x = [x]
  pretty' (HsSigTypeInsideVerticalFuncSig GHC.HsSig {..}) =
    case sig_bndrs of
      GHC.HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap (pretty . fmap mkTypeVariable . fromGenLocated) xs
        dot
        printCommentsAnd sig_body $ \case
          GHC.HsQualTy {..} -> do
            (space >> pretty (HorizontalContext hst_ctxt))
              <-|> (newline >> pretty (VerticalContext hst_ctxt))
            newline
            prefixed "=> " $ pretty hst_body
          x -> pretty $ HsTypeInsideDeclSig x
      _ -> pretty $ fmap HsTypeInsideDeclSig sig_body
  pretty' (HsSigType' for dir GHC.HsSig {..}) = do
    case sig_bndrs of
      GHC.HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap (pretty . fmap mkTypeVariable . fromGenLocated) xs
        dot
        space
      _ -> return ()
    pretty $ HsType' for dir <$> sig_body

instance Pretty
           (GHC.Match
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' = prettyMatchExpr

prettyMatchExpr :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Printer ()
prettyMatchExpr GHC.Match {m_ctxt = GHC.LambdaExpr, ..} = do
  string "\\"
  unless (null m_pats)
    $ case GHC.unLoc $ head m_pats of
        GHC.LazyPat {} -> space
        GHC.BangPat {} -> space
        _ -> return ()
  spaced $ fmap pretty m_pats
  pretty $ GRHSsExpr GRHSExprLambda m_grhss
prettyMatchExpr GHC.Match {m_ctxt = GHC.CaseAlt, ..} = do
  mapM_ pretty m_pats
  pretty $ GRHSsExpr GRHSExprCase m_grhss
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyMatchExpr GHC.Match {m_ctxt = GHC.LamCaseAlt {}, ..} = do
  spaced $ fmap pretty m_pats
  pretty $ GRHSsExpr GRHSExprCase m_grhss
#endif
prettyMatchExpr GHC.Match {..} =
  case GHC.mc_fixity m_ctxt of
    GHC.Prefix -> do
      pretty m_ctxt
      spacePrefixed $ fmap pretty m_pats
      pretty m_grhss
    GHC.Infix -> do
      case (m_pats, m_ctxt) of
        (l:r:xs, GHC.FunRhs {..}) -> do
          spaced
            $ [pretty l, pretty $ fmap mkInfixOperator mc_fun, pretty r]
                ++ fmap pretty xs
          pretty m_grhss
        _ -> error "Not enough parameters are passed."

instance Pretty
           (GHC.Match
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs))) where
  pretty' = prettyMatchProc

prettyMatchProc :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Printer ()
prettyMatchProc GHC.Match {m_ctxt = GHC.LambdaExpr, ..} = do
  string "\\"
  unless (null m_pats)
    $ case GHC.unLoc $ head m_pats of
        GHC.LazyPat {} -> space
        GHC.BangPat {} -> space
        _ -> return ()
  spaced $ fmap pretty m_pats ++ [pretty m_grhss]
prettyMatchProc GHC.Match {m_ctxt = GHC.CaseAlt, ..} =
  spaced [mapM_ pretty m_pats, pretty m_grhss]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyMatchProc GHC.Match {m_ctxt = GHC.LamCaseAlt {}, ..} = do
  spaced [mapM_ pretty m_pats, pretty m_grhss]
#endif
prettyMatchProc _ = notGeneratedByParser

instance Pretty
           (GHC.StmtLR
              GHC.GhcPs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' (GHC.LastStmt _ x _ _) = pretty $ fmap mkExpression x
  pretty' (GHC.BindStmt _ pat body) = do
    pretty pat
    string " <-"
    hor <-|> ver
    where
      hor = space >> pretty (fmap mkExpression body)
      ver = newline >> indentedBlock (pretty $ fmap mkExpression body)
  pretty' GHC.ApplicativeStmt {} = notGeneratedByParser
  pretty' (GHC.BodyStmt _ (GHC.L loc (GHC.OpApp _ l o r)) _ _) =
    pretty (GHC.L loc (InfixApp l o r))
  pretty' (GHC.BodyStmt _ body _ _) = pretty $ fmap mkExpression body
  pretty' (GHC.LetStmt _ l) = string "let " |=> pretty l
  pretty' (GHC.ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
  pretty' GHC.TransStmt {..} =
    vCommaSep
      $ fmap pretty trS_stmts
          ++ [string "then " >> pretty (fmap mkExpression trS_using)]
  pretty' GHC.RecStmt {..} =
    string "rec " |=> printCommentsAnd recS_stmts (lined . fmap pretty)

instance Pretty
           (GHC.StmtLR
              GHC.GhcPs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs))) where
  pretty' (GHC.LastStmt _ x _ _) = pretty x
  pretty' (GHC.BindStmt _ pat body) = hor <-|> ver
    where
      hor = spaced [pretty pat, string "<-", pretty body]
      ver = do
        pretty pat
        string " <-"
        newline
        indentedBlock $ pretty body
  pretty' GHC.ApplicativeStmt {} = notGeneratedByParser
  pretty' (GHC.BodyStmt _ body _ _) = pretty body
  pretty' (GHC.LetStmt _ l) = string "let " |=> pretty l
  pretty' (GHC.ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
  pretty' GHC.TransStmt {..} =
    vCommaSep
      $ fmap pretty trS_stmts
          ++ [string "then " >> pretty (fmap mkExpression trS_using)]
  pretty' GHC.RecStmt {..} =
    string "rec " |=> printCommentsAnd recS_stmts (lined . fmap pretty)

instance Pretty StmtLRInsideVerticalList where
  pretty' (StmtLRInsideVerticalList (GHC.ParStmt _ xs _ _)) =
    vBarSep $ fmap (pretty . ParStmtBlockInsideVerticalList) xs
  pretty' (StmtLRInsideVerticalList x) = pretty x

-- | For pattern matching.
instance Pretty
           (GHC.HsRecFields
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.Pat GHC.GhcPs))) where
  pretty' GHC.HsRecFields {..} = horizontal <-|> vertical
    where
      horizontal =
        case rec_dotdot of
          Just _ -> braces $ string ".."
          Nothing -> hFields $ fmap pretty rec_flds
      vertical = vFields $ fmap pretty rec_flds

-- | For record updates
instance Pretty
           (GHC.HsRecFields
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' GHC.HsRecFields {..} = hvFields fieldPrinters
    where
      fieldPrinters =
        fmap pretty rec_flds
          ++ maybeToList (fmap (const (string "..")) rec_dotdot)

instance Pretty (GHC.HsType GHC.GhcPs) where
  pretty' = pretty' . HsType' HsTypeForNormalDecl HsTypeNoDir

instance Pretty HsType' where
  pretty' (HsTypeInsideVerticalFuncSig (GHC.HsFunTy _ _ a b)) = do
    pretty $ HsTypeInsideVerticalFuncSig <$> a
    newline
    prefixed "-> " $ pretty $ HsTypeInsideVerticalFuncSig <$> b
  pretty' (HsTypeInsideDeclSig GHC.HsQualTy {..}) = hor <-|> ver
    where
      hor = spaced [pretty $ Context hst_ctxt, string "=>", pretty hst_body]
      ver = do
        pretty $ Context hst_ctxt
        newline
        prefixed "=> " $ pretty $ fmap HsTypeInsideVerticalFuncSig hst_body
  pretty' (HsTypeInsideDeclSig (GHC.HsFunTy _ _ a b)) = hor <-|> ver
    where
      hor = spaced [pretty a, string "->", pretty b]
      ver = do
        pretty $ fmap HsTypeInsideVerticalFuncSig a
        newline
        prefixed "-> " $ pretty $ fmap HsTypeInsideVerticalFuncSig b
  pretty' (HsTypeInsideInstDecl GHC.HsQualTy {..}) = hor <-|> ver
    where
      hor = spaced [pretty (Context hst_ctxt), string "=>", pretty hst_body]
      ver = do
        pretty (Context hst_ctxt)
        string " =>"
        newline
        pretty hst_body
  pretty' (HsTypeWithVerticalAppTy (GHC.HsAppTy _ l r)) = do
    pretty $ fmap HsTypeWithVerticalAppTy l
    newline
    indentedBlock $ pretty $ fmap HsTypeWithVerticalAppTy r
  pretty' (HsType' _ _ x) = prettyHsType x

prettyHsType :: GHC.HsType GHC.GhcPs -> Printer ()
prettyHsType (GHC.HsForAllTy _ tele body) =
  (pretty tele >> space) |=> pretty body
prettyHsType GHC.HsQualTy {..} = hor <-|> ver
  where
    hor = spaced [pretty $ Context hst_ctxt, string "=>", pretty hst_body]
    ver = do
      pretty $ Context hst_ctxt
      lined [string " =>", indentedBlock $ pretty hst_body]
prettyHsType (GHC.HsTyVar _ GHC.NotPromoted x) = pretty x
prettyHsType (GHC.HsTyVar _ GHC.IsPromoted x) = string "'" >> pretty x
prettyHsType x@(GHC.HsAppTy _ l r) = hor <-|> ver
  where
    hor = spaced $ fmap pretty [l, r]
    ver = pretty $ HsTypeWithVerticalAppTy x
#if MIN_VERSION_ghc_lib_parser(9,8,1)
prettyHsType (GHC.HsAppKindTy _ l _ r) = pretty l >> string " @" >> pretty r
#else
prettyHsType (GHC.HsAppKindTy _ l r) = pretty l >> string " @" >> pretty r
#endif
prettyHsType (GHC.HsFunTy _ _ a b) = (pretty a >> string " -> ") |=> pretty b
prettyHsType (GHC.HsListTy _ xs) = brackets $ pretty xs
prettyHsType (GHC.HsTupleTy _ GHC.HsUnboxedTuple []) = string "(# #)"
prettyHsType (GHC.HsTupleTy _ GHC.HsBoxedOrConstraintTuple []) = string "()"
prettyHsType (GHC.HsTupleTy _ GHC.HsUnboxedTuple xs) =
  hvUnboxedTuple' $ fmap pretty xs
prettyHsType (GHC.HsTupleTy _ GHC.HsBoxedOrConstraintTuple xs) =
  hvTuple' $ fmap pretty xs
prettyHsType (GHC.HsSumTy _ xs) = hvUnboxedSum' $ fmap pretty xs
-- For `HsOpTy`, we do not need a single quote for the infix operator. An
-- explicit promotion is necessary if there is a data constructor and
-- a type with the same name. However, infix data constructors never
-- share their names with types because types cannot contain symbols.
-- Thus there is no ambiguity.
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsType (GHC.HsOpTy _ _ l op r) = do
  lineBreak <- gets (configLineBreaks . psConfig)
  if showOutputable op `elem` lineBreak
    then do
      pretty l
      newline
      pretty $ fmap mkInfixOperator op
      space
      pretty r
    else spaced [pretty l, pretty $ fmap mkInfixOperator op, pretty r]
#else
prettyHsType (GHC.HsOpTy _ l op r) = do
  lineBreak <- gets (configLineBreaks . psConfig)
  if showOutputable op `elem` lineBreak
    then do
      pretty l
      newline
      pretty $ fmap mkInfixOperator op
      space
      pretty r
    else spaced [pretty l, pretty $ fmap mkInfixOperator op, pretty r]
#endif
prettyHsType (GHC.HsParTy _ inside) = parens $ pretty inside
prettyHsType (GHC.HsIParamTy _ x ty) =
  spaced [string "?" >> pretty x, string "::", pretty ty]
prettyHsType GHC.HsStarTy {} = string "*"
prettyHsType (GHC.HsKindSig _ t k) = spaced [pretty t, string "::", pretty k]
prettyHsType (GHC.HsSpliceTy _ sp) = pretty $ mkSplice sp
prettyHsType GHC.HsDocTy {} = docNode
prettyHsType (GHC.HsBangTy _ pack x) = pretty pack >> pretty x
prettyHsType (GHC.HsRecTy _ xs) =
  hvFields $ fmap (pretty . fmap mkRecordField . fromGenLocated) xs
prettyHsType (GHC.HsExplicitListTy _ _ xs) =
  case xs of
    [] -> string "'[]"
    _ -> hvPromotedList $ fmap pretty xs
prettyHsType (GHC.HsExplicitTupleTy _ xs) = hPromotedTuple $ fmap pretty xs
prettyHsType (GHC.HsTyLit _ x) = pretty x
prettyHsType GHC.HsWildCardTy {} = string "_"
prettyHsType GHC.XHsType {} = notGeneratedByParser

instance Pretty
           (GHC.GRHSs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' = pretty' . GRHSsExpr GRHSExprNormal

instance Pretty GRHSsExpr where
  pretty' (GRHSsExpr {grhssExpr = GHC.GRHSs {..}, ..}) = do
    mapM_ (pretty . fmap (GRHSExpr grhssExprType)) grhssGRHSs
    case (grhssLocalBinds, grhssExprType) of
      (GHC.HsValBinds {}, GRHSExprCase) ->
        indentedBlock $ do
          newline
          string "where " |=> pretty grhssLocalBinds
      (GHC.HsValBinds epa lr, _) ->
        indentedWithSpace 2
          $ newlinePrefixed
              [ string "where"
              , printCommentsAnd (GHC.L epa lr) (indentedWithSpace 2 . pretty)
              ]
      _ -> return ()

instance Pretty
           (GHC.GRHSs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs))) where
  pretty' GHC.GRHSs {..} = do
    mapM_ (pretty . fmap GRHSProc) grhssGRHSs
    case grhssLocalBinds of
      (GHC.HsValBinds epa lr) ->
        indentedWithSpace 2
          $ newlinePrefixed
              [ string "where"
              , printCommentsAnd (GHC.L epa lr) (indentedWithSpace 2 . pretty)
              ]
      _ -> return ()

instance Pretty (GHC.HsMatchContext GHC.GhcPs) where
  pretty' = prettyHsMatchContext

prettyHsMatchContext :: GHC.HsMatchContext GHC.GhcPs -> Printer ()
prettyHsMatchContext GHC.FunRhs {..} = pretty mc_strictness >> pretty mc_fun
prettyHsMatchContext GHC.LambdaExpr = return ()
prettyHsMatchContext GHC.CaseAlt = return ()
prettyHsMatchContext GHC.IfAlt {} = notGeneratedByParser
prettyHsMatchContext GHC.ArrowMatchCtxt {} = notGeneratedByParser
prettyHsMatchContext GHC.PatBindRhs {} = notGeneratedByParser
prettyHsMatchContext GHC.PatBindGuards {} = notGeneratedByParser
prettyHsMatchContext GHC.RecUpd {} = notGeneratedByParser
prettyHsMatchContext GHC.StmtCtxt {} = notGeneratedByParser
prettyHsMatchContext GHC.ThPatSplice {} = notGeneratedByParser
prettyHsMatchContext GHC.ThPatQuote {} = notGeneratedByParser
prettyHsMatchContext GHC.PatSyn {} = notGeneratedByParser
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsMatchContext GHC.LamCaseAlt {} = notUsedInParsedStage
#endif
instance Pretty (GHC.ParStmtBlock GHC.GhcPs GHC.GhcPs) where
  pretty' (GHC.ParStmtBlock _ xs _ _) = hvCommaSep $ fmap pretty xs

instance Pretty ParStmtBlockInsideVerticalList where
  pretty' (ParStmtBlockInsideVerticalList (GHC.ParStmtBlock _ xs _ _)) =
    vCommaSep $ fmap pretty xs

instance Pretty GHC.RdrName where
  pretty' = pretty . mkPrefixOperator

instance Pretty
           (GHC.GRHS
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' = pretty' . GRHSExpr GRHSExprNormal

instance Pretty GRHSExpr where
  pretty' (GRHSExpr {grhsExpr = (GHC.GRHS _ [] body), ..}) = do
    space
    rhsSeparator grhsExprType
    case GHC.unLoc body of
      GHC.HsDo _ (GHC.DoExpr m) stmts ->
        printCommentsAnd body (const (doExpr (QualifiedDo m Do) stmts))
      GHC.HsDo _ (GHC.MDoExpr m) stmts ->
        printCommentsAnd body (const (doExpr (QualifiedDo m Mdo) stmts))
      GHC.OpApp _ (GHC.L _ (GHC.HsDo _ GHC.DoExpr {} _)) _ _ ->
        space >> pretty (fmap mkExpression body)
      GHC.OpApp _ (GHC.L _ (GHC.HsDo _ GHC.MDoExpr {} _)) _ _ ->
        space >> pretty (fmap mkExpression body)
      _ ->
        let hor = space >> pretty (fmap mkExpression body)
            ver = newline >> indentedBlock (pretty $ fmap mkExpression body)
         in hor <-|> ver
    where
      doExpr qDo stmts = do
        space
        pretty qDo
        newline
        indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
  pretty' (GRHSExpr {grhsExpr = (GHC.GRHS _ guards body), ..}) = do
    unless (grhsExprType == GRHSExprMultiWayIf) newline
    (if grhsExprType == GRHSExprMultiWayIf
       then id
       else indentedBlock) $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      space
      rhsSeparator grhsExprType
      printCommentsAnd body $ \case
        GHC.HsDo _ (GHC.DoExpr m) stmts -> doExpr (QualifiedDo m Do) stmts
        GHC.HsDo _ (GHC.MDoExpr m) stmts -> doExpr (QualifiedDo m Mdo) stmts
        x ->
          let hor = space >> pretty (mkExpression x)
              ver = newline >> indentedBlock (pretty $ mkExpression x)
           in hor <-|> ver
    where
      doExpr qDo stmts = do
        space
        pretty qDo
        newline
        indentedBlock (printCommentsAnd stmts (lined . fmap pretty))

instance Pretty GRHSProc where
  pretty' (GRHSProc (GHC.GRHS _ guards body)) =
    if null guards
      then bodyPrinter
      else do
        newline
        indentedBlock $ do
          string "| " |=> vCommaSep (fmap pretty guards)
          space
          bodyPrinter
    where
      bodyPrinter = do
        string "->"
        printCommentsAnd body $ \case
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

instance Pretty GHC.EpaCommentTok where
  pretty' (GHC.EpaLineComment c) = string c
  pretty' (GHC.EpaBlockComment c) =
    case lines c of
      [] -> pure ()
      [x] -> string x
      (x:xs) -> do
        string x
        newline
        -- 'indentedWithFixedLevel 0' is used because an 'EpaBlockComment'
        -- contains indent spaces for all lines except the first one.
        indentedWithFixedLevel 0 $ lined $ fmap string xs
  pretty' _ = docNode

instance Pretty (GHC.Pat GHC.GhcPs) where
  pretty' = prettyPat

instance Pretty PatInsidePatDecl where
  pretty' (PatInsidePatDecl (GHC.ConPat {pat_args = (GHC.InfixCon l r), ..})) =
    spaced [pretty l, pretty $ fmap mkInfixOperator pat_con, pretty r]
  pretty' (PatInsidePatDecl x) = pretty x

prettyPat :: GHC.Pat GHC.GhcPs -> Printer ()
prettyPat GHC.WildPat {} = string "_"
prettyPat (GHC.VarPat _ x) = pretty x
prettyPat (GHC.LazyPat _ x) = string "~" >> pretty x
#if MIN_VERSION_ghc_lib_parser(9,6,1)
prettyPat (GHC.AsPat _ a _ b) = pretty a >> string "@" >> pretty b
#else
prettyPat (GHC.AsPat _ a b) = pretty a >> string "@" >> pretty b
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyPat (GHC.ParPat _ _ inner _) = parens $ pretty inner
#else
prettyPat (GHC.ParPat _ inner) = parens $ pretty inner
#endif
prettyPat (GHC.BangPat _ x) = string "!" >> pretty x
prettyPat (GHC.ListPat _ xs) = hList $ fmap pretty xs
prettyPat (GHC.TuplePat _ pats GHC.Boxed) = hTuple $ fmap pretty pats
prettyPat (GHC.TuplePat _ pats GHC.Unboxed) = hUnboxedTuple $ fmap pretty pats
prettyPat (GHC.SumPat _ x position numElem) = do
  string "(#"
  forM_ [1 .. numElem] $ \idx -> do
    if idx == position
      then string " " >> pretty x >> string " "
      else string " "
    when (idx < numElem) $ string "|"
  string "#)"
prettyPat GHC.ConPat {..} =
  case pat_args of
    GHC.PrefixCon _ as -> do
      pretty $ fmap mkPrefixOperator pat_con
      spacePrefixed $ fmap pretty as
    GHC.RecCon rec -> (pretty pat_con >> space) |=> pretty (RecConPat rec)
    GHC.InfixCon a b -> do
      pretty a
      unlessSpecialOp (GHC.unLoc pat_con) space
      pretty $ fmap mkInfixOperator pat_con
      unlessSpecialOp (GHC.unLoc pat_con) space
      pretty b
prettyPat (GHC.ViewPat _ l r) =
  spaced [pretty $ fmap mkExpression l, string "->", pretty r]
prettyPat (GHC.SplicePat _ x) = pretty $ mkSplice x
prettyPat (GHC.LitPat _ x) = pretty x
prettyPat (GHC.NPat _ x _ _) = pretty x
prettyPat (GHC.NPlusKPat _ n k _ _ _) = pretty n >> string "+" >> pretty k
prettyPat (GHC.SigPat _ l r) = spaced [pretty l, string "::", pretty r]

instance Pretty RecConPat where
  pretty' (RecConPat GHC.HsRecFields {..}) =
    case fieldPrinters of
      [] -> string "{}"
      [x] -> braces x
      xs -> hvFields xs
    where
      fieldPrinters =
        fmap (pretty . fmap RecConField) rec_flds
          ++ maybeToList (fmap (const (string "..")) rec_dotdot)

instance Pretty SBF.SigBindFamily where
  pretty' (SBF.Sig x) = pretty $ mkSignature x
  pretty' (SBF.Bind x) = pretty $ mkBind x
  pretty' (SBF.TypeFamily x)
    | Just fam <- mkTypeFamily x = pretty fam
    | otherwise = error "Unreachable"
  pretty' (SBF.TyFamInst x) = pretty x
  pretty' (SBF.DataFamInst x) = pretty $ DataFamInstDeclInsideClassInst x

instance Pretty GHC.EpaComment where
  pretty' GHC.EpaComment {..} = pretty ac_tok

instance Pretty (GHC.HsLocalBindsLR GHC.GhcPs GHC.GhcPs) where
  pretty' (GHC.HsValBinds _ lr) = pretty lr
  pretty' (GHC.HsIPBinds _ x) = pretty x
  pretty' GHC.EmptyLocalBinds {} =
    error
      "This branch indicates that the bind is empty, but since calling this code means that let or where has already been output, it cannot be handled here. It should be handled higher up in the AST."

instance Pretty (GHC.HsValBindsLR GHC.GhcPs GHC.GhcPs) where
  pretty' (GHC.ValBinds _ methods sigs) = lined $ fmap pretty sigsAndMethods
    where
      sigsAndMethods =
        SBF.mkSortedLSigBindFamilyList sigs (GHC.bagToList methods) [] [] []
  pretty' GHC.XValBindsLR {} = notUsedInParsedStage

instance Pretty (GHC.HsTupArg GHC.GhcPs) where
  pretty' (GHC.Present _ e) = pretty $ fmap mkExpression e
  pretty' GHC.Missing {} = pure () -- This appears in a tuple section.
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty RecConField where
  pretty' (RecConField GHC.HsFieldBind {..}) = do
    pretty hfbLHS
    unless hfbPun $ do
      string " = "
      pretty hfbRHS
#else
-- | For pattern matching against a record.
instance Pretty
           (GHC.HsRecField'
              (GHC.FieldOcc GHC.GhcPs)
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.Pat GHC.GhcPs))) where
  pretty' GHC.HsRecField {..} =
    (pretty hsRecFieldLbl >> string " = ") |=> pretty hsRecFieldArg

-- | For record updates.
instance Pretty
           (GHC.HsRecField'
              (GHC.FieldOcc GHC.GhcPs)
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' GHC.HsRecField {..} = do
    pretty hsRecFieldLbl
    unless hsRecPun $ do
      string " ="
      horizontal <-|> vertical
    where
      horizontal = space >> pretty (fmap mkExpression hsRecFieldArg)
      vertical =
        newline >> indentedBlock (pretty $ fmap mkExpression hsRecFieldArg)
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
-- | For pattern matchings against records.
instance Pretty
           (GHC.HsFieldBind
              (GHC.GenLocated (GHC.SrcAnn GHC.NoEpAnns) (GHC.FieldOcc GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.Pat GHC.GhcPs))) where
  pretty' GHC.HsFieldBind {..} =
    (pretty hfbLHS >> string " = ") |=> pretty hfbRHS

-- | For record updates.
instance Pretty
           (GHC.HsFieldBind
              (GHC.GenLocated (GHC.SrcAnn GHC.NoEpAnns) (GHC.FieldOcc GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' GHC.HsFieldBind {..} = do
    pretty hfbLHS
    unless hfbPun $ do
      string " ="
      horizontal <-|> vertical
    where
      horizontal = space >> pretty (fmap mkExpression hfbRHS)
      vertical = newline >> indentedBlock (pretty $ fmap mkExpression hfbRHS)
#else
instance Pretty RecConField where
  pretty' (RecConField GHC.HsRecField {..}) = do
    pretty hsRecFieldLbl
    unless hsRecPun $ do
      string " = "
      pretty hsRecFieldArg
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (GHC.FieldOcc GHC.GhcPs) where
  pretty' GHC.FieldOcc {..} = pretty foLabel
#else
instance Pretty (GHC.FieldOcc GHC.GhcPs) where
  pretty' GHC.FieldOcc {..} = pretty rdrNameFieldOcc
#endif
instance Pretty a => Pretty (GHC.HsScaled GHC.GhcPs a) where
  pretty' (GHC.HsScaled _ x) = pretty x

instance Pretty InfixExpr where
  pretty' (InfixExpr (GHC.L _ (GHC.HsVar _ bind))) =
    pretty $ fmap mkInfixOperator bind
  pretty' (InfixExpr x) = pretty' $ fmap mkExpression x

instance Pretty InfixApp where
  pretty' InfixApp {..} = horizontal <-|> vertical
    where
      horizontal =
        spaced
          [ pretty $ fmap mkExpression lhs
          , pretty $ InfixExpr op
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
      prettyOps [l, o, GHC.L _ (GHC.HsDo _ (GHC.DoExpr m) xs)] = do
        spaced
          [ pretty $ fmap mkExpression l
          , pretty $ InfixExpr o
          , pretty $ QualifiedDo m Do
          ]
        newline
        indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
      prettyOps [l, o, GHC.L _ (GHC.HsDo _ (GHC.MDoExpr m) xs)] = do
        spaced
          [ pretty $ fmap mkExpression l
          , pretty $ InfixExpr o
          , pretty $ QualifiedDo m Mdo
          ]
        newline
        indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
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

instance Pretty (GHC.FieldLabelStrings GHC.GhcPs) where
  pretty' (GHC.FieldLabelStrings xs) = hDotSep $ fmap pretty xs

instance Pretty (GHC.AmbiguousFieldOcc GHC.GhcPs) where
  pretty' (GHC.Unambiguous _ name) = pretty name
  pretty' (GHC.Ambiguous _ name) = pretty name

instance Pretty (GHC.DerivClauseTys GHC.GhcPs) where
  pretty' (GHC.DctSingle _ ty) = parens $ pretty ty
  pretty' (GHC.DctMulti _ ts) = hvTuple $ fmap pretty ts

instance Pretty GHC.StringLiteral where
  pretty' = output

instance Pretty (GHC.ArithSeqInfo GHC.GhcPs) where
  pretty' (GHC.From from) =
    brackets $ spaced [pretty $ fmap mkExpression from, string ".."]
  pretty' (GHC.FromThen from next) =
    brackets
      $ spaced
          [ pretty (fmap mkExpression from)
              >> comma
              >> pretty (fmap mkExpression next)
          , string ".."
          ]
  pretty' (GHC.FromTo from to) =
    brackets
      $ spaced
          [ pretty $ fmap mkExpression from
          , string ".."
          , pretty $ fmap mkExpression to
          ]
  pretty' (GHC.FromThenTo from next to) =
    brackets
      $ spaced
          [ pretty (fmap mkExpression from)
              >> comma
              >> pretty (fmap mkExpression next)
          , string ".."
          , pretty $ fmap mkExpression to
          ]

instance Pretty (GHC.HsForAllTelescope GHC.GhcPs) where
  pretty' GHC.HsForAllVis {..} = do
    string "forall "
    spaced $ fmap (pretty . fmap mkTypeVariable . fromGenLocated) hsf_vis_bndrs
    dot
  pretty' GHC.HsForAllInvis {..} = do
    string "forall "
    spaced
      $ fmap (pretty . fmap mkTypeVariable . fromGenLocated) hsf_invis_bndrs
    dot

instance Pretty Context where
  pretty' (Context xs) =
    pretty (HorizontalContext xs) <-|> pretty (VerticalContext xs)
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty HorizontalContext where
  pretty' (HorizontalContext xs) =
    constraintsParens $ printCommentsAnd xs (hCommaSep . fmap pretty)
    where
      constraintsParens =
        case xs of
          (GHC.L _ []) -> parens
          (GHC.L _ [_]) -> id
          _ -> parens

instance Pretty VerticalContext where
  pretty' (VerticalContext full@(GHC.L _ [])) =
    printCommentsAnd full (const $ string "()")
  pretty' (VerticalContext full@(GHC.L _ [x])) =
    printCommentsAnd full (const $ pretty x)
  pretty' (VerticalContext xs) = printCommentsAnd xs (vTuple . fmap pretty)
#else
instance Pretty HorizontalContext where
  pretty' (HorizontalContext xs) =
    constraintsParens $ mapM_ (`printCommentsAnd` (hCommaSep . fmap pretty)) xs
    where
      constraintsParens =
        case xs of
          Nothing -> id
          Just (GHC.L _ []) -> parens
          Just (GHC.L _ [_]) -> id
          Just _ -> parens

instance Pretty VerticalContext where
  pretty' (VerticalContext Nothing) = pure ()
  pretty' (VerticalContext (Just (GHC.L _ []))) = string "()"
  pretty' (VerticalContext (Just full@(GHC.L _ [x]))) =
    printCommentsAnd full (const $ pretty x)
  pretty' (VerticalContext (Just xs)) =
    printCommentsAnd xs (vTuple . fmap pretty)
#endif
-- Wrap a value of this type with 'ModulenameWithPrefix' to print it with
-- the "module " prefix.
instance Pretty GHC.ModuleName where
  pretty' = output

instance Pretty ModuleNameWithPrefix where
  pretty' (ModuleNameWithPrefix name) = spaced [string "module", pretty name]

instance Pretty (GHC.IE GHC.GhcPs) where
  pretty' (GHC.IEVar _ name) = pretty name
  pretty' (GHC.IEThingAbs _ name) = pretty name
  pretty' (GHC.IEThingAll _ name) = do
    pretty name
    string "(..)"
  -- FIXME: Currently, pretty-printing a 'IEThingWith' uses
  -- 'ghc-lib-parser''s pretty-printer. However, we should avoid it because
  -- 'ghc-lib-parser' may suddenly change how it prints, resulting in
  -- unexpected test failures.
  pretty' x@GHC.IEThingWith {} =
    case lines $ showOutputable x of
      [] -> pure ()
      [x'] -> string x'
      xs -> do
        string $ head xs
        indentedWithFixedLevel 0 $ newlinePrefixed $ string <$> tail xs
  pretty' (GHC.IEModuleContents _ name) =
    pretty $ fmap ModuleNameWithPrefix name
  pretty' GHC.IEGroup {} = docNode
  pretty' GHC.IEDoc {} = docNode
  pretty' GHC.IEDocNamed {} = docNode

instance Pretty
           (GHC.FamEqn
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))) where
  pretty' GHC.FamEqn {..} = do
    pretty feqn_tycon
    spacePrefixed $ fmap pretty feqn_pats
    string " = "
    pretty feqn_rhs

-- | Pretty-print a data instance.
instance Pretty (GHC.FamEqn GHC.GhcPs (GHC.HsDataDefn GHC.GhcPs)) where
  pretty' = pretty' . FamEqnTopLevel
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance Pretty FamEqn' where
  pretty' FamEqn' {famEqn = GHC.FamEqn {..}, ..} = do
    spaced $ string prefix : pretty feqn_tycon : fmap pretty feqn_pats
    pretty (mkDataBody feqn_rhs)
    where
      prefix =
        case (famEqnFor, GHC.dd_cons feqn_rhs) of
          (DataFamInstDeclForTopLevel, GHC.NewTypeCon {}) -> "newtype instance"
          (DataFamInstDeclForTopLevel, GHC.DataTypeCons {}) -> "data instance"
          (DataFamInstDeclForInsideClassInst, GHC.NewTypeCon {}) -> "newtype"
          (DataFamInstDeclForInsideClassInst, GHC.DataTypeCons {}) -> "data"
#else
instance Pretty FamEqn' where
  pretty' FamEqn' {famEqn = GHC.FamEqn {..}, ..} = do
    spaced $ string prefix : pretty feqn_tycon : fmap pretty feqn_pats
    pretty (mkDataBody feqn_rhs)
    where
      prefix =
        case (famEqnFor, GHC.dd_ND feqn_rhs) of
          (DataFamInstDeclForTopLevel, GHC.NewType) -> "newtype instance"
          (DataFamInstDeclForTopLevel, GHC.DataType) -> "data instance"
          (DataFamInstDeclForInsideClassInst, GHC.NewType) -> "newtype"
          (DataFamInstDeclForInsideClassInst, GHC.DataType) -> "data"
#endif
-- | HsArg (LHsType GhcPs) (LHsType GhcPs)
#if MIN_VERSION_ghc_lib_parser(9,8,1)
instance Pretty
           (GHC.HsArg
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))) where
  pretty' (GHC.HsValArg x) = pretty x
  pretty' (GHC.HsTypeArg _ x) = string "@" >> pretty x
  pretty' GHC.HsArgPar {} = notUsedInParsedStage
#else
instance Pretty
           (GHC.HsArg
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))) where
  pretty' (GHC.HsValArg x) = pretty x
  pretty' (GHC.HsTypeArg _ x) = string "@" >> pretty x
  pretty' GHC.HsArgPar {} = notUsedInParsedStage
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (GHC.WithHsDocIdentifiers GHC.StringLiteral GHC.GhcPs) where
  pretty' GHC.WithHsDocIdentifiers {..} = pretty hsDocString
#endif

#if MIN_VERSION_ghc_lib_parser(9,6,1)
-- | 'Pretty' for 'LIEWrappedName (IdP GhcPs)'
instance Pretty (GHC.IEWrappedName GHC.GhcPs) where
  pretty' (GHC.IEName _ name) = pretty name
  pretty' (GHC.IEPattern _ name) = spaced [string "pattern", pretty name]
  pretty' (GHC.IEType _ name) = string "type " >> pretty name
#else
-- | 'Pretty' for 'LIEWrappedName (IdP GhcPs)'
instance Pretty (GHC.IEWrappedName GHC.RdrName) where
  pretty' (GHC.IEName name) = pretty name
  pretty' (GHC.IEPattern _ name) = spaced [string "pattern", pretty name]
  pretty' (GHC.IEType _ name) = string "type " >> pretty name
#endif
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (GHC.DotFieldOcc GHC.GhcPs) where
  pretty' GHC.DotFieldOcc {..} = printCommentsAnd dfoLabel pretty
#elif MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (GHC.DotFieldOcc GHC.GhcPs) where
  pretty' GHC.DotFieldOcc {..} =
    printCommentsAnd dfoLabel (string . GHC.unpackFS)
#else
instance Pretty (GHC.HsFieldLabel GHC.GhcPs) where
  pretty' GHC.HsFieldLabel {..} =
    printCommentsAnd hflLabel (string . GHC.unpackFS)
#endif
instance Pretty GHC.OccName where
  pretty' = output

-- | 'Pretty' for 'LHsWcType'
instance Pretty
           (GHC.HsWildCardBndrs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))) where
  pretty' GHC.HsWC {..} = pretty hswc_body

instance Pretty (GHC.TyFamInstDecl GHC.GhcPs) where
  pretty' GHC.TyFamInstDecl {..} = string "type " >> pretty tfid_eqn

instance Pretty TopLevelTyFamInstDecl where
  pretty' (TopLevelTyFamInstDecl GHC.TyFamInstDecl {..}) =
    string "type instance " >> pretty tfid_eqn

instance Pretty (GHC.DataFamInstDecl GHC.GhcPs) where
  pretty' = pretty' . DataFamInstDeclTopLevel

instance Pretty DataFamInstDecl' where
  pretty' DataFamInstDecl' {dataFamInstDecl = GHC.DataFamInstDecl {..}, ..} =
    pretty $ FamEqn' dataFamInstDeclFor dfid_eqn

-- | 'Pretty' for 'HsPatSynDetails'.
instance Pretty
           (GHC.HsConDetails
              Void
              (GHC.GenLocated GHC.SrcSpanAnnN GHC.RdrName)
              [GHC.RecordPatSynField GHC.GhcPs]) where
  pretty' (GHC.PrefixCon _ xs) = spaced $ fmap pretty xs
  pretty' (GHC.RecCon rec) = hFields $ fmap pretty rec
  pretty' GHC.InfixCon {} =
    error
      "Cannot handle here because `InfixCon` does not have the information of the constructor."

instance Pretty (GHC.HsPatSynDir GHC.GhcPs) where
  pretty' GHC.Unidirectional = string "<-"
  pretty' GHC.ImplicitBidirectional = string "="
  pretty' GHC.ExplicitBidirectional {} = string "<-"

instance Pretty (GHC.HsOverLit GHC.GhcPs) where
  pretty' GHC.OverLit {..} = pretty ol_val

instance Pretty GHC.OverLitVal where
  pretty' (GHC.HsIntegral x) = pretty x
  pretty' (GHC.HsFractional x) = pretty x
  pretty' (GHC.HsIsString _ x) = string $ GHC.unpackFS x
#if MIN_VERSION_ghc_lib_parser(9,8,1)
instance Pretty GHC.IntegralLit where
  pretty' GHC.IL {il_text = GHC.SourceText s} = output s
  pretty' GHC.IL {..} = string $ show il_value
#else
instance Pretty GHC.IntegralLit where
  pretty' GHC.IL {il_text = GHC.SourceText s} = string s
  pretty' GHC.IL {..} = string $ show il_value
#endif
instance Pretty GHC.FractionalLit where
  pretty' = output

instance Pretty (GHC.HsLit GHC.GhcPs) where
  pretty' x@(GHC.HsChar _ _) = output x
  pretty' x@GHC.HsCharPrim {} = output x
  pretty' GHC.HsInt {} = notUsedInParsedStage
  pretty' (GHC.HsIntPrim _ x) = string $ show x ++ "#"
  pretty' GHC.HsWordPrim {} = notUsedInParsedStage
  pretty' GHC.HsInt64Prim {} = notUsedInParsedStage
  pretty' GHC.HsWord64Prim {} = notUsedInParsedStage
  pretty' GHC.HsInteger {} = notUsedInParsedStage
  pretty' GHC.HsRat {} = notUsedInParsedStage
  pretty' (GHC.HsFloatPrim _ x) = pretty x >> string "#"
  pretty' GHC.HsDoublePrim {} = notUsedInParsedStage
  pretty' x =
    case x of
      GHC.HsString {} -> prettyString
      GHC.HsStringPrim {} -> prettyString
    where
      prettyString =
        case lines $ showOutputable x of
          [] -> pure ()
          [l] -> string l
          (s:ss) ->
            string "" |=> do
              string s
              newline
              indentedWithSpace (-1)
                $ lined
                $ fmap (string . dropWhile (/= '\\')) ss
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (GHC.HsPragE GHC.GhcPs) where
  pretty' (GHC.HsPragSCC _ x) =
    spaced [string "{-# SCC", pretty x, string "#-}"]
#else
instance Pretty (GHC.HsPragE GHC.GhcPs) where
  pretty' (GHC.HsPragSCC _ _ x) =
    spaced [string "{-# SCC", pretty x, string "#-}"]
#endif
instance Pretty GHC.HsIPName where
  pretty' (GHC.HsIPName x) = string $ GHC.unpackFS x
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (GHC.HsTyLit GHC.GhcPs) where
  pretty' (GHC.HsNumTy _ x) = string $ show x
  pretty' (GHC.HsStrTy _ x) = string $ ushow x
  pretty' (GHC.HsCharTy _ x) = string $ show x
#else
instance Pretty GHC.HsTyLit where
  pretty' (GHC.HsNumTy _ x) = string $ show x
  pretty' (GHC.HsStrTy _ x) = string $ ushow x
  pretty' (GHC.HsCharTy _ x) = string $ show x
#endif
instance Pretty (GHC.HsPatSigType GHC.GhcPs) where
  pretty' GHC.HsPS {..} = pretty hsps_body

instance Pretty (GHC.HsIPBinds GHC.GhcPs) where
  pretty' (GHC.IPBinds _ xs) = lined $ fmap pretty xs

instance Pretty (GHC.IPBind GHC.GhcPs) where
  pretty' = prettyIPBind

prettyIPBind :: GHC.IPBind GHC.GhcPs -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyIPBind (GHC.IPBind _ l r) =
  spaced [string "?" >> pretty l, string "=", pretty $ fmap mkExpression r]
#else
prettyIPBind (GHC.IPBind _ (Right _) _) = notUsedInParsedStage
prettyIPBind (GHC.IPBind _ (Left l) r) =
  spaced [string "?" >> pretty l, string "=", pretty $ fmap mkExpression r]
#endif
instance Pretty (GHC.RecordPatSynField GHC.GhcPs) where
  pretty' GHC.RecordPatSynField {..} = pretty recordPatSynField

instance Pretty (GHC.HsCmdTop GHC.GhcPs) where
  pretty' (GHC.HsCmdTop _ cmd) = pretty cmd

instance Pretty (GHC.HsCmd GHC.GhcPs) where
  pretty' = prettyHsCmd

prettyHsCmd :: GHC.HsCmd GHC.GhcPs -> Printer ()
prettyHsCmd (GHC.HsCmdArrApp _ f arg GHC.HsHigherOrderApp True) =
  spaced
    [pretty $ fmap mkExpression f, string "-<<", pretty $ fmap mkExpression arg]
prettyHsCmd (GHC.HsCmdArrApp _ f arg GHC.HsHigherOrderApp False) =
  spaced
    [pretty $ fmap mkExpression arg, string ">>-", pretty $ fmap mkExpression f]
prettyHsCmd (GHC.HsCmdArrApp _ f arg GHC.HsFirstOrderApp True) =
  spaced
    [pretty $ fmap mkExpression f, string "-<", pretty $ fmap mkExpression arg]
prettyHsCmd (GHC.HsCmdArrApp _ f arg GHC.HsFirstOrderApp False) =
  spaced
    [pretty $ fmap mkExpression arg, string ">-", pretty $ fmap mkExpression f]
prettyHsCmd (GHC.HsCmdArrForm _ f _ _ args) =
  bananaBrackets $ spaced $ pretty (fmap mkExpression f) : fmap pretty args
prettyHsCmd (GHC.HsCmdApp _ f arg) =
  spaced [pretty f, pretty $ fmap mkExpression arg]
prettyHsCmd (GHC.HsCmdLam _ x) = pretty x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsCmd (GHC.HsCmdPar _ _ x _) = parens $ pretty x
#else
prettyHsCmd (GHC.HsCmdPar _ x) = parens $ pretty x
#endif
prettyHsCmd (GHC.HsCmdCase _ cond arms) = do
  spaced [string "case", pretty $ fmap mkExpression cond, string "of"]
  newline
  indentedBlock $ pretty arms
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsCmd (GHC.HsCmdLamCase _ _ arms) = do
  string "\\case"
  newline
  indentedBlock $ pretty arms
#else
prettyHsCmd (GHC.HsCmdLamCase _ arms) = do
  string "\\case"
  newline
  indentedBlock $ pretty arms
#endif
prettyHsCmd (GHC.HsCmdIf _ _ cond t f) = do
  string "if "
  pretty $ fmap mkExpression cond
  newline
  indentedBlock $ lined [string "then " >> pretty t, string "else " >> pretty f]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsCmd (GHC.HsCmdLet _ _ binds _ expr) =
  lined [string "let " |=> pretty binds, string " in " |=> pretty expr]
#else
prettyHsCmd (GHC.HsCmdLet _ binds expr) =
  lined [string "let " |=> pretty binds, string " in " |=> pretty expr]
#endif
prettyHsCmd (GHC.HsCmdDo _ stmts) = do
  string "do"
  newline
  indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)

instance Pretty ListComprehension where
  pretty' ListComprehension {..} = horizontal <-|> vertical
    where
      horizontal =
        brackets
          $ spaced
              [ pretty listCompLhs
              , string "|"
              , hCommaSep $ fmap pretty listCompRhs
              ]
      vertical = do
        string "[ "
        pretty $ fmap StmtLRInsideVerticalList listCompLhs
        newline
        forM_ (stmtsAndPrefixes listCompRhs) $ \(p, x) -> do
          string p |=> pretty (fmap StmtLRInsideVerticalList x)
          newline
        string "]"
      stmtsAndPrefixes l = ("| ", head l) : fmap (", ", ) (tail l)

instance Pretty DoExpression where
  pretty' DoExpression {..} = do
    pretty qualifiedDo
    newline
    indentedBlock $ lined $ fmap pretty doStmts

instance Pretty DoOrMdo where
  pretty' Do = string "do"
  pretty' Mdo = string "mdo"

instance Pretty QualifiedDo where
  pretty' (QualifiedDo (Just m) d) = do
    pretty m
    string "."
    pretty d
  pretty' (QualifiedDo Nothing d) = pretty d

instance Pretty LetIn where
  pretty' LetIn {..} =
    lined
      [ string "let " |=> pretty letBinds
      , string " in " |=> pretty (fmap mkExpression inExpr)
      ]

instance Pretty GHC.HsSrcBang where
  pretty' (GHC.HsSrcBang _ unpack strictness) = do
    pretty unpack
    unless (unpack == GHC.NoSrcUnpack) space
    pretty strictness

instance Pretty GHC.SrcUnpackedness where
  pretty' GHC.SrcUnpack = string "{-# UNPACK #-}"
  pretty' GHC.SrcNoUnpack = string "{-# NOUNPACK #-}"
  pretty' GHC.NoSrcUnpack = pure ()

instance Pretty GHC.SrcStrictness where
  pretty' GHC.SrcLazy = string "~"
  pretty' GHC.SrcStrict = string "!"
  pretty' GHC.NoSrcStrict = pure ()
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty GHC.FieldLabelString where
  pretty' = output
#endif
-- | Marks an AST node as never appearing in an AST.
--
-- Some AST node types are only defined in `ghc-lib-parser` and not
-- generated by it.
notGeneratedByParser :: HasCallStack => a
notGeneratedByParser = error "`ghc-lib-parser` never generates this AST node."

-- | Marks an AST node as related to Haddock comments.
--
-- The parser parses haddock comments as normal ones, meaning AST nodes
-- related to haddock never appear in an AST.
docNode :: HasCallStack => a
docNode =
  error
    "This AST node is related to Haddocks, but haddock comments are treated as normal ones, and this node should never appear in an AST."

-- | Marks an AST node as never appearing in the AST.
--
-- Some AST node types are only used in the renaming or type-checking phase.
notUsedInParsedStage :: HasCallStack => a
notUsedInParsedStage =
  error
    "This AST should never appears in an AST. It only appears in the renaming or type checked stages."
