{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified GHC.Data.FastString as GHC
import qualified GHC.Hs as GHC
import GHC.Stack
import qualified GHC.Types.SourceText as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Comment (mkComment)
import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Data.Body
import HIndent.Ast.Declaration.Family.Data
import HIndent.Ast.Declaration.Family.Type
import HIndent.Ast.Declaration.Instance.Family.Type.Associated
  ( mkAssociatedType
  )
import HIndent.Ast.Declaration.Signature
import HIndent.Ast.Expression (mkExpression)
import HIndent.Ast.MatchGroup (mkCmdMatchGroup)
import HIndent.Ast.Module.Name (mkModuleName)
import HIndent.Ast.Name.ImportExport
import HIndent.Ast.Name.Prefix
import HIndent.Ast.Name.RecordField (mkFieldNameFromFieldOcc)
import HIndent.Ast.NodeComments
import HIndent.Ast.Pattern
import HIndent.Ast.Type
import HIndent.Ast.Type.ImplicitParameterName (mkImplicitParameterName)
import HIndent.Ast.Type.Strictness
import HIndent.Ast.WithComments
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import qualified HIndent.Pretty.SigBindFamily as SBF
import HIndent.Pretty.Types
import HIndent.Printer
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
import qualified GHC.Types.Name.Reader as GHC
#endif
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
import qualified GHC.Core.DataCon as GHC
#else
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
    let col = fromIntegral $ GHC.srcSpanStartCol (getAnc loc) - 1
    indentedWithFixedLevel col $ pretty c
    newline

-- | Prints comments that are on the same line as the given AST node.
printCommentOnSameLine :: CommentExtraction a => a -> Printer ()
printCommentOnSameLine (commentsOnSameLine . nodeComments -> (c:cs)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithFixedLevel
           (fromIntegral $ GHC.srcSpanStartCol $ getAnc $ GHC.getLoc c)
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
        let col = fromIntegral $ GHC.srcSpanStartCol (getAnc loc) - 1
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
           (GHC.StmtLR
              GHC.GhcPs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' = prettyStmtLRExpr

prettyStmtLRExpr ::
     GHC.StmtLR
       GHC.GhcPs
       GHC.GhcPs
       (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))
  -> Printer ()
prettyStmtLRExpr (GHC.LastStmt _ x _ _) =
  pretty $ mkExpression <$> fromGenLocated x
prettyStmtLRExpr (GHC.BindStmt _ pat body) = do
  pretty $ mkPattern <$> fromGenLocated pat
  string " <-"
  hor <-|> ver
  where
    hor = do
      space
      pretty $ mkExpression <$> fromGenLocated body
    ver = do
      newline
      indentedBlock $ pretty $ mkExpression <$> fromGenLocated body
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyStmtLRExpr GHC.ApplicativeStmt {} = notGeneratedByParser
#endif
prettyStmtLRExpr (GHC.BodyStmt _ body _ _) =
  pretty $ mkExpression <$> fromGenLocated body
prettyStmtLRExpr (GHC.LetStmt _ l) = string "let " |=> pretty l
prettyStmtLRExpr (GHC.ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
prettyStmtLRExpr GHC.TransStmt {..} =
  vCommaSep
    $ fmap pretty trS_stmts
        ++ [ string "then "
               >> pretty (mkExpression <$> fromGenLocated trS_using)
           ]
prettyStmtLRExpr GHC.RecStmt {..} =
  string "rec " |=> printCommentsAnd recS_stmts (lined . fmap pretty)

instance Pretty
           (GHC.StmtLR
              GHC.GhcPs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs))) where
  pretty' = prettyStmtLRCmd

prettyStmtLRCmd ::
     GHC.StmtLR
       GHC.GhcPs
       GHC.GhcPs
       (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs))
  -> Printer ()
prettyStmtLRCmd (GHC.LastStmt _ x _ _) = pretty x
prettyStmtLRCmd (GHC.BindStmt _ pat body) = hor <-|> ver
  where
    hor =
      spaced
        [pretty $ mkPattern <$> fromGenLocated pat, string "<-", pretty body]
    ver = do
      pretty $ mkPattern <$> fromGenLocated pat
      string " <-"
      newline
      indentedBlock $ pretty body
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyStmtLRCmd GHC.ApplicativeStmt {} = notGeneratedByParser
#endif
prettyStmtLRCmd (GHC.BodyStmt _ body _ _) = pretty body
prettyStmtLRCmd (GHC.LetStmt _ l) = string "let " |=> pretty l
prettyStmtLRCmd (GHC.ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
prettyStmtLRCmd GHC.TransStmt {..} =
  vCommaSep
    $ fmap pretty trS_stmts
        ++ [ string "then "
               >> pretty (mkExpression <$> fromGenLocated trS_using)
           ]
prettyStmtLRCmd GHC.RecStmt {..} =
  string "rec " |=> printCommentsAnd recS_stmts (lined . fmap pretty)

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
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance Pretty
           (GHC.HsMatchContext (GHC.GenLocated GHC.SrcSpanAnnN GHC.RdrName)) where
  pretty' = prettyHsMatchContext

prettyHsMatchContext ::
     GHC.HsMatchContext (GHC.GenLocated GHC.SrcSpanAnnN GHC.RdrName)
  -> Printer ()
prettyHsMatchContext GHC.FunRhs {..} =
  maybe (pure ()) pretty (mkStrictness mc_strictness)
    >> pretty (fmap mkPrefixName mc_fun)
prettyHsMatchContext GHC.CaseAlt = return ()
prettyHsMatchContext GHC.LamAlt {} = notGeneratedByParser
prettyHsMatchContext GHC.LazyPatCtx = notGeneratedByParser
prettyHsMatchContext GHC.IfAlt {} = notGeneratedByParser
prettyHsMatchContext GHC.ArrowMatchCtxt {} = notGeneratedByParser
prettyHsMatchContext GHC.PatBindRhs {} = notGeneratedByParser
prettyHsMatchContext GHC.PatBindGuards {} = notGeneratedByParser
prettyHsMatchContext GHC.RecUpd {} = notGeneratedByParser
prettyHsMatchContext GHC.StmtCtxt {} = notGeneratedByParser
prettyHsMatchContext GHC.ThPatSplice {} = notGeneratedByParser
prettyHsMatchContext GHC.ThPatQuote {} = notGeneratedByParser
prettyHsMatchContext GHC.PatSyn {} = notGeneratedByParser
#else
instance Pretty (GHC.HsMatchContext GHC.GhcPs) where
  pretty' = prettyHsMatchContext

prettyHsMatchContext :: GHC.HsMatchContext GHC.GhcPs -> Printer ()
prettyHsMatchContext GHC.FunRhs {..} =
  maybe (pure ()) pretty (mkStrictness mc_strictness)
    >> pretty (fmap mkPrefixName mc_fun)
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
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
prettyHsMatchContext GHC.LamCaseAlt {} = notUsedInParsedStage
#endif
#endif
instance Pretty (GHC.ParStmtBlock GHC.GhcPs GHC.GhcPs) where
  pretty' (GHC.ParStmtBlock _ xs _ _) = hvCommaSep $ fmap pretty xs

instance Pretty SBF.SigBindFamily where
  pretty' (SBF.Sig x) = pretty $ mkSignature x
  pretty' (SBF.Bind x) = pretty $ mkBind x
  pretty' (SBF.Family x)
    | Just fam <- mkTypeFamily x = pretty fam
    | Just fam <- mkDataFamily x = pretty fam
    | otherwise = error "Unreachable"
  pretty' (SBF.TyFamInst x) = pretty $ mkAssociatedType x
  pretty' (SBF.DataFamInst x) = pretty $ DataFamInstDeclInsideClassInst x

instance Pretty GHC.EpaComment where
  pretty' GHC.EpaComment {..} = pretty $ mkComment ac_tok

instance Pretty (GHC.HsLocalBindsLR GHC.GhcPs GHC.GhcPs) where
  pretty' (GHC.HsValBinds _ lr) = pretty lr
  pretty' (GHC.HsIPBinds _ x) = pretty x
  pretty' GHC.EmptyLocalBinds {} =
    error
      "This branch indicates that the bind is empty, but since calling this code means that let or where has already been output, it cannot be handled here. It should be handled higher up in the AST."

instance Pretty (GHC.HsValBindsLR GHC.GhcPs GHC.GhcPs) where
  pretty' = prettyHsValBindsLR

prettyHsValBindsLR :: GHC.HsValBindsLR GHC.GhcPs GHC.GhcPs -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyHsValBindsLR (GHC.ValBinds _ methods sigs) =
  lined $ fmap pretty sigsAndMethods
  where
    sigsAndMethods = SBF.mkSortedLSigBindFamilyList sigs methods [] [] []
prettyHsValBindsLR GHC.XValBindsLR {} = notUsedInParsedStage
#else
prettyHsValBindsLR (GHC.ValBinds _ methods sigs) =
  lined $ fmap pretty sigsAndMethods
  where
    sigsAndMethods =
      SBF.mkSortedLSigBindFamilyList sigs (GHC.bagToList methods) [] [] []
prettyHsValBindsLR GHC.XValBindsLR {} = notUsedInParsedStage
#endif
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
-- | For pattern matching against a record.
instance Pretty
           (GHC.HsRecField'
              (GHC.FieldOcc GHC.GhcPs)
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.Pat GHC.GhcPs))) where
  pretty' GHC.HsRecField {..} =
    (pretty (mkFieldNameFromFieldOcc <$> hsRecFieldLbl) >> string " = ")
      |=> pretty (mkPattern <$> fromGenLocated hsRecFieldArg)

-- | For record updates.
instance Pretty
           (GHC.HsRecField'
              (GHC.FieldOcc GHC.GhcPs)
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' GHC.HsRecField {..} = do
    pretty $ mkFieldNameFromFieldOcc <$> hsRecFieldLbl
    unless hsRecPun $ do
      string " ="
      horizontal <-|> vertical
    where
      horizontal = space >> pretty hsRecFieldArg
      vertical = newline >> indentedBlock (pretty hsRecFieldArg)
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
-- | For pattern matchings against records.
instance Pretty
           (GHC.HsFieldBind
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.FieldOcc GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.Pat GHC.GhcPs))) where
  pretty' GHC.HsFieldBind {..} =
    (pretty (mkFieldNameFromFieldOcc <$> hfbLHS) >> string " = ")
      |=> pretty (mkPattern <$> hfbRHS)

-- | For record updates.
instance Pretty
           (GHC.HsFieldBind
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.FieldOcc GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' GHC.HsFieldBind {..} = do
    pretty $ mkFieldNameFromFieldOcc <$> hfbLHS
    unless hfbPun $ do
      string " ="
      horizontal <-|> vertical
    where
      horizontal = space >> pretty (mkExpression <$> hfbRHS)
      vertical = newline >> indentedBlock (pretty $ mkExpression <$> hfbRHS)
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
-- | For pattern matchings against records.
instance Pretty
           (GHC.HsFieldBind
              (GHC.GenLocated (GHC.SrcAnn GHC.NoEpAnns) (GHC.FieldOcc GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.Pat GHC.GhcPs))) where
  pretty' GHC.HsFieldBind {..} =
    (pretty (mkFieldNameFromFieldOcc <$> hfbLHS) >> string " = ")
      |=> (pretty $ mkPattern <$> fromGenLocated hfbRHS)

-- | For record updates.
instance Pretty
           (GHC.HsFieldBind
              (GHC.GenLocated (GHC.SrcAnn GHC.NoEpAnns) (GHC.FieldOcc GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' GHC.HsFieldBind {..} = do
    pretty $ mkFieldNameFromFieldOcc <$> hfbLHS
    unless hfbPun $ do
      string " ="
      horizontal <-|> vertical
    where
      horizontal = space >> pretty (mkExpression <$> hfbRHS)
      vertical = newline >> indentedBlock (pretty $ mkExpression <$> hfbRHS)
#endif
instance Pretty
           (GHC.HsScaled
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))) where
  pretty' (GHC.HsScaled _ ty) = pretty $ fmap mkType ty

instance Pretty (GHC.DerivClauseTys GHC.GhcPs) where
  pretty' (GHC.DctSingle _ ty) = parens $ pretty $ mkTypeFromHsSigType <$> ty
  pretty' (GHC.DctMulti _ ts) =
    hvTuple $ pretty . fmap mkTypeFromHsSigType . fromGenLocated <$> ts
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance Pretty GHC.StringLiteral where
  pretty' GHC.StringLiteral {sl_st = GHC.SourceText s} = string $ GHC.unpackFS s
  pretty' GHC.StringLiteral {..} = string $ GHC.unpackFS sl_fs
#else
instance Pretty GHC.StringLiteral where
  pretty' = output
#endif
instance Pretty Context where
  pretty' (Context xs) =
    pretty (HorizontalContext xs) <-|> pretty (VerticalContext xs)
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty HorizontalContext where
  pretty' (HorizontalContext xs) =
    constraintsParens
      $ printCommentsAnd xs (hCommaSep . fmap (pretty . fmap mkType))
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
    printCommentsAnd full (const $ pretty $ mkType <$> x)
  pretty' (VerticalContext xs) =
    printCommentsAnd xs (vTuple . fmap (pretty . fmap mkType))
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
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance Pretty (GHC.IE GHC.GhcPs) where
  pretty' (GHC.IEVar _ name _) = pretty $ mkImportExportName <$> name
  pretty' (GHC.IEThingAbs _ name _) = pretty $ mkImportExportName <$> name
  pretty' (GHC.IEThingAll _ name _) = do
    pretty $ mkImportExportName <$> name
    string "(..)"
  -- FIXME: Currently, pretty-printing a 'IEThingWith' uses
  -- 'ghc-lib-parser''s pretty-printer. However, we should avoid it because
  -- 'ghc-lib-parser' may suddenly change how it prints, resulting in
  -- unexpected test failures.
  pretty' x@GHC.IEThingWith {} =
    case lines $ showOutputable x of
      [] -> pure ()
      [x'] -> string x'
      x':xs' -> do
        string x'
        indentedWithFixedLevel 0 $ newlinePrefixed $ string <$> xs'
  pretty' (GHC.IEModuleContents _ name) =
    printCommentsAnd name $ \n ->
      spaced [string "module", pretty (mkModuleName n)]
  pretty' GHC.IEGroup {} = docNode
  pretty' GHC.IEDoc {} = docNode
  pretty' GHC.IEDocNamed {} = docNode
#else
instance Pretty (GHC.IE GHC.GhcPs) where
  pretty' (GHC.IEVar _ name) = pretty $ mkImportExportName <$> name
  pretty' (GHC.IEThingAbs _ name) = pretty $ mkImportExportName <$> name
  pretty' (GHC.IEThingAll _ name) = do
    pretty $ mkImportExportName <$> name
    string "(..)"
  -- FIXME: Currently, pretty-printing a 'IEThingWith' uses
  -- 'ghc-lib-parser''s pretty-printer. However, we should avoid it because
  -- 'ghc-lib-parser' may suddenly change how it prints, resulting in
  -- unexpected test failures.
  pretty' x@GHC.IEThingWith {} =
    case lines $ showOutputable x of
      [] -> pure ()
      [x'] -> string x'
      x':xs' -> do
        string x'
        indentedWithFixedLevel 0 $ newlinePrefixed $ string <$> xs'
  pretty' (GHC.IEModuleContents _ name) =
    printCommentsAnd name $ \n ->
      spaced [string "module", pretty (mkModuleName n)]
  pretty' GHC.IEGroup {} = docNode
  pretty' GHC.IEDoc {} = docNode
  pretty' GHC.IEDocNamed {} = docNode
#endif
instance Pretty
           (GHC.FamEqn
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))) where
  pretty' GHC.FamEqn {..} = do
    pretty $ fmap mkPrefixName feqn_tycon
    spacePrefixed $ fmap pretty feqn_pats
    string " = "
    pretty $ mkType <$> feqn_rhs

-- | Pretty-print a data instance.
instance Pretty (GHC.FamEqn GHC.GhcPs (GHC.HsDataDefn GHC.GhcPs)) where
  pretty' = pretty' . FamEqnTopLevel
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance Pretty FamEqn' where
  pretty' FamEqn' {famEqn = GHC.FamEqn {..}, ..} = do
    spaced
      $ string prefix
          : pretty (fmap mkPrefixName feqn_tycon)
          : fmap pretty feqn_pats
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
    spaced
      $ string prefix
          : pretty (fmap mkPrefixName feqn_tycon)
          : fmap pretty feqn_pats
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
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance Pretty
           (GHC.HsArg
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))) where
  pretty' (GHC.HsValArg _ x) = pretty $ mkType <$> x
  pretty' (GHC.HsTypeArg _ x) = string "@" >> pretty (mkType <$> x)
  pretty' GHC.HsArgPar {} = notUsedInParsedStage
#elif MIN_VERSION_ghc_lib_parser(9, 8, 1)
instance Pretty
           (GHC.HsArg
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))) where
  pretty' (GHC.HsValArg x) = pretty $ mkType <$> x
  pretty' (GHC.HsTypeArg _ x) = string "@" >> pretty (mkType <$> x)
  pretty' GHC.HsArgPar {} = notUsedInParsedStage
#else
instance Pretty
           (GHC.HsArg
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))) where
  pretty' (GHC.HsValArg x) = pretty $ mkType <$> x
  pretty' (GHC.HsTypeArg _ x) = string "@" >> pretty (mkType <$> x)
  pretty' GHC.HsArgPar {} = notUsedInParsedStage
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (GHC.WithHsDocIdentifiers GHC.StringLiteral GHC.GhcPs) where
  pretty' GHC.WithHsDocIdentifiers {..} = pretty hsDocString
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
-- | 'Pretty' for 'LHsWcType'
instance Pretty
           (GHC.HsWildCardBndrs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))) where
  pretty' GHC.HsWC {..} = pretty $ mkType <$> hswc_body

instance Pretty TopLevelTyFamInstDecl where
  pretty' (TopLevelTyFamInstDecl GHC.TyFamInstDecl {..}) =
    string "instance " >> pretty tfid_eqn

instance Pretty (GHC.DataFamInstDecl GHC.GhcPs) where
  pretty' = pretty' . DataFamInstDeclTopLevel

instance Pretty DataFamInstDecl' where
  pretty' DataFamInstDecl' {dataFamInstDecl = GHC.DataFamInstDecl {..}, ..} =
    pretty $ FamEqn' dataFamInstDeclFor dfid_eqn

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
  pretty' = prettyHsLit

prettyHsLit :: GHC.HsLit GHC.GhcPs -> Printer ()
prettyHsLit x@(GHC.HsChar _ _) = output x
prettyHsLit x@GHC.HsCharPrim {} = output x
prettyHsLit GHC.HsInt {} = notUsedInParsedStage
prettyHsLit (GHC.HsIntPrim _ x) = string $ show x ++ "#"
prettyHsLit GHC.HsWordPrim {} = notUsedInParsedStage
prettyHsLit GHC.HsInt64Prim {} = notUsedInParsedStage
prettyHsLit GHC.HsWord64Prim {} = notUsedInParsedStage
prettyHsLit GHC.HsInteger {} = notUsedInParsedStage
prettyHsLit GHC.HsRat {} = notUsedInParsedStage
prettyHsLit (GHC.HsFloatPrim _ x) = pretty x >> string "#"
prettyHsLit GHC.HsDoublePrim {} = notUsedInParsedStage
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
prettyHsLit GHC.HsInt8Prim {} = notUsedInParsedStage
prettyHsLit GHC.HsInt16Prim {} = notUsedInParsedStage
prettyHsLit GHC.HsInt32Prim {} = notUsedInParsedStage
prettyHsLit GHC.HsWord8Prim {} = notUsedInParsedStage
prettyHsLit GHC.HsWord16Prim {} = notUsedInParsedStage
prettyHsLit GHC.HsWord32Prim {} = notUsedInParsedStage
#endif
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyHsLit GHC.HsMultilineString {} = notGeneratedByParser
#endif
prettyHsLit x =
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
instance Pretty (GHC.HsIPBinds GHC.GhcPs) where
  pretty' (GHC.IPBinds _ xs) = lined $ fmap pretty xs

instance Pretty (GHC.IPBind GHC.GhcPs) where
  pretty' = prettyIPBind

prettyIPBind :: GHC.IPBind GHC.GhcPs -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyIPBind (GHC.IPBind _ l r) =
  spaced
    [ pretty $ mkImplicitParameterName <$> fromGenLocated l
    , string "="
    , pretty $ mkExpression <$> fromGenLocated r
    ]
#else
prettyIPBind (GHC.IPBind _ (Right _) _) = notUsedInParsedStage
prettyIPBind (GHC.IPBind _ (Left l) r) =
  spaced
    [ pretty $ mkImplicitParameterName <$> fromGenLocated l
    , string "="
    , pretty $ mkExpression <$> fromGenLocated r
    ]
#endif
instance Pretty (GHC.HsCmdTop GHC.GhcPs) where
  pretty' (GHC.HsCmdTop _ cmd) = pretty cmd

instance Pretty (GHC.HsCmd GHC.GhcPs) where
  pretty' = prettyHsCmd

prettyHsCmd :: GHC.HsCmd GHC.GhcPs -> Printer ()
prettyHsCmd (GHC.HsCmdArrApp _ f arg GHC.HsHigherOrderApp True) =
  spaced
    [ pretty $ mkExpression <$> fromGenLocated f
    , string "-<<"
    , pretty $ mkExpression <$> fromGenLocated arg
    ]
prettyHsCmd (GHC.HsCmdArrApp _ f arg GHC.HsHigherOrderApp False) =
  spaced
    [ pretty $ mkExpression <$> fromGenLocated arg
    , string ">>-"
    , pretty $ mkExpression <$> fromGenLocated f
    ]
prettyHsCmd (GHC.HsCmdArrApp _ f arg GHC.HsFirstOrderApp True) =
  spaced
    [ pretty $ mkExpression <$> fromGenLocated f
    , string "-<"
    , pretty $ mkExpression <$> fromGenLocated arg
    ]
prettyHsCmd (GHC.HsCmdArrApp _ f arg GHC.HsFirstOrderApp False) =
  spaced
    [ pretty $ mkExpression <$> fromGenLocated arg
    , string ">-"
    , pretty $ mkExpression <$> fromGenLocated f
    ]
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyHsCmd (GHC.HsCmdArrForm _ f _ args) =
  bananaBrackets
    $ spaced
    $ pretty (mkExpression <$> fromGenLocated f) : fmap pretty args
#else
prettyHsCmd (GHC.HsCmdArrForm _ f _ _ args) =
  bananaBrackets
    $ spaced
    $ pretty (mkExpression <$> fromGenLocated f) : fmap pretty args
#endif
prettyHsCmd (GHC.HsCmdApp _ f arg) =
  spaced [pretty f, pretty $ mkExpression <$> fromGenLocated arg]
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyHsCmd (GHC.HsCmdLam _ GHC.LamSingle x) = pretty $ mkCmdMatchGroup x
prettyHsCmd (GHC.HsCmdLam _ GHC.LamCase arms) = do
  string "\\case"
  newline
  indentedBlock $ pretty $ mkCmdMatchGroup arms
prettyHsCmd (GHC.HsCmdLam _ GHC.LamCases arms) = do
  string "\\cases"
  newline
  indentedBlock $ pretty $ mkCmdMatchGroup arms
#else
prettyHsCmd (GHC.HsCmdLam _ x) = pretty $ mkCmdMatchGroup x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyHsCmd (GHC.HsCmdPar _ _ x _) = parens $ pretty x
#else
prettyHsCmd (GHC.HsCmdPar _ x) = parens $ pretty x
#endif
prettyHsCmd (GHC.HsCmdCase _ cond arms) = do
  spaced
    [string "case", pretty $ mkExpression <$> fromGenLocated cond, string "of"]
  newline
  indentedBlock $ pretty $ mkCmdMatchGroup arms
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
-- No `HsCmdLamCase` since 9.10.1
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
prettyHsCmd (GHC.HsCmdLamCase _ _ arms) = do
  string "\\case"
  newline
  indentedBlock $ pretty $ mkCmdMatchGroup arms
#else
prettyHsCmd (GHC.HsCmdLamCase _ arms) = do
  string "\\case"
  newline
  indentedBlock $ pretty $ mkCmdMatchGroup arms
#endif
prettyHsCmd (GHC.HsCmdIf _ _ cond t f) = do
  string "if "
  pretty $ mkExpression <$> fromGenLocated cond
  newline
  indentedBlock $ lined [string "then " >> pretty t, string "else " >> pretty f]
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
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

instance Pretty DoOrMdo where
  pretty' Do = string "do"
  pretty' Mdo = string "mdo"

instance Pretty QualifiedDo where
  pretty' (QualifiedDo (Just m) d) = do
    pretty m
    string "."
    pretty d
  pretty' (QualifiedDo Nothing d) = pretty d
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
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
-- | Marks an AST node as it is used only for Haskell Program Coverage.
forHpc :: HasCallStack => a
forHpc = error "This AST type is for the use of Haskell Program Coverage."
#endif

#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
getAnc :: GHC.EpaLocation' a -> GHC.RealSrcSpan
getAnc (GHC.EpaSpan (GHC.RealSrcSpan x _)) = x
getAnc _ = undefined
#else
getAnc :: GHC.Anchor -> GHC.RealSrcSpan
getAnc = GHC.anchor
#endif
