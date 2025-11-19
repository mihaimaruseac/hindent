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
import qualified GHC.Data.FastString as GHC
import qualified GHC.Hs as GHC
import GHC.Stack
import qualified GHC.Types.SourceText as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Applicative (whenJust)
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
import HIndent.Ast.LocalBinds (mkLocalBinds)
import HIndent.Ast.Module.Name (mkModuleName)
import HIndent.Ast.Name.ImportExport
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.Pattern
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import qualified HIndent.Pretty.SigBindFamily as SBF
import HIndent.Pretty.Types
import HIndent.Printer
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
prettyStmtLRExpr (GHC.LetStmt _ l) =
  whenJust (mkLocalBinds l) $ \binds -> string "let " |=> pretty binds
prettyStmtLRExpr (GHC.ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
prettyStmtLRExpr GHC.TransStmt {..} =
  vCommaSep
    $ fmap pretty trS_stmts
        ++ [ string "then "
               >> pretty (mkExpression <$> fromGenLocated trS_using)
           ]
prettyStmtLRExpr GHC.RecStmt {..} =
  string "rec " |=> printCommentsAnd recS_stmts (lined . fmap pretty)

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

instance Pretty
           (GHC.HsScaled
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))) where
  pretty' (GHC.HsScaled _ ty) = pretty $ fmap mkType ty
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
