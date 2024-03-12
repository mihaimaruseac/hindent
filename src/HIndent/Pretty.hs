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
import GHC.Core.Coercion
import GHC.Core.InstEnv
import GHC.Data.Bag
import GHC.Data.BooleanFormula
import GHC.Data.FastString
import GHC.Hs
import GHC.Stack
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Unit.Module.Warnings
import HIndent.Applicative
import HIndent.Ast.NodeComments
import HIndent.Config
import HIndent.Fixity
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.SigBindFamily
import HIndent.Pretty.Types
import HIndent.Printer
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Text.Show.Unicode
#if MIN_VERSION_ghc_lib_parser(9,6,1)
import qualified Data.Foldable as NonEmpty
import GHC.Core.DataCon
#endif
#if !MIN_VERSION_ghc_lib_parser(9,6,1)
import GHC.Unit
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
     (CommentExtraction l) => GenLocated l e -> (e -> Printer ()) -> Printer ()
printCommentsAnd (L l e) f = do
  printCommentsBefore l
  f e
  printCommentOnSameLine l
  printCommentsAfter l

-- | Prints comments that are before the given AST node.
printCommentsBefore :: CommentExtraction a => a -> Printer ()
printCommentsBefore p =
  forM_ (commentsBefore $ nodeComments p) $ \(L loc c) -> do
    let col = fromIntegral $ srcSpanStartCol (anchor loc) - 1
    indentedWithFixedLevel col $ pretty c
    newline

-- | Prints comments that are on the same line as the given AST node.
printCommentOnSameLine :: CommentExtraction a => a -> Printer ()
printCommentOnSameLine (commentsOnSameLine . nodeComments -> (c:cs)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithFixedLevel
           (fromIntegral $ srcSpanStartCol $ anchor $ getLoc c)
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
      forM_ xs $ \(L loc c) -> do
        let col = fromIntegral $ srcSpanStartCol (anchor loc) - 1
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
instance (CommentExtraction l, Pretty e) => Pretty (GenLocated l e) where
  pretty' (L _ e) = pretty e

instance Pretty (GHC.Hs.HsDecl GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.TyClD _ d) = pretty d
  pretty' (GHC.Hs.InstD _ inst) = pretty inst
  pretty' (GHC.Hs.DerivD _ x) = pretty x
  pretty' (GHC.Hs.ValD _ bind) = pretty bind
  pretty' (GHC.Hs.SigD _ s) = pretty s
  pretty' (GHC.Hs.KindSigD _ x) = pretty x
  pretty' (GHC.Hs.DefD _ x) = pretty x
  pretty' (GHC.Hs.ForD _ x) = pretty x
  pretty' (GHC.Hs.WarningD _ x) = pretty x
  pretty' (GHC.Hs.AnnD _ x) = pretty x
  pretty' (GHC.Hs.RuleD _ x) = pretty x
  pretty' (GHC.Hs.SpliceD _ sp) = pretty sp
  pretty' GHC.Hs.DocD {} = docNode
  pretty' (GHC.Hs.RoleAnnotD _ x) = pretty x

instance Pretty (GHC.Hs.TyClDecl GHC.Hs.GhcPs) where
  pretty' = prettyTyClDecl

prettyTyClDecl :: GHC.Hs.TyClDecl GHC.Hs.GhcPs -> Printer ()
prettyTyClDecl (GHC.Hs.FamDecl _ x) = pretty x
prettyTyClDecl GHC.Hs.SynDecl {..} = do
  string "type "
  case tcdFixity of
    Prefix -> spaced $ pretty tcdLName : fmap pretty (hsq_explicit tcdTyVars)
    Infix ->
      case hsq_explicit tcdTyVars of
        (l:r:xs) -> do
          spaced [pretty l, pretty $ fmap InfixOp tcdLName, pretty r]
          forM_ xs $ \x -> do
            space
            pretty x
        _ -> error "Not enough parameters are given."
  hor <-|> ver
  where
    hor = string " = " >> pretty tcdRhs
    ver = newline >> indentedBlock (string "= " |=> pretty tcdRhs)
#if MIN_VERSION_ghc_lib_parser(9,6,1)
prettyTyClDecl DataDecl {..} = do
  printDataNewtype |=> do
    whenJust (dd_ctxt tcdDataDefn) $ \x -> do
      pretty $ Context x
      string " =>"
      newline
    pretty tcdLName
  spacePrefixed $ pretty <$> hsq_explicit tcdTyVars
  pretty tcdDataDefn
  where
    printDataNewtype =
      case dd_cons tcdDataDefn of
        DataTypeCons {} -> string "data "
        NewTypeCon {} -> string "newtype "
#elif MIN_VERSION_ghc_lib_parser(9,4,1)
prettyTyClDecl DataDecl {..} = do
  printDataNewtype |=> do
    whenJust (dd_ctxt tcdDataDefn) $ \x -> do
      pretty $ Context x
      string " =>"
      newline
    pretty tcdLName
  spacePrefixed $ pretty <$> hsq_explicit tcdTyVars
  pretty tcdDataDefn
  where
    printDataNewtype =
      case dd_ND tcdDataDefn of
        DataType -> string "data "
        NewType -> string "newtype "
#else
prettyTyClDecl GHC.Hs.DataDecl {..} = do
  printDataNewtype |=> do
    whenJust (dd_ctxt tcdDataDefn) $ \_ -> do
      pretty $ Context $ dd_ctxt tcdDataDefn
      string " =>"
      newline
    pretty tcdLName
  spacePrefixed $ pretty <$> hsq_explicit tcdTyVars
  pretty tcdDataDefn
  where
    printDataNewtype =
      case dd_ND tcdDataDefn of
        GHC.Hs.DataType -> string "data "
        GHC.Hs.NewType -> string "newtype "
#endif
prettyTyClDecl GHC.Hs.ClassDecl {..} = do
  if isJust tcdCtxt
    then verHead
    else horHead <-|> verHead
  indentedBlock $ newlinePrefixed $ fmap pretty sigsMethodsFamilies
  where
    horHead = do
      string "class "
      printNameAndTypeVariables
      unless (null tcdFDs) $ do
        string " | "
        forM_ tcdFDs $ \x@(L _ GHC.Hs.FunDep {}) ->
          printCommentsAnd x $ \(GHC.Hs.FunDep _ from to) ->
            spaced $ fmap pretty from ++ [string "->"] ++ fmap pretty to
      unless (null sigsMethodsFamilies) $ string " where"
    verHead = do
      string "class " |=> do
        whenJust tcdCtxt $ \ctx -> do
          printCommentsAnd ctx $ \case
            [] -> string "()"
            [x] -> pretty x
            xs -> hvTuple $ fmap pretty xs
          string " =>"
          newline
        printNameAndTypeVariables
      unless (null tcdFDs) $ do
        newline
        indentedBlock
          $ string "| "
              |=> vCommaSep
                    (flip fmap tcdFDs $ \x@(L _ GHC.Hs.FunDep {}) ->
                       printCommentsAnd x $ \(GHC.Hs.FunDep _ from to) ->
                         spaced
                           $ fmap pretty from ++ [string "->"] ++ fmap pretty to)
      unless (null sigsMethodsFamilies) $ do
        newline
        indentedBlock $ string "where"
    printNameAndTypeVariables =
      case tcdFixity of
        Prefix ->
          spaced $ pretty tcdLName : fmap pretty (hsq_explicit tcdTyVars)
        Infix ->
          case hsq_explicit tcdTyVars of
            (l:r:xs) -> do
              parens
                $ spaced [pretty l, pretty $ fmap InfixOp tcdLName, pretty r]
              spacePrefixed $ fmap pretty xs
            _ -> error "Not enough parameters are given."
    sigsMethodsFamilies =
      mkSortedLSigBindFamilyList tcdSigs (bagToList tcdMeths) tcdATs [] []

instance Pretty (GHC.Hs.InstDecl GHC.Hs.GhcPs) where
  pretty' GHC.Hs.ClsInstD {..} = pretty cid_inst
  pretty' GHC.Hs.DataFamInstD {..} = pretty dfid_inst
  pretty' GHC.Hs.TyFamInstD {..} = pretty $ TopLevelTyFamInstDecl tfid_inst

instance Pretty (GHC.Hs.HsBind GHC.Hs.GhcPs) where
  pretty' = prettyHsBind

prettyHsBind :: GHC.Hs.HsBind GHC.Hs.GhcPs -> Printer ()
prettyHsBind GHC.Hs.FunBind {..} = pretty fun_matches
prettyHsBind GHC.Hs.PatBind {..} = pretty pat_lhs >> pretty pat_rhs
prettyHsBind GHC.Hs.VarBind {} = notGeneratedByParser
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsBind GHC.Hs.AbsBinds {} = notGeneratedByParser
#endif
prettyHsBind (GHC.Hs.PatSynBind _ x) = pretty x
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (Sig GhcPs) where
  pretty' (TypeSig _ funName params) = do
    printFunName
    string " ::"
    horizontal <-|> vertical
    where
      horizontal = do
        space
        pretty $ HsSigTypeInsideDeclSig <$> hswc_body params
      vertical = do
        headLen <- printerLength printFunName
        indentSpaces <- getIndentSpaces
        if headLen < indentSpaces
          then space |=> pretty (HsSigTypeInsideDeclSig <$> hswc_body params)
          else do
            newline
            indentedBlock
              $ indentedWithSpace 3
              $ pretty
              $ HsSigTypeInsideDeclSig <$> hswc_body params
      printFunName = hCommaSep $ fmap pretty funName
  pretty' (PatSynSig _ names sig) =
    spaced
      [string "pattern", hCommaSep $ fmap pretty names, string "::", pretty sig]
  pretty' (ClassOpSig _ True funNames params) =
    spaced
      [ string "default"
      , hCommaSep $ fmap pretty funNames
      , string "::"
      , printCommentsAnd params pretty
      ]
  pretty' (ClassOpSig _ False funNames params) = do
    hCommaSep $ fmap pretty funNames
    string " ::"
    hor <-|> ver
    where
      hor = space >> printCommentsAnd params (pretty . HsSigTypeInsideDeclSig)
      ver = do
        newline
        indentedBlock
          $ indentedWithSpace 3
          $ printCommentsAnd params (pretty . HsSigTypeInsideDeclSig)
  pretty' (FixSig _ x) = pretty x
  pretty' (InlineSig _ name detail) =
    spaced [string "{-#", pretty detail, pretty name, string "#-}"]
  pretty' (SpecSig _ name sigs _) =
    spaced
      [ string "{-# SPECIALISE"
      , pretty name
      , string "::"
      , hCommaSep $ fmap pretty sigs
      , string "#-}"
      ]
  pretty' (SpecInstSig _ sig) =
    spaced [string "{-# SPECIALISE instance", pretty sig, string "#-}"]
  pretty' (MinimalSig _ xs) =
    string "{-# MINIMAL " |=> do
      pretty xs
      string " #-}"
  pretty' (SCCFunSig _ name _) =
    spaced [string "{-# SCC", pretty name, string "#-}"]
  pretty' (CompleteMatchSig _ names _) =
    spaced
      [ string "{-# COMPLETE"
      , printCommentsAnd names (hCommaSep . fmap pretty)
      , string "#-}"
      ]
#else
instance Pretty (GHC.Hs.Sig GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.TypeSig _ funName params) = do
    printFunName
    string " ::"
    horizontal <-|> vertical
    where
      horizontal = do
        space
        pretty $ HsSigTypeInsideDeclSig <$> hswc_body params
      vertical = do
        headLen <- printerLength printFunName
        indentSpaces <- getIndentSpaces
        if headLen < indentSpaces
          then space |=> pretty (HsSigTypeInsideDeclSig <$> hswc_body params)
          else do
            newline
            indentedBlock
              $ indentedWithSpace 3
              $ pretty
              $ HsSigTypeInsideDeclSig <$> hswc_body params
      printFunName = hCommaSep $ fmap pretty funName
  pretty' (GHC.Hs.PatSynSig _ names sig) =
    spaced
      [string "pattern", hCommaSep $ fmap pretty names, string "::", pretty sig]
  pretty' (GHC.Hs.ClassOpSig _ True funNames params) =
    spaced
      [ string "default"
      , hCommaSep $ fmap pretty funNames
      , string "::"
      , printCommentsAnd params pretty
      ]
  pretty' (GHC.Hs.ClassOpSig _ False funNames params) = do
    hCommaSep $ fmap pretty funNames
    string " ::"
    hor <-|> ver
    where
      hor = space >> printCommentsAnd params (pretty . HsSigTypeInsideDeclSig)
      ver = do
        newline
        indentedBlock
          $ indentedWithSpace 3
          $ printCommentsAnd params (pretty . HsSigTypeInsideDeclSig)
  pretty' GHC.Hs.IdSig {} = notGeneratedByParser
  pretty' (GHC.Hs.FixSig _ x) = pretty x
  pretty' (GHC.Hs.InlineSig _ name detail) =
    spaced [string "{-#", pretty detail, pretty name, string "#-}"]
  pretty' (GHC.Hs.SpecSig _ name sigs _) =
    spaced
      [ string "{-# SPECIALISE"
      , pretty name
      , string "::"
      , hCommaSep $ fmap pretty sigs
      , string "#-}"
      ]
  pretty' (GHC.Hs.SpecInstSig _ _ sig) =
    spaced [string "{-# SPECIALISE instance", pretty sig, string "#-}"]
  pretty' (GHC.Hs.MinimalSig _ _ xs) =
    string "{-# MINIMAL " |=> do
      pretty xs
      string " #-}"
  pretty' (GHC.Hs.SCCFunSig _ _ name _) =
    spaced [string "{-# SCC", pretty name, string "#-}"]
  pretty' (GHC.Hs.CompleteMatchSig _ _ names _) =
    spaced
      [ string "{-# COMPLETE"
      , printCommentsAnd names (hCommaSep . fmap pretty)
      , string "#-}"
      ]
#endif
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (HsDataDefn GhcPs) where
  pretty' HsDataDefn {..} =
    if isGADT
      then do
        whenJust dd_kindSig $ \x -> do
          string " :: "
          pretty x
        string " where"
        indentedBlock $ newlinePrefixed $ fmap pretty cons
      else do
        case cons of
          [] -> indentedBlock derivingsAfterNewline
          [x@(L _ ConDeclH98 {con_args = RecCon {}})] -> do
            string " = "
            pretty x
            unless (null dd_derivs) $ space |=> printDerivings
          [x] -> do
            string " ="
            newline
            indentedBlock $ do
              pretty x
              derivingsAfterNewline
          _ ->
            indentedBlock $ do
              newline
              string "= " |=> vBarSep (fmap pretty cons)
              derivingsAfterNewline
    where
      cons =
        case dd_cons of
          NewTypeCon x -> [x]
          DataTypeCons _ xs -> xs
      isGADT =
        case dd_cons of
          (DataTypeCons _ (L _ ConDeclGADT {}:_)) -> True
          _ -> False
      derivingsAfterNewline =
        unless (null dd_derivs) $ newline >> printDerivings
      printDerivings = lined $ fmap pretty dd_derivs
#else
instance Pretty (GHC.Hs.HsDataDefn GHC.Hs.GhcPs) where
  pretty' GHC.Hs.HsDataDefn {..} =
    if isGADT
      then do
        whenJust dd_kindSig $ \x -> do
          string " :: "
          pretty x
        string " where"
        indentedBlock $ newlinePrefixed $ fmap pretty dd_cons
      else do
        case dd_cons of
          [] -> indentedBlock derivingsAfterNewline
          [x@(L _ GHC.Hs.ConDeclH98 {con_args = GHC.Hs.RecCon {}})] -> do
            string " = "
            pretty x
            unless (null dd_derivs) $ space |=> printDerivings
          [x] -> do
            string " ="
            newline
            indentedBlock $ do
              pretty x
              derivingsAfterNewline
          _ ->
            indentedBlock $ do
              newline
              string "= " |=> vBarSep (fmap pretty dd_cons)
              derivingsAfterNewline
    where
      isGADT =
        case dd_cons of
          (L _ GHC.Hs.ConDeclGADT {}:_) -> True
          _ -> False
      derivingsAfterNewline =
        unless (null dd_derivs) $ newline >> printDerivings
      printDerivings = lined $ fmap pretty dd_derivs
#endif
instance Pretty (GHC.Hs.ClsInstDecl GHC.Hs.GhcPs) where
  pretty' GHC.Hs.ClsInstDecl {..} = do
    string "instance " |=> do
      whenJust cid_overlap_mode $ \x -> do
        pretty x
        space
      pretty (fmap HsSigTypeInsideInstDecl cid_poly_ty)
        |=> unless (null sigsAndMethods) (string " where")
    unless (null sigsAndMethods) $ do
      newline
      indentedBlock $ lined $ fmap pretty sigsAndMethods
    where
      sigsAndMethods =
        mkSortedLSigBindFamilyList
          cid_sigs
          (bagToList cid_binds)
          []
          cid_tyfam_insts
          cid_datafam_insts

instance Pretty
           (GHC.Hs.MatchGroup
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsExpr GHC.Hs.GhcPs))) where
  pretty' GHC.Hs.MG {..} = printCommentsAnd mg_alts (lined . fmap pretty)

instance Pretty
           (GHC.Hs.MatchGroup
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsCmd GHC.Hs.GhcPs))) where
  pretty' GHC.Hs.MG {..} = printCommentsAnd mg_alts (lined . fmap pretty)

instance Pretty (GHC.Hs.HsExpr GHC.Hs.GhcPs) where
  pretty' = prettyHsExpr

prettyHsExpr :: GHC.Hs.HsExpr GHC.Hs.GhcPs -> Printer ()
prettyHsExpr (GHC.Hs.HsVar _ bind) = pretty $ fmap PrefixOp bind
prettyHsExpr (GHC.Hs.HsUnboundVar _ x) = pretty x
#if MIN_VERSION_ghc_lib_parser(9,6,1)
prettyHsExpr (HsOverLabel _ _ l) = string "#" >> string (unpackFS l)
#else
prettyHsExpr (GHC.Hs.HsOverLabel _ l) = string "#" >> string (unpackFS l)
#endif
prettyHsExpr (GHC.Hs.HsIPVar _ var) = string "?" >> pretty var
prettyHsExpr (GHC.Hs.HsOverLit _ x) = pretty x
prettyHsExpr (GHC.Hs.HsLit _ l) = pretty l
prettyHsExpr (GHC.Hs.HsLam _ body) = pretty body
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (HsLamCase _ LamCase matches) = pretty $ LambdaCase matches Case
prettyHsExpr (HsLamCase _ LamCases matches) = pretty $ LambdaCase matches Cases
#else
prettyHsExpr (GHC.Hs.HsLamCase _ matches) = pretty $ LambdaCase matches Case
#endif
prettyHsExpr (GHC.Hs.HsApp _ l r) = horizontal <-|> vertical
  where
    horizontal = spaced [pretty l, pretty r]
    vertical = do
      let (f, args) =
            case flatten l ++ [r] of
              [] -> error "Invalid function application."
              (f':args') -> (f', args')
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
    flatten :: GHC.Hs.LHsExpr GHC.Hs.GhcPs -> [GHC.Hs.LHsExpr GHC.Hs.GhcPs]
    flatten (L (GHC.Hs.SrcSpanAnn (GHC.Hs.EpAnn _ _ cs) _) (GHC.Hs.HsApp _ l' r')) =
      flatten l' ++ [insertComments cs r']
    flatten x = [x]
    insertComments ::
         GHC.Hs.EpAnnComments
      -> GHC.Hs.LHsExpr GHC.Hs.GhcPs
      -> GHC.Hs.LHsExpr GHC.Hs.GhcPs
    insertComments cs (L s@GHC.Hs.SrcSpanAnn {ann = e@GHC.Hs.EpAnn {comments = cs'}} r') =
      L (s {ann = e {comments = cs <> cs'}}) r'
    insertComments _ x = x
#if MIN_VERSION_ghc_lib_parser(9,6,1)
prettyHsExpr (HsAppType _ l _ r) = do
  pretty l
  string " @"
  pretty r
#else
prettyHsExpr (GHC.Hs.HsAppType _ l r) = do
  pretty l
  string " @"
  pretty r
#endif
prettyHsExpr (GHC.Hs.OpApp _ l o r) = pretty (InfixApp l o r)
prettyHsExpr (GHC.Hs.NegApp _ x _) = string "-" >> pretty x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (HsPar _ _ expr _) = parens $ pretty expr
#else
prettyHsExpr (GHC.Hs.HsPar _ expr) = parens $ pretty expr
#endif
prettyHsExpr (GHC.Hs.SectionL _ l o) = spaced [pretty l, pretty (InfixExpr o)]
prettyHsExpr (GHC.Hs.SectionR _ o r) =
  (pretty (InfixExpr o) >> space) |=> pretty r
prettyHsExpr (GHC.Hs.ExplicitTuple _ full _) = horizontal <-|> vertical
  where
    horizontal = hTuple $ fmap pretty full
    vertical =
      parens
        $ prefixedLined ","
        $ fmap (\e -> unless (isMissing e) (space |=> pretty e)) full
    isMissing GHC.Hs.Missing {} = True
    isMissing _ = False
prettyHsExpr (GHC.Hs.ExplicitSum _ position numElem expr) = do
  string "(#"
  forM_ [1 .. numElem] $ \idx -> do
    if idx == position
      then string " " >> pretty expr >> string " "
      else string " "
    when (idx < numElem) $ string "|"
  string "#)"
prettyHsExpr (GHC.Hs.HsCase _ cond arms) = do
  string "case " |=> do
    pretty cond
    string " of"
  if null $ unLoc $ mg_alts arms
    then string " {}"
    else do
      newline
      indentedBlock $ pretty arms
prettyHsExpr (GHC.Hs.HsIf _ cond t f) = do
  string "if " |=> pretty cond
  indentedBlock $ newlinePrefixed [branch "then " t, branch "else " f]
  where
    branch :: String -> GHC.Hs.LHsExpr GHC.Hs.GhcPs -> Printer ()
    branch str e =
      case e of
        (L _ (GHC.Hs.HsDo _ (GHC.Hs.DoExpr m) xs)) ->
          doStmt (QualifiedDo m Do) xs
        (L _ (GHC.Hs.HsDo _ (GHC.Hs.MDoExpr m) xs)) ->
          doStmt (QualifiedDo m Mdo) xs
        _ -> string str |=> pretty e
      where
        doStmt qDo stmts = do
          string str
          pretty qDo
          newline
          indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
prettyHsExpr (GHC.Hs.HsMultiIf _ guards) =
  string "if "
    |=> lined (fmap (pretty . fmap (GRHSExpr GRHSExprMultiWayIf)) guards)
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (HsLet _ _ binds _ exprs) = pretty $ LetIn binds exprs
#else
prettyHsExpr (GHC.Hs.HsLet _ binds exprs) = pretty $ LetIn binds exprs
#endif
prettyHsExpr (GHC.Hs.HsDo _ GHC.Hs.ListComp {} (L _ [])) =
  error "Not enough arguments are passed to pretty-print a list comprehension."
prettyHsExpr (GHC.Hs.HsDo _ GHC.Hs.ListComp {} (L l (lhs:rhs))) =
  pretty $ L l $ ListComprehension lhs rhs
-- While the name contains 'Monad', 'MonadComp' is for list comprehensions.
prettyHsExpr (GHC.Hs.HsDo _ GHC.Hs.MonadComp {} (L _ [])) =
  error "Not enough arguments are passed to pretty-print a list comprehension."
prettyHsExpr (GHC.Hs.HsDo _ GHC.Hs.MonadComp {} (L l (lhs:rhs))) =
  pretty $ L l $ ListComprehension lhs rhs
prettyHsExpr (GHC.Hs.HsDo _ (GHC.Hs.DoExpr m) (L l xs)) =
  pretty $ L l $ DoExpression xs (QualifiedDo m Do)
prettyHsExpr (GHC.Hs.HsDo _ (GHC.Hs.MDoExpr m) (L l xs)) =
  pretty $ L l $ DoExpression xs (QualifiedDo m Mdo)
prettyHsExpr (GHC.Hs.HsDo _ GHC.Hs.GhciStmtCtxt {} _) =
  error "We're not using GHCi, are we?"
prettyHsExpr (GHC.Hs.ExplicitList _ xs) = horizontal <-|> vertical
  where
    horizontal = brackets $ hCommaSep $ fmap pretty xs
    vertical = vList $ fmap pretty xs
prettyHsExpr (GHC.Hs.RecordCon _ name fields) = horizontal <-|> vertical
  where
    horizontal = spaced [pretty name, pretty fields]
    vertical = do
      pretty name
      (space >> pretty fields) <-|> (newline >> indentedBlock (pretty fields))
#if MIN_VERSION_ghc_lib_parser(9,8,1)
prettyHsExpr (RecordUpd _ name fields) = hor <-|> ver
  where
    hor = spaced [pretty name, printHorFields fields]
    ver = do
      pretty name
      newline
      indentedBlock $ printHorFields fields <-|> printVerFields fields
    printHorFields RegularRecUpdFields {..} =
      hFields $ fmap (`printCommentsAnd` horField) recUpdFields
    printHorFields OverloadedRecUpdFields {..} =
      hFields $ fmap (`printCommentsAnd` horField) olRecUpdFields
    printVerFields RegularRecUpdFields {..} =
      vFields $ fmap printField recUpdFields
    printVerFields OverloadedRecUpdFields {..} =
      vFields $ fmap printField olRecUpdFields
    printField x = printCommentsAnd x $ (<-|>) <$> horField <*> verField
    horField HsFieldBind {..} = do
      pretty hfbLHS
      string " = "
      pretty hfbRHS
    verField HsFieldBind {..} = do
      pretty hfbLHS
      string " ="
      newline
      indentedBlock $ pretty hfbRHS
#elif MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (RecordUpd _ name fields) = hor <-|> ver
  where
    hor = spaced [pretty name, either printHorFields printHorFields fields]
    ver = do
      pretty name
      newline
      indentedBlock
        $ either printHorFields printHorFields fields
            <-|> either printVerFields printVerFields fields
    printHorFields ::
         (Pretty a, Pretty b, CommentExtraction l)
      => [GenLocated l (HsFieldBind a b)]
      -> Printer ()
    printHorFields = hFields . fmap (`printCommentsAnd` horField)
    printVerFields ::
         (Pretty a, Pretty b, CommentExtraction l)
      => [GenLocated l (HsFieldBind a b)]
      -> Printer ()
    printVerFields = vFields . fmap printField
    printField x = printCommentsAnd x $ (<-|>) <$> horField <*> verField
    horField HsFieldBind {..} = do
      pretty hfbLHS
      string " = "
      pretty hfbRHS
    verField HsFieldBind {..} = do
      pretty hfbLHS
      string " ="
      newline
      indentedBlock $ pretty hfbRHS
#else
prettyHsExpr (GHC.Hs.RecordUpd _ name fields) = hor <-|> ver
  where
    hor = spaced [pretty name, either printHorFields printHorFields fields]
    ver = do
      pretty name
      newline
      indentedBlock
        $ either printHorFields printHorFields fields
            <-|> either printVerFields printVerFields fields
    printHorFields ::
         (Pretty a, Pretty b, CommentExtraction l)
      => [GenLocated l (GHC.Hs.HsRecField' a b)]
      -> Printer ()
    printHorFields = hFields . fmap (`printCommentsAnd` horField)
    printVerFields ::
         (Pretty a, Pretty b, CommentExtraction l)
      => [GenLocated l (GHC.Hs.HsRecField' a b)]
      -> Printer ()
    printVerFields = vFields . fmap printField
    printField x = printCommentsAnd x $ (<-|>) <$> horField <*> verField
    horField GHC.Hs.HsRecField {..} = do
      pretty hsRecFieldLbl
      string " = "
      pretty hsRecFieldArg
    verField GHC.Hs.HsRecField {..} = do
      pretty hsRecFieldLbl
      string " ="
      newline
      indentedBlock $ pretty hsRecFieldArg
#endif
prettyHsExpr (GHC.Hs.HsGetField _ e f) = do
  pretty e
  dot
  pretty f
prettyHsExpr GHC.Hs.HsProjection {..} =
  parens
    $ forM_ proj_flds
    $ \x -> do
        string "."
        pretty x
prettyHsExpr (GHC.Hs.ExprWithTySig _ e sig) = do
  pretty e
  string " :: "
  pretty $ hswc_body sig
prettyHsExpr (GHC.Hs.ArithSeq _ _ x) = pretty x
#if !MIN_VERSION_ghc_lib_parser(9,6,1)
prettyHsExpr (GHC.Hs.HsSpliceE _ x) = pretty x
#endif
prettyHsExpr (GHC.Hs.HsProc _ pat x@(L _ (GHC.Hs.HsCmdTop _ (L _ (GHC.Hs.HsCmdDo _ xs))))) = do
  spaced [string "proc", pretty pat, string "-> do"]
  newline
  indentedBlock
    $ printCommentsAnd x (const (printCommentsAnd xs (lined . fmap pretty)))
prettyHsExpr (GHC.Hs.HsProc _ pat body) = hor <-|> ver
  where
    hor = spaced [string "proc", pretty pat, string "->", pretty body]
    ver = do
      spaced [string "proc", pretty pat, string "->"]
      newline
      indentedBlock (pretty body)
prettyHsExpr (GHC.Hs.HsStatic _ x) = spaced [string "static", pretty x]
prettyHsExpr (GHC.Hs.HsPragE _ p x) = spaced [pretty p, pretty x]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr HsRecSel {} = notGeneratedByParser
prettyHsExpr (HsTypedBracket _ inner) = typedBrackets $ pretty inner
prettyHsExpr (HsUntypedBracket _ inner) = pretty inner
#else
prettyHsExpr GHC.Hs.HsConLikeOut {} = notGeneratedByParser
prettyHsExpr GHC.Hs.HsRecFld {} = notGeneratedByParser
prettyHsExpr (GHC.Hs.HsDo _ GHC.Hs.ArrowExpr {} _) = notGeneratedByParser
prettyHsExpr (GHC.Hs.HsDo _ GHC.Hs.PatGuard {} _) = notGeneratedByParser
prettyHsExpr (GHC.Hs.HsDo _ GHC.Hs.ParStmtCtxt {} _) = notGeneratedByParser
prettyHsExpr (GHC.Hs.HsDo _ GHC.Hs.TransStmtCtxt {} _) = notGeneratedByParser
prettyHsExpr GHC.Hs.HsTick {} = forHpc
prettyHsExpr GHC.Hs.HsBinTick {} = forHpc
prettyHsExpr (GHC.Hs.HsBracket _ inner) = pretty inner
prettyHsExpr GHC.Hs.HsRnBracketOut {} = notGeneratedByParser
prettyHsExpr GHC.Hs.HsTcBracketOut {} = notGeneratedByParser
#endif
#if MIN_VERSION_ghc_lib_parser(9,6,1)
prettyHsExpr (HsTypedSplice _ x) = string "$$" >> pretty x
prettyHsExpr (HsUntypedSplice _ x) = pretty x
#endif
instance Pretty LambdaCase where
  pretty' (LambdaCase matches caseOrCases) = do
    case caseOrCases of
      Case -> string "\\case"
      Cases -> string "\\cases"
    if null $ unLoc $ mg_alts matches
      then string " {}"
      else do
        newline
        indentedBlock $ pretty matches

instance Pretty (GHC.Hs.HsSigType GHC.Hs.GhcPs) where
  pretty' = pretty' . HsSigType' HsTypeForNormalDecl HsTypeNoDir

instance Pretty HsSigType' where
  pretty' (HsSigTypeInsideDeclSig GHC.Hs.HsSig {..}) =
    case sig_bndrs of
      GHC.Hs.HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        case unLoc sig_body of
          GHC.Hs.HsQualTy {..} ->
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
      flatten :: GHC.Hs.LHsType GHC.Hs.GhcPs -> [GHC.Hs.LHsType GHC.Hs.GhcPs]
      flatten (L _ (GHC.Hs.HsFunTy _ _ l r)) = flatten l ++ flatten r
      flatten x = [x]
  pretty' (HsSigTypeInsideVerticalFuncSig GHC.Hs.HsSig {..}) =
    case sig_bndrs of
      GHC.Hs.HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        printCommentsAnd sig_body $ \case
          GHC.Hs.HsQualTy {..} -> do
            (space >> pretty (HorizontalContext hst_ctxt))
              <-|> (newline >> pretty (VerticalContext hst_ctxt))
            newline
            prefixed "=> " $ pretty hst_body
          x -> pretty $ HsTypeInsideDeclSig x
      _ -> pretty $ fmap HsTypeInsideDeclSig sig_body
  pretty' (HsSigType' for dir GHC.Hs.HsSig {..}) = do
    case sig_bndrs of
      GHC.Hs.HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        space
      _ -> return ()
    pretty $ HsType' for dir <$> sig_body

instance Pretty (GHC.Hs.ConDecl GHC.Hs.GhcPs) where
  pretty' = prettyConDecl

prettyConDecl :: GHC.Hs.ConDecl GHC.Hs.GhcPs -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9,6,1)
prettyConDecl ConDeclGADT {..} = do
  hCommaSep $ fmap pretty $ NonEmpty.toList con_names
  hor <-|> ver
  where
    hor = string " :: " |=> body
    ver = do
      newline
      indentedBlock (string ":: " |=> body)
    body =
      case (forallNeeded, con_mb_cxt) of
        (True, Just ctx) -> withForallCtx ctx
        (True, Nothing) -> withForallOnly
        (False, Just ctx) -> withCtxOnly ctx
        (False, Nothing) -> noForallCtx
    withForallOnly = do
      pretty con_bndrs
      (space >> horArgs) <-|> (newline >> verArgs)
    noForallCtx = horArgs <-|> verArgs
    withForallCtx ctx = do
      pretty con_bndrs
      (space >> pretty (Context ctx)) <-|> (newline >> pretty (Context ctx))
      newline
      prefixed "=> " verArgs
    withCtxOnly ctx =
      (pretty (Context ctx) >> string " => " >> horArgs)
        <-|> (pretty (Context ctx) >> prefixed "=> " verArgs)
    horArgs =
      case con_g_args of
        PrefixConGADT xs ->
          inter (string " -> ")
            $ fmap (\(HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        RecConGADT xs _ -> inter (string " -> ") [recArg xs, pretty con_res_ty]
    verArgs =
      case con_g_args of
        PrefixConGADT xs ->
          prefixedLined "-> "
            $ fmap (\(HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        RecConGADT xs _ -> prefixedLined "-> " [recArg xs, pretty con_res_ty]
    recArg xs = printCommentsAnd xs $ \xs' -> vFields' $ fmap pretty xs'
    forallNeeded =
      case unLoc con_bndrs of
        HsOuterImplicit {} -> False
        HsOuterExplicit {} -> True
#else
prettyConDecl GHC.Hs.ConDeclGADT {..} = do
  hCommaSep $ fmap pretty con_names
  hor <-|> ver
  where
    hor = string " :: " |=> body
    ver = do
      newline
      indentedBlock (string ":: " |=> body)
    body =
      case (forallNeeded, con_mb_cxt) of
        (True, Just ctx) -> withForallCtx ctx
        (True, Nothing) -> withForallOnly
        (False, Just ctx) -> withCtxOnly ctx
        (False, Nothing) -> noForallCtx
    withForallOnly = do
      pretty con_bndrs
      (space >> horArgs) <-|> (newline >> verArgs)
    noForallCtx = horArgs <-|> verArgs
#if MIN_VERSION_ghc_lib_parser(9,4,1)
    withForallCtx ctx = do
      pretty con_bndrs
      (space >> pretty (Context ctx)) <-|> (newline >> pretty (Context ctx))
      newline
      prefixed "=> " verArgs
    
    withCtxOnly ctx =
      (pretty (Context ctx) >> string " => " >> horArgs)
        <-|> (pretty (Context ctx) >> prefixed "=> " verArgs)
    
    horArgs =
      case con_g_args of
        PrefixConGADT xs ->
          inter (string " -> ")
            $ fmap (\(HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        RecConGADT xs _ -> inter (string " -> ") [recArg xs, pretty con_res_ty]
    
    verArgs =
      case con_g_args of
        PrefixConGADT xs ->
          prefixedLined "-> "
            $ fmap (\(HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        RecConGADT xs _ -> prefixedLined "-> " [recArg xs, pretty con_res_ty]
#else
    withForallCtx _ = do
      pretty con_bndrs
      (space >> pretty (Context con_mb_cxt))
        <-|> (newline >> pretty (Context con_mb_cxt))
      newline
      prefixed "=> " verArgs
    
    withCtxOnly _ =
      (pretty (Context con_mb_cxt) >> string " => " >> horArgs)
        <-|> (pretty (Context con_mb_cxt) >> prefixed "=> " verArgs)
    
    horArgs =
      case con_g_args of
        GHC.Hs.PrefixConGADT xs ->
          inter (string " -> ")
            $ fmap (\(GHC.Hs.HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        GHC.Hs.RecConGADT xs -> inter (string " -> ") [recArg xs, pretty con_res_ty]
    
    verArgs =
      case con_g_args of
        GHC.Hs.PrefixConGADT xs ->
          prefixedLined "-> "
            $ fmap (\(GHC.Hs.HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        GHC.Hs.RecConGADT xs -> prefixedLined "-> " [recArg xs, pretty con_res_ty]
#endif
    recArg xs = printCommentsAnd xs $ \xs' -> vFields' $ fmap pretty xs'
    
    forallNeeded =
      case unLoc con_bndrs of
        GHC.Hs.HsOuterImplicit {} -> False
        GHC.Hs.HsOuterExplicit {} -> True
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyConDecl ConDeclH98 {con_forall = True, ..} =
  (do
     string "forall "
     spaced $ fmap pretty con_ex_tvs
     string ". ")
    |=> (do
           whenJust con_mb_cxt $ \c -> do
             pretty $ Context c
             string " =>"
             newline
           pretty con_name
           pretty con_args)
#else
prettyConDecl GHC.Hs.ConDeclH98 {con_forall = True, ..} =
  (do
     string "forall "
     spaced $ fmap pretty con_ex_tvs
     string ". ")
    |=> (do
           whenJust con_mb_cxt $ \_ -> do
             pretty $ Context con_mb_cxt
             string " =>"
             newline
           pretty con_name
           pretty con_args)
#endif
prettyConDecl GHC.Hs.ConDeclH98 {con_forall = False, ..} =
  case con_args of
    (GHC.Hs.InfixCon l r) ->
      spaced [pretty l, pretty $ fmap InfixOp con_name, pretty r]
    _ -> do
      pretty con_name
      pretty con_args

instance Pretty
           (GHC.Hs.Match
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsExpr GHC.Hs.GhcPs))) where
  pretty' = prettyMatchExpr

prettyMatchExpr ::
     GHC.Hs.Match GHC.Hs.GhcPs (GHC.Hs.LHsExpr GHC.Hs.GhcPs) -> Printer ()
prettyMatchExpr GHC.Hs.Match {m_ctxt = GHC.Hs.LambdaExpr, ..} = do
  string "\\"
  unless (null m_pats)
    $ case unLoc $ head m_pats of
        GHC.Hs.LazyPat {} -> space
        GHC.Hs.BangPat {} -> space
        _ -> return ()
  spaced $ fmap pretty m_pats
  pretty $ GRHSsExpr GRHSExprLambda m_grhss
prettyMatchExpr GHC.Hs.Match {m_ctxt = GHC.Hs.CaseAlt, ..} = do
  mapM_ pretty m_pats
  pretty $ GRHSsExpr GRHSExprCase m_grhss
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyMatchExpr Match {m_ctxt = LamCaseAlt {}, ..} = do
  spaced $ fmap pretty m_pats
  pretty $ GRHSsExpr GRHSExprCase m_grhss
#endif
prettyMatchExpr GHC.Hs.Match {..} =
  case mc_fixity m_ctxt of
    Prefix -> do
      pretty m_ctxt
      spacePrefixed $ fmap pretty m_pats
      pretty m_grhss
    Infix -> do
      case (m_pats, m_ctxt) of
        (l:r:xs, GHC.Hs.FunRhs {..}) -> do
          spaced
            $ [pretty l, pretty $ fmap InfixOp mc_fun, pretty r]
                ++ fmap pretty xs
          pretty m_grhss
        _ -> error "Not enough parameters are passed."

instance Pretty
           (GHC.Hs.Match
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsCmd GHC.Hs.GhcPs))) where
  pretty' = prettyMatchProc

prettyMatchProc ::
     GHC.Hs.Match GHC.Hs.GhcPs (GHC.Hs.LHsCmd GHC.Hs.GhcPs) -> Printer ()
prettyMatchProc GHC.Hs.Match {m_ctxt = GHC.Hs.LambdaExpr, ..} = do
  string "\\"
  unless (null m_pats)
    $ case unLoc $ head m_pats of
        GHC.Hs.LazyPat {} -> space
        GHC.Hs.BangPat {} -> space
        _ -> return ()
  spaced $ fmap pretty m_pats ++ [pretty m_grhss]
prettyMatchProc GHC.Hs.Match {m_ctxt = GHC.Hs.CaseAlt, ..} =
  spaced [mapM_ pretty m_pats, pretty m_grhss]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyMatchProc Match {m_ctxt = LamCaseAlt {}, ..} = do
  spaced [mapM_ pretty m_pats, pretty m_grhss]
#endif
prettyMatchProc _ = notGeneratedByParser

instance Pretty
           (GHC.Hs.StmtLR
              GHC.Hs.GhcPs
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsExpr GHC.Hs.GhcPs))) where
  pretty' (GHC.Hs.LastStmt _ x _ _) = pretty x
  pretty' (GHC.Hs.BindStmt _ pat body) = do
    pretty pat
    string " <-"
    hor <-|> ver
    where
      hor = space >> pretty body
      ver = newline >> indentedBlock (pretty body)
  pretty' GHC.Hs.ApplicativeStmt {} = notGeneratedByParser
  pretty' (GHC.Hs.BodyStmt _ (L loc (GHC.Hs.OpApp _ l o r)) _ _) =
    pretty (L loc (InfixApp l o r))
  pretty' (GHC.Hs.BodyStmt _ body _ _) = pretty body
  pretty' (GHC.Hs.LetStmt _ l) = string "let " |=> pretty l
  pretty' (GHC.Hs.ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
  pretty' GHC.Hs.TransStmt {..} =
    vCommaSep $ fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
  pretty' GHC.Hs.RecStmt {..} =
    string "rec " |=> printCommentsAnd recS_stmts (lined . fmap pretty)

instance Pretty
           (GHC.Hs.StmtLR
              GHC.Hs.GhcPs
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsCmd GHC.Hs.GhcPs))) where
  pretty' (GHC.Hs.LastStmt _ x _ _) = pretty x
  pretty' (GHC.Hs.BindStmt _ pat body) = hor <-|> ver
    where
      hor = spaced [pretty pat, string "<-", pretty body]
      ver = do
        pretty pat
        string " <-"
        newline
        indentedBlock $ pretty body
  pretty' GHC.Hs.ApplicativeStmt {} = notGeneratedByParser
  pretty' (GHC.Hs.BodyStmt _ body _ _) = pretty body
  pretty' (GHC.Hs.LetStmt _ l) = string "let " |=> pretty l
  pretty' (GHC.Hs.ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
  pretty' GHC.Hs.TransStmt {..} =
    vCommaSep $ fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
  pretty' GHC.Hs.RecStmt {..} =
    string "rec " |=> printCommentsAnd recS_stmts (lined . fmap pretty)

instance Pretty StmtLRInsideVerticalList where
  pretty' (StmtLRInsideVerticalList (GHC.Hs.ParStmt _ xs _ _)) =
    vBarSep $ fmap (pretty . ParStmtBlockInsideVerticalList) xs
  pretty' (StmtLRInsideVerticalList x) = pretty x

-- | For pattern matching.
instance Pretty
           (GHC.Hs.HsRecFields
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.Pat GHC.Hs.GhcPs))) where
  pretty' GHC.Hs.HsRecFields {..} = horizontal <-|> vertical
    where
      horizontal =
        case rec_dotdot of
          Just _ -> braces $ string ".."
          Nothing -> hFields $ fmap pretty rec_flds
      vertical = vFields $ fmap pretty rec_flds

-- | For record updates
instance Pretty
           (GHC.Hs.HsRecFields
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsExpr GHC.Hs.GhcPs))) where
  pretty' GHC.Hs.HsRecFields {..} = hvFields fieldPrinters
    where
      fieldPrinters =
        fmap pretty rec_flds
          ++ maybeToList (fmap (const (string "..")) rec_dotdot)

instance Pretty (GHC.Hs.HsType GHC.Hs.GhcPs) where
  pretty' = pretty' . HsType' HsTypeForNormalDecl HsTypeNoDir

instance Pretty HsType' where
  pretty' (HsTypeInsideVerticalFuncSig (GHC.Hs.HsFunTy _ _ a b)) = do
    pretty $ HsTypeInsideVerticalFuncSig <$> a
    newline
    prefixed "-> " $ pretty $ HsTypeInsideVerticalFuncSig <$> b
  pretty' (HsTypeInsideDeclSig GHC.Hs.HsQualTy {..}) = hor <-|> ver
    where
      hor = spaced [pretty $ Context hst_ctxt, string "=>", pretty hst_body]
      ver = do
        pretty $ Context hst_ctxt
        newline
        prefixed "=> " $ pretty $ fmap HsTypeInsideVerticalFuncSig hst_body
  pretty' (HsTypeInsideDeclSig (GHC.Hs.HsFunTy _ _ a b)) = hor <-|> ver
    where
      hor = spaced [pretty a, string "->", pretty b]
      ver = do
        pretty $ fmap HsTypeInsideVerticalFuncSig a
        newline
        prefixed "-> " $ pretty $ fmap HsTypeInsideVerticalFuncSig b
  pretty' (HsTypeInsideInstDecl GHC.Hs.HsQualTy {..}) = hor <-|> ver
    where
      hor = spaced [pretty (Context hst_ctxt), string "=>", pretty hst_body]
      ver = do
        pretty (Context hst_ctxt)
        string " =>"
        newline
        pretty hst_body
  pretty' (HsTypeWithVerticalAppTy (GHC.Hs.HsAppTy _ l r)) = do
    pretty $ fmap HsTypeWithVerticalAppTy l
    newline
    indentedBlock $ pretty $ fmap HsTypeWithVerticalAppTy r
  pretty' (HsType' _ _ x) = prettyHsType x

prettyHsType :: GHC.Hs.HsType GHC.Hs.GhcPs -> Printer ()
prettyHsType (GHC.Hs.HsForAllTy _ tele body) =
  (pretty tele >> space) |=> pretty body
prettyHsType GHC.Hs.HsQualTy {..} = hor <-|> ver
  where
    hor = spaced [pretty $ Context hst_ctxt, string "=>", pretty hst_body]
    ver = do
      pretty $ Context hst_ctxt
      lined [string " =>", indentedBlock $ pretty hst_body]
prettyHsType (GHC.Hs.HsTyVar _ NotPromoted x) = pretty x
prettyHsType (GHC.Hs.HsTyVar _ IsPromoted x) = string "'" >> pretty x
prettyHsType x@(GHC.Hs.HsAppTy _ l r) = hor <-|> ver
  where
    hor = spaced $ fmap pretty [l, r]
    ver = pretty $ HsTypeWithVerticalAppTy x
#if MIN_VERSION_ghc_lib_parser(9,8,1)
prettyHsType (HsAppKindTy _ l _ r) = pretty l >> string " @" >> pretty r
#else
prettyHsType (GHC.Hs.HsAppKindTy _ l r) = pretty l >> string " @" >> pretty r
#endif
prettyHsType (GHC.Hs.HsFunTy _ _ a b) = (pretty a >> string " -> ") |=> pretty b
prettyHsType (GHC.Hs.HsListTy _ xs) = brackets $ pretty xs
prettyHsType (GHC.Hs.HsTupleTy _ GHC.Hs.HsUnboxedTuple []) = string "(# #)"
prettyHsType (GHC.Hs.HsTupleTy _ GHC.Hs.HsBoxedOrConstraintTuple []) =
  string "()"
prettyHsType (GHC.Hs.HsTupleTy _ GHC.Hs.HsUnboxedTuple xs) =
  hvUnboxedTuple' $ fmap pretty xs
prettyHsType (GHC.Hs.HsTupleTy _ GHC.Hs.HsBoxedOrConstraintTuple xs) =
  hvTuple' $ fmap pretty xs
prettyHsType (GHC.Hs.HsSumTy _ xs) = hvUnboxedSum' $ fmap pretty xs
-- For `HsOpTy`, we do not need a single quote for the infix operator. An
-- explicit promotion is necessary if there is a data constructor and
-- a type with the same name. However, infix data constructors never
-- share their names with types because types cannot contain symbols.
-- Thus there is no ambiguity.
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsType (HsOpTy _ _ l op r) = do
  lineBreak <- gets (configLineBreaks . psConfig)
  if showOutputable op `elem` lineBreak
    then do
      pretty l
      newline
      pretty $ fmap InfixOp op
      space
      pretty r
    else spaced [pretty l, pretty $ fmap InfixOp op, pretty r]
#else
prettyHsType (GHC.Hs.HsOpTy _ l op r) = do
  lineBreak <- gets (configLineBreaks . psConfig)
  if showOutputable op `elem` lineBreak
    then do
      pretty l
      newline
      pretty $ fmap InfixOp op
      space
      pretty r
    else spaced [pretty l, pretty $ fmap InfixOp op, pretty r]
#endif
prettyHsType (GHC.Hs.HsParTy _ inside) = parens $ pretty inside
prettyHsType (GHC.Hs.HsIParamTy _ x ty) =
  spaced [string "?" >> pretty x, string "::", pretty ty]
prettyHsType GHC.Hs.HsStarTy {} = string "*"
prettyHsType (GHC.Hs.HsKindSig _ t k) = spaced [pretty t, string "::", pretty k]
prettyHsType (GHC.Hs.HsSpliceTy _ sp) = pretty sp
prettyHsType GHC.Hs.HsDocTy {} = docNode
prettyHsType (GHC.Hs.HsBangTy _ pack x) = pretty pack >> pretty x
prettyHsType (GHC.Hs.HsRecTy _ xs) = hvFields $ fmap pretty xs
prettyHsType (GHC.Hs.HsExplicitListTy _ _ xs) =
  case xs of
    [] -> string "'[]"
    _ -> hvPromotedList $ fmap pretty xs
prettyHsType (GHC.Hs.HsExplicitTupleTy _ xs) = hPromotedTuple $ fmap pretty xs
prettyHsType (GHC.Hs.HsTyLit _ x) = pretty x
prettyHsType GHC.Hs.HsWildCardTy {} = string "_"
prettyHsType GHC.Hs.XHsType {} = notGeneratedByParser

instance Pretty
           (GHC.Hs.GRHSs
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsExpr GHC.Hs.GhcPs))) where
  pretty' = pretty' . GRHSsExpr GRHSExprNormal

instance Pretty GRHSsExpr where
  pretty' (GRHSsExpr {grhssExpr = GHC.Hs.GRHSs {..}, ..}) = do
    mapM_ (pretty . fmap (GRHSExpr grhssExprType)) grhssGRHSs
    case (grhssLocalBinds, grhssExprType) of
      (GHC.Hs.HsValBinds {}, GRHSExprCase) ->
        indentedBlock $ do
          newline
          string "where " |=> pretty grhssLocalBinds
      (GHC.Hs.HsValBinds epa lr, _) ->
        indentedWithSpace 2
          $ newlinePrefixed
              [ string "where"
              , printCommentsAnd (L epa lr) (indentedWithSpace 2 . pretty)
              ]
      _ -> return ()

instance Pretty
           (GHC.Hs.GRHSs
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsCmd GHC.Hs.GhcPs))) where
  pretty' GHC.Hs.GRHSs {..} = do
    mapM_ (pretty . fmap GRHSProc) grhssGRHSs
    case grhssLocalBinds of
      (GHC.Hs.HsValBinds epa lr) ->
        indentedWithSpace 2
          $ newlinePrefixed
              [ string "where"
              , printCommentsAnd (L epa lr) (indentedWithSpace 2 . pretty)
              ]
      _ -> return ()

instance Pretty (GHC.Hs.HsMatchContext GHC.Hs.GhcPs) where
  pretty' = prettyHsMatchContext

prettyHsMatchContext :: GHC.Hs.HsMatchContext GHC.Hs.GhcPs -> Printer ()
prettyHsMatchContext GHC.Hs.FunRhs {..} = pretty mc_strictness >> pretty mc_fun
prettyHsMatchContext GHC.Hs.LambdaExpr = return ()
prettyHsMatchContext GHC.Hs.CaseAlt = return ()
prettyHsMatchContext GHC.Hs.IfAlt {} = notGeneratedByParser
prettyHsMatchContext GHC.Hs.ArrowMatchCtxt {} = notGeneratedByParser
prettyHsMatchContext GHC.Hs.PatBindRhs {} = notGeneratedByParser
prettyHsMatchContext GHC.Hs.PatBindGuards {} = notGeneratedByParser
prettyHsMatchContext GHC.Hs.RecUpd {} = notGeneratedByParser
prettyHsMatchContext GHC.Hs.StmtCtxt {} = notGeneratedByParser
prettyHsMatchContext GHC.Hs.ThPatSplice {} = notGeneratedByParser
prettyHsMatchContext GHC.Hs.ThPatQuote {} = notGeneratedByParser
prettyHsMatchContext GHC.Hs.PatSyn {} = notGeneratedByParser
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsMatchContext LamCaseAlt {} = notUsedInParsedStage
#endif
instance Pretty (GHC.Hs.ParStmtBlock GHC.Hs.GhcPs GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.ParStmtBlock _ xs _ _) = hvCommaSep $ fmap pretty xs

instance Pretty ParStmtBlockInsideVerticalList where
  pretty' (ParStmtBlockInsideVerticalList (GHC.Hs.ParStmtBlock _ xs _ _)) =
    vCommaSep $ fmap pretty xs

instance Pretty RdrName where
  pretty' = pretty . PrefixOp

instance Pretty
           (GHC.Hs.GRHS
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsExpr GHC.Hs.GhcPs))) where
  pretty' = pretty' . GRHSExpr GRHSExprNormal

instance Pretty GRHSExpr where
  pretty' (GRHSExpr {grhsExpr = (GHC.Hs.GRHS _ [] body), ..}) = do
    space
    rhsSeparator grhsExprType
    case unLoc body of
      GHC.Hs.HsDo _ (GHC.Hs.DoExpr m) stmts ->
        printCommentsAnd body (const (doExpr (QualifiedDo m Do) stmts))
      GHC.Hs.HsDo _ (GHC.Hs.MDoExpr m) stmts ->
        printCommentsAnd body (const (doExpr (QualifiedDo m Mdo) stmts))
      GHC.Hs.OpApp _ (L _ (GHC.Hs.HsDo _ GHC.Hs.DoExpr {} _)) _ _ ->
        space >> pretty body
      GHC.Hs.OpApp _ (L _ (GHC.Hs.HsDo _ GHC.Hs.MDoExpr {} _)) _ _ ->
        space >> pretty body
      _ ->
        let hor = space >> pretty body
            ver = newline >> indentedBlock (pretty body)
         in hor <-|> ver
    where
      doExpr qDo stmts = do
        space
        pretty qDo
        newline
        indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
  pretty' (GRHSExpr {grhsExpr = (GHC.Hs.GRHS _ guards body), ..}) = do
    unless (grhsExprType == GRHSExprMultiWayIf) newline
    (if grhsExprType == GRHSExprMultiWayIf
       then id
       else indentedBlock) $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      space
      rhsSeparator grhsExprType
      printCommentsAnd body $ \case
        GHC.Hs.HsDo _ (GHC.Hs.DoExpr m) stmts -> doExpr (QualifiedDo m Do) stmts
        GHC.Hs.HsDo _ (GHC.Hs.MDoExpr m) stmts ->
          doExpr (QualifiedDo m Mdo) stmts
        x ->
          let hor = space >> pretty x
              ver = newline >> indentedBlock (pretty x)
           in hor <-|> ver
    where
      doExpr qDo stmts = do
        space
        pretty qDo
        newline
        indentedBlock (printCommentsAnd stmts (lined . fmap pretty))

instance Pretty GRHSProc where
  pretty' (GRHSProc (GHC.Hs.GRHS _ guards body)) =
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
          GHC.Hs.HsCmdDo _ stmts ->
            let hor = space >> printCommentsAnd stmts (lined . fmap pretty)
                ver = do
                  newline
                  indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
             in hor <-|> ver
          x ->
            let hor = space >> pretty x
                ver = newline >> indentedBlock (pretty x)
             in hor <-|> ver

instance Pretty GHC.Hs.EpaCommentTok where
  pretty' (GHC.Hs.EpaLineComment c) = string c
  pretty' (GHC.Hs.EpaBlockComment c) =
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

instance Pretty (GHC.Hs.SpliceDecl GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.SpliceDecl _ sp _) = pretty sp
#if !MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (GHC.Hs.HsSplice GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.HsTypedSplice _ _ _ body) = string "$$" >> pretty body
  pretty' (GHC.Hs.HsUntypedSplice _ GHC.Hs.DollarSplice _ body) =
    string "$" >> pretty body
  pretty' (GHC.Hs.HsUntypedSplice _ GHC.Hs.BareSplice _ body) = pretty body
  -- The body of a quasi-quote must not be changed by a formatter.
  -- Changing it will modify the actual behavior of the code.
  pretty' (GHC.Hs.HsQuasiQuote _ _ l _ r) =
    brackets $ do
      pretty l
      wrapWithBars
        $ indentedWithFixedLevel 0
        $ sequence_
        $ printers [] ""
        $ unpackFS r
    where
      printers ps s [] = reverse (string (reverse s) : ps)
      printers ps s ('\n':xs) =
        printers (newline : string (reverse s) : ps) "" xs
      printers ps s (x:xs) = printers ps (x : s) xs
  pretty' GHC.Hs.HsSpliced {} = notGeneratedByParser
#endif
instance Pretty (GHC.Hs.Pat GHC.Hs.GhcPs) where
  pretty' = prettyPat

instance Pretty PatInsidePatDecl where
  pretty' (PatInsidePatDecl (GHC.Hs.ConPat { pat_args = (GHC.Hs.InfixCon l r)
                                           , ..
                                           })) =
    spaced [pretty l, pretty $ fmap InfixOp pat_con, pretty r]
  pretty' (PatInsidePatDecl x) = pretty x

prettyPat :: GHC.Hs.Pat GHC.Hs.GhcPs -> Printer ()
prettyPat GHC.Hs.WildPat {} = string "_"
prettyPat (GHC.Hs.VarPat _ x) = pretty x
prettyPat (GHC.Hs.LazyPat _ x) = string "~" >> pretty x
#if MIN_VERSION_ghc_lib_parser(9,6,1)
prettyPat (AsPat _ a _ b) = pretty a >> string "@" >> pretty b
#else
prettyPat (GHC.Hs.AsPat _ a b) = pretty a >> string "@" >> pretty b
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyPat (ParPat _ _ inner _) = parens $ pretty inner
#else
prettyPat (GHC.Hs.ParPat _ inner) = parens $ pretty inner
#endif
prettyPat (GHC.Hs.BangPat _ x) = string "!" >> pretty x
prettyPat (GHC.Hs.ListPat _ xs) = hList $ fmap pretty xs
prettyPat (GHC.Hs.TuplePat _ pats Boxed) = hTuple $ fmap pretty pats
prettyPat (GHC.Hs.TuplePat _ pats Unboxed) = hUnboxedTuple $ fmap pretty pats
prettyPat (GHC.Hs.SumPat _ x position numElem) = do
  string "(#"
  forM_ [1 .. numElem] $ \idx -> do
    if idx == position
      then string " " >> pretty x >> string " "
      else string " "
    when (idx < numElem) $ string "|"
  string "#)"
prettyPat GHC.Hs.ConPat {..} =
  case pat_args of
    GHC.Hs.PrefixCon _ as -> do
      pretty $ fmap PrefixOp pat_con
      spacePrefixed $ fmap pretty as
    GHC.Hs.RecCon rec -> (pretty pat_con >> space) |=> pretty (RecConPat rec)
    GHC.Hs.InfixCon a b -> do
      pretty a
      unlessSpecialOp (unLoc pat_con) space
      pretty $ fmap InfixOp pat_con
      unlessSpecialOp (unLoc pat_con) space
      pretty b
prettyPat (GHC.Hs.ViewPat _ l r) = spaced [pretty l, string "->", pretty r]
prettyPat (GHC.Hs.SplicePat _ x) = pretty x
prettyPat (GHC.Hs.LitPat _ x) = pretty x
prettyPat (GHC.Hs.NPat _ x _ _) = pretty x
prettyPat (GHC.Hs.NPlusKPat _ n k _ _ _) = pretty n >> string "+" >> pretty k
prettyPat (GHC.Hs.SigPat _ l r) = spaced [pretty l, string "::", pretty r]

instance Pretty RecConPat where
  pretty' (RecConPat GHC.Hs.HsRecFields {..}) =
    case fieldPrinters of
      [] -> string "{}"
      [x] -> braces x
      xs -> hvFields xs
    where
      fieldPrinters =
        fmap (pretty . fmap RecConField) rec_flds
          ++ maybeToList (fmap (const (string "..")) rec_dotdot)
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (GHC.Hs.HsBracket GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.ExpBr _ expr) = brackets $ wrapWithBars $ pretty expr
  pretty' (GHC.Hs.PatBr _ expr) =
    brackets $ string "p" >> wrapWithBars (pretty expr)
  pretty' (GHC.Hs.DecBrL _ decls) =
    brackets $ string "d| " |=> lined (fmap pretty decls) >> string " |"
  pretty' GHC.Hs.DecBrG {} = notGeneratedByParser
  pretty' (GHC.Hs.TypBr _ expr) =
    brackets $ string "t" >> wrapWithBars (pretty expr)
  pretty' (GHC.Hs.VarBr _ True var) = string "'" >> pretty var
  pretty' (GHC.Hs.VarBr _ False var) = string "''" >> pretty var
  pretty' (GHC.Hs.TExpBr _ x) = typedBrackets $ pretty x
#endif
instance Pretty SigBindFamily where
  pretty' (Sig x) = pretty x
  pretty' (Bind x) = pretty x
  pretty' (TypeFamily x) = pretty x
  pretty' (TyFamInst x) = pretty x
  pretty' (DataFamInst x) = pretty $ DataFamInstDeclInsideClassInst x

instance Pretty GHC.Hs.EpaComment where
  pretty' GHC.Hs.EpaComment {..} = pretty ac_tok

instance Pretty (GHC.Hs.HsLocalBindsLR GHC.Hs.GhcPs GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.HsValBinds _ lr) = pretty lr
  pretty' (GHC.Hs.HsIPBinds _ x) = pretty x
  pretty' GHC.Hs.EmptyLocalBinds {} =
    error
      "This branch indicates that the bind is empty, but since calling this code means that let or where has already been output, it cannot be handled here. It should be handled higher up in the AST."

instance Pretty (GHC.Hs.HsValBindsLR GHC.Hs.GhcPs GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.ValBinds _ methods sigs) = lined $ fmap pretty sigsAndMethods
    where
      sigsAndMethods =
        mkSortedLSigBindFamilyList sigs (bagToList methods) [] [] []
  pretty' GHC.Hs.XValBindsLR {} = notUsedInParsedStage

instance Pretty (GHC.Hs.HsTupArg GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.Present _ e) = pretty e
  pretty' GHC.Hs.Missing {} = pure () -- This appears in a tuple section.
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty RecConField where
  pretty' (RecConField HsFieldBind {..}) = do
    pretty hfbLHS
    unless hfbPun $ do
      string " = "
      pretty hfbRHS
#else
-- | For pattern matching against a record.
instance Pretty
           (GHC.Hs.HsRecField'
              (GHC.Hs.FieldOcc GHC.Hs.GhcPs)
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.Pat GHC.Hs.GhcPs))) where
  pretty' GHC.Hs.HsRecField {..} =
    (pretty hsRecFieldLbl >> string " = ") |=> pretty hsRecFieldArg

-- | For record updates.
instance Pretty
           (GHC.Hs.HsRecField'
              (GHC.Hs.FieldOcc GHC.Hs.GhcPs)
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsExpr GHC.Hs.GhcPs))) where
  pretty' GHC.Hs.HsRecField {..} = do
    pretty hsRecFieldLbl
    unless hsRecPun $ do
      string " ="
      horizontal <-|> vertical
    where
      horizontal = space >> pretty hsRecFieldArg
      vertical = newline >> indentedBlock (pretty hsRecFieldArg)
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
-- | For pattern matchings against records.
instance Pretty
           (HsFieldBind
              (GenLocated (SrcAnn NoEpAnns) (FieldOcc GhcPs))
              (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  pretty' HsFieldBind {..} = (pretty hfbLHS >> string " = ") |=> pretty hfbRHS

-- | For record updates.
instance Pretty
           (HsFieldBind
              (GenLocated (SrcAnn NoEpAnns) (FieldOcc GhcPs))
              (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' HsFieldBind {..} = do
    pretty hfbLHS
    unless hfbPun $ do
      string " ="
      horizontal <-|> vertical
    where
      horizontal = space >> pretty hfbRHS
      vertical = newline >> indentedBlock (pretty hfbRHS)
#else
instance Pretty RecConField where
  pretty' (RecConField GHC.Hs.HsRecField {..}) = do
    pretty hsRecFieldLbl
    unless hsRecPun $ do
      string " = "
      pretty hsRecFieldArg
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (FieldOcc GhcPs) where
  pretty' FieldOcc {..} = pretty foLabel
#else
instance Pretty (GHC.Hs.FieldOcc GHC.Hs.GhcPs) where
  pretty' GHC.Hs.FieldOcc {..} = pretty rdrNameFieldOcc
#endif
-- HsConDeclH98Details
instance Pretty
           (GHC.Hs.HsConDetails
              Void
              (GHC.Hs.HsScaled
                 GHC.Hs.GhcPs
                 (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.BangType GHC.Hs.GhcPs)))
              (GenLocated
                 GHC.Hs.SrcSpanAnnL
                 [GenLocated
                    GHC.Hs.SrcSpanAnnA
                    (GHC.Hs.ConDeclField GHC.Hs.GhcPs)])) where
  pretty' (GHC.Hs.PrefixCon _ xs) = horizontal <-|> vertical
    where
      horizontal = spacePrefixed $ fmap pretty xs
      vertical = indentedBlock $ newlinePrefixed $ fmap pretty xs
  pretty' (GHC.Hs.RecCon x) =
    printCommentsAnd x $ \rec -> do
      newline
      indentedBlock $ vFields $ fmap pretty rec
  pretty' GHC.Hs.InfixCon {} =
    error
      "Cannot handle here because 'InfixCon' does not have the information of its constructor."

instance Pretty a => Pretty (GHC.Hs.HsScaled GHC.Hs.GhcPs a) where
  pretty' (GHC.Hs.HsScaled _ x) = pretty x

instance Pretty (GHC.Hs.ConDeclField GHC.Hs.GhcPs) where
  pretty' GHC.Hs.ConDeclField {..}
    -- Here, we *ignore* the 'cd_fld_doc' field because doc strings are
    -- also stored as comments, and printing both results in duplicated
    -- comments.
   = do
    hCommaSep $ fmap pretty cd_fld_names
    string " :: "
    pretty cd_fld_type

instance Pretty InfixExpr where
  pretty' (InfixExpr (L _ (GHC.Hs.HsVar _ bind))) = pretty $ fmap InfixOp bind
  pretty' (InfixExpr x) = pretty' x

instance Pretty InfixApp where
  pretty' InfixApp {..} = horizontal <-|> vertical
    where
      horizontal = spaced [pretty lhs, pretty (InfixExpr op), pretty rhs]
      vertical =
        case findFixity op of
          Fixity _ _ InfixL -> leftAssoc
          Fixity _ _ InfixR -> rightAssoc
          Fixity _ _ InfixN -> noAssoc
      leftAssoc = prettyOps allOperantsAndOperatorsLeftAssoc
      rightAssoc = prettyOps allOperantsAndOperatorsRightAssoc
      noAssoc
        | L _ (GHC.Hs.OpApp _ _ o _) <- lhs
        , isSameAssoc o = leftAssoc
        | otherwise = rightAssoc
      prettyOps [l, o, L _ (GHC.Hs.HsDo _ (GHC.Hs.DoExpr m) xs)] = do
        spaced [pretty l, pretty $ InfixExpr o, pretty $ QualifiedDo m Do]
        newline
        indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
      prettyOps [l, o, L _ (GHC.Hs.HsDo _ (GHC.Hs.MDoExpr m) xs)] = do
        spaced [pretty l, pretty $ InfixExpr o, pretty $ QualifiedDo m Mdo]
        newline
        indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
      prettyOps [l, o, r@(L _ GHC.Hs.HsLam {})] = do
        spaced [pretty l, pretty $ InfixExpr o, pretty r]
      prettyOps [l, o, r@(L _ GHC.Hs.HsLamCase {})] = do
        spaced [pretty l, pretty $ InfixExpr o, pretty r]
      prettyOps (l:xs) = do
        pretty l
        newline
        indentedBlock $ f xs
        where
          f (o:r:rems) = do
            (pretty (InfixExpr o) >> space) |=> pretty r
            unless (null rems) $ do
              newline
              f rems
          f _ =
            error
              "The number of the sum of operants and operators should be odd."
      prettyOps _ = error "Too short list."
      findFixity o = fromMaybe defaultFixity $ lookup (varToStr o) fixities
      allOperantsAndOperatorsLeftAssoc = reverse $ rhs : op : collect lhs
        where
          collect ::
               GHC.Hs.LHsExpr GHC.Hs.GhcPs -> [GHC.Hs.LHsExpr GHC.Hs.GhcPs]
          collect (L _ (GHC.Hs.OpApp _ l o r))
            | isSameAssoc o = r : o : collect l
          collect x = [x]
      allOperantsAndOperatorsRightAssoc = lhs : op : collect rhs
        where
          collect ::
               GHC.Hs.LHsExpr GHC.Hs.GhcPs -> [GHC.Hs.LHsExpr GHC.Hs.GhcPs]
          collect (L _ (GHC.Hs.OpApp _ l o r))
            | isSameAssoc o = l : o : collect r
          collect x = [x]
      isSameAssoc (findFixity -> Fixity _ lv d) = lv == level && d == dir
      Fixity _ level dir = findFixity op

instance Pretty a => Pretty (BooleanFormula a) where
  pretty' (Var x) = pretty x
  pretty' (And xs) = hvCommaSep $ fmap pretty xs
  pretty' (Or xs) = hvBarSep $ fmap pretty xs
  pretty' (Parens x) = parens $ pretty x

instance Pretty (GHC.Hs.FieldLabelStrings GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.FieldLabelStrings xs) = hDotSep $ fmap pretty xs

instance Pretty (GHC.Hs.AmbiguousFieldOcc GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.Unambiguous _ name) = pretty name
  pretty' (GHC.Hs.Ambiguous _ name) = pretty name

instance Pretty (GHC.Hs.HsDerivingClause GHC.Hs.GhcPs) where
  pretty' GHC.Hs.HsDerivingClause { deriv_clause_strategy = Just strategy@(L _ GHC.Hs.ViaStrategy {})
                                  , ..
                                  } =
    spaced [string "deriving", pretty deriv_clause_tys, pretty strategy]
  pretty' GHC.Hs.HsDerivingClause {..} = do
    string "deriving "
    whenJust deriv_clause_strategy $ \x -> do
      pretty x
      space
    pretty deriv_clause_tys

instance Pretty (GHC.Hs.DerivClauseTys GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.DctSingle _ ty) = parens $ pretty ty
  pretty' (GHC.Hs.DctMulti _ ts) = hvTuple $ fmap pretty ts

instance Pretty OverlapMode where
  pretty' NoOverlap {} = notUsedInParsedStage
  pretty' Overlappable {} = string "{-# OVERLAPPABLE #-}"
  pretty' Overlapping {} = string "{-# OVERLAPPING #-}"
  pretty' Overlaps {} = string "{-# OVERLAPS #-}"
  pretty' Incoherent {} = string "{-# INCOHERENT #-}"

instance Pretty StringLiteral where
  pretty' = output

-- | This instance is for type family declarations inside a class declaration.
instance Pretty (GHC.Hs.FamilyDecl GHC.Hs.GhcPs) where
  pretty' GHC.Hs.FamilyDecl {..} = do
    string
      $ case fdInfo of
          GHC.Hs.DataFamily -> "data"
          GHC.Hs.OpenTypeFamily -> "type"
          GHC.Hs.ClosedTypeFamily {} -> "type"
    case fdTopLevel of
      TopLevel -> string " family "
      NotTopLevel -> space
    pretty fdLName
    spacePrefixed $ pretty <$> hsq_explicit fdTyVars
    case unLoc fdResultSig of
      GHC.Hs.NoSig {} -> pure ()
      GHC.Hs.TyVarSig {} -> do
        string " = "
        pretty fdResultSig
      _ -> do
        space
        pretty fdResultSig
    whenJust fdInjectivityAnn $ \x -> do
      string " | "
      pretty x
    case fdInfo of
      GHC.Hs.ClosedTypeFamily (Just xs) -> do
        string " where"
        newline
        indentedBlock $ lined $ fmap pretty xs
      _ -> pure ()

instance Pretty (GHC.Hs.FamilyResultSig GHC.Hs.GhcPs) where
  pretty' GHC.Hs.NoSig {} = pure ()
  pretty' (GHC.Hs.KindSig _ x) = string ":: " >> pretty x
  pretty' (GHC.Hs.TyVarSig _ x) = pretty x

instance Pretty (GHC.Hs.HsTyVarBndr a GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.UserTyVar _ _ x) = pretty x
  pretty' (GHC.Hs.KindedTyVar _ _ name ty) =
    parens $ spaced [pretty name, string "::", pretty ty]

instance Pretty (GHC.Hs.InjectivityAnn GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.InjectivityAnn _ from to) =
    spaced $ pretty from : string "->" : fmap pretty to

instance Pretty (GHC.Hs.ArithSeqInfo GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.From from) = brackets $ spaced [pretty from, string ".."]
  pretty' (GHC.Hs.FromThen from next) =
    brackets $ spaced [pretty from >> comma >> pretty next, string ".."]
  pretty' (GHC.Hs.FromTo from to) =
    brackets $ spaced [pretty from, string "..", pretty to]
  pretty' (GHC.Hs.FromThenTo from next to) =
    brackets
      $ spaced [pretty from >> comma >> pretty next, string "..", pretty to]

instance Pretty (GHC.Hs.HsForAllTelescope GHC.Hs.GhcPs) where
  pretty' GHC.Hs.HsForAllVis {..} = do
    string "forall "
    spaced $ fmap pretty hsf_vis_bndrs
    dot
  pretty' GHC.Hs.HsForAllInvis {..} = do
    string "forall "
    spaced $ fmap pretty hsf_invis_bndrs
    dot

instance Pretty InfixOp where
  pretty' (InfixOp (Unqual name)) = backticksIfNotSymbol name $ pretty name
  pretty' (InfixOp (Qual modName name)) =
    backticksIfNotSymbol name $ do
      pretty modName
      string "."
      pretty name
  pretty' (InfixOp Orig {}) = notUsedInParsedStage
  pretty' (InfixOp (Exact name)) = backticksIfNotSymbol occ $ pretty occ
    where
      occ = occName name

instance Pretty PrefixOp where
  pretty' (PrefixOp (Unqual name)) = parensIfSymbol name $ pretty name
  pretty' (PrefixOp (Qual modName name)) =
    parensIfSymbol name $ do
      pretty modName
      string "."
      pretty name
  pretty' (PrefixOp Orig {}) = notUsedInParsedStage
  pretty' (PrefixOp (Exact name)) = parensIfSymbol occ $ output name
    where
      occ = occName name

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
          (L _ []) -> parens
          (L _ [_]) -> id
          _ -> parens

instance Pretty VerticalContext where
  pretty' (VerticalContext full@(L _ [])) =
    printCommentsAnd full (const $ string "()")
  pretty' (VerticalContext full@(L _ [x])) =
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
          Just (L _ []) -> parens
          Just (L _ [_]) -> id
          Just _ -> parens

instance Pretty VerticalContext where
  pretty' (VerticalContext Nothing) = pure ()
  pretty' (VerticalContext (Just (L _ []))) = string "()"
  pretty' (VerticalContext (Just full@(L _ [x]))) =
    printCommentsAnd full (const $ pretty x)
  pretty' (VerticalContext (Just xs)) =
    printCommentsAnd xs (vTuple . fmap pretty)
#endif
-- Wrap a value of this type with 'ModulenameWithPrefix' to print it with
-- the "module " prefix.
instance Pretty ModuleName where
  pretty' = output

instance Pretty ModuleNameWithPrefix where
  pretty' (ModuleNameWithPrefix name) = spaced [string "module", pretty name]

instance Pretty (GHC.Hs.IE GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.IEVar _ name) = pretty name
  pretty' (GHC.Hs.IEThingAbs _ name) = pretty name
  pretty' (GHC.Hs.IEThingAll _ name) = do
    pretty name
    string "(..)"
  -- FIXME: Currently, pretty-printing a 'IEThingWith' uses
  -- 'ghc-lib-parser''s pretty-printer. However, we should avoid it because
  -- 'ghc-lib-parser' may suddenly change how it prints, resulting in
  -- unexpected test failures.
  pretty' x@GHC.Hs.IEThingWith {} =
    case lines $ showOutputable x of
      [] -> pure ()
      [x'] -> string x'
      xs -> do
        string $ head xs
        indentedWithFixedLevel 0 $ newlinePrefixed $ string <$> tail xs
  pretty' (GHC.Hs.IEModuleContents _ name) =
    pretty $ fmap ModuleNameWithPrefix name
  pretty' GHC.Hs.IEGroup {} = docNode
  pretty' GHC.Hs.IEDoc {} = docNode
  pretty' GHC.Hs.IEDocNamed {} = docNode

instance Pretty
           (GHC.Hs.FamEqn
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsType GHC.Hs.GhcPs))) where
  pretty' GHC.Hs.FamEqn {..} = do
    pretty feqn_tycon
    spacePrefixed $ fmap pretty feqn_pats
    string " = "
    pretty feqn_rhs

-- | Pretty-print a data instance.
instance Pretty (GHC.Hs.FamEqn GHC.Hs.GhcPs (GHC.Hs.HsDataDefn GHC.Hs.GhcPs)) where
  pretty' = pretty' . FamEqnTopLevel

instance Pretty FamEqn' where
  pretty' FamEqn' {famEqn = GHC.Hs.FamEqn {..}, ..} = do
    spaced $ string prefix : pretty feqn_tycon : fmap pretty feqn_pats
    pretty feqn_rhs
    where
      prefix =
        case famEqnFor of
          DataFamInstDeclForTopLevel -> "data instance"
          DataFamInstDeclForInsideClassInst -> "data"
-- | HsArg (LHsType GhcPs) (LHsType GhcPs)
#if MIN_VERSION_ghc_lib_parser(9,8,1)
instance Pretty
           (HsArg
              GhcPs
              (GenLocated SrcSpanAnnA (HsType GhcPs))
              (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  pretty' (HsValArg x) = pretty x
  pretty' (HsTypeArg _ x) = string "@" >> pretty x
  pretty' HsArgPar {} = notUsedInParsedStage
#else
instance Pretty
           (GHC.Hs.HsArg
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsType GHC.Hs.GhcPs))
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsType GHC.Hs.GhcPs))) where
  pretty' (GHC.Hs.HsValArg x) = pretty x
  pretty' (GHC.Hs.HsTypeArg _ x) = string "@" >> pretty x
  pretty' GHC.Hs.HsArgPar {} = notUsedInParsedStage
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (HsQuote GhcPs) where
  pretty' (ExpBr _ x) = brackets $ wrapWithBars $ pretty x
  pretty' (PatBr _ x) = brackets $ string "p" >> wrapWithBars (pretty x)
  pretty' (DecBrL _ decls) =
    brackets $ string "d| " |=> lined (fmap pretty decls) >> string " |"
  pretty' DecBrG {} = notUsedInParsedStage
  pretty' (TypBr _ x) = brackets $ string "t" >> wrapWithBars (pretty x)
  pretty' (VarBr _ True x) = string "'" >> pretty x
  pretty' (VarBr _ False x) = string "''" >> pretty x
#endif
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (WarnDecls GhcPs) where
  pretty' (Warnings _ x) = lined $ fmap pretty x
#else
instance Pretty (GHC.Hs.WarnDecls GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.Warnings _ _ x) = lined $ fmap pretty x
#endif
#if MIN_VERSION_ghc_lib_parser(9,8,1)
instance Pretty (WarnDecl GhcPs) where
  pretty' (Warning _ names deprecatedOrWarning) =
    case deprecatedOrWarning of
      DeprecatedTxt _ reasons -> prettyWithTitleReasons "DEPRECATED" reasons
      WarningTxt _ _ reasons -> prettyWithTitleReasons "WARNING" reasons
    where
      prettyWithTitleReasons title reasons =
        lined
          [ string $ "{-# " ++ title
          , spaced
              [hCommaSep $ fmap pretty names, hCommaSep $ fmap pretty reasons]
          , string " #-}"
          ]
#else
instance Pretty (GHC.Hs.WarnDecl GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.Warning _ names deprecatedOrWarning) =
    case deprecatedOrWarning of
      DeprecatedTxt _ reasons -> prettyWithTitleReasons "DEPRECATED" reasons
      WarningTxt _ reasons -> prettyWithTitleReasons "WARNING" reasons
    where
      prettyWithTitleReasons title reasons =
        lined
          [ string $ "{-# " ++ title
          , spaced
              [hCommaSep $ fmap pretty names, hCommaSep $ fmap pretty reasons]
          , string " #-}"
          ]
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (WithHsDocIdentifiers StringLiteral GhcPs) where
  pretty' WithHsDocIdentifiers {..} = pretty hsDocString
#endif

#if MIN_VERSION_ghc_lib_parser(9,6,1)
-- | 'Pretty' for 'LIEWrappedName (IdP GhcPs)'
instance Pretty (IEWrappedName GhcPs) where
  pretty' (IEName _ name) = pretty name
  pretty' (IEPattern _ name) = spaced [string "pattern", pretty name]
  pretty' (IEType _ name) = string "type " >> pretty name
#else
-- | 'Pretty' for 'LIEWrappedName (IdP GhcPs)'
instance Pretty (GHC.Hs.IEWrappedName RdrName) where
  pretty' (GHC.Hs.IEName name) = pretty name
  pretty' (GHC.Hs.IEPattern _ name) = spaced [string "pattern", pretty name]
  pretty' (GHC.Hs.IEType _ name) = string "type " >> pretty name
#endif
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (DotFieldOcc GhcPs) where
  pretty' DotFieldOcc {..} = printCommentsAnd dfoLabel pretty
#elif MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (DotFieldOcc GhcPs) where
  pretty' DotFieldOcc {..} = printCommentsAnd dfoLabel (string . unpackFS)
#else
instance Pretty (GHC.Hs.HsFieldLabel GHC.Hs.GhcPs) where
  pretty' GHC.Hs.HsFieldLabel {..} =
    printCommentsAnd hflLabel (string . unpackFS)
#endif
instance Pretty (GHC.Hs.RuleDecls GHC.Hs.GhcPs) where
  pretty' GHC.Hs.HsRules {..} =
    lined $ string "{-# RULES" : fmap pretty rds_rules ++ [string " #-}"]
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (RuleDecl GhcPs) where
  pretty' HsRule {..} =
    spaced
      [ printCommentsAnd rd_name (doubleQuotes . string . unpackFS)
      , lhs
      , string "="
      , pretty rd_rhs
      ]
    where
      lhs =
        if null rd_tmvs
          then pretty rd_lhs
          else do
            string "forall "
            spaced $ fmap pretty rd_tmvs
            dot
            space
            pretty rd_lhs
#else
instance Pretty (GHC.Hs.RuleDecl GHC.Hs.GhcPs) where
  pretty' GHC.Hs.HsRule {..} =
    spaced
      [ printCommentsAnd rd_name (doubleQuotes . string . unpackFS . snd)
      , lhs
      , string "="
      , pretty rd_rhs
      ]
    where
      lhs =
        if null rd_tmvs
          then pretty rd_lhs
          else do
            string "forall "
            spaced $ fmap pretty rd_tmvs
            dot
            space
            pretty rd_lhs
#endif
instance Pretty OccName where
  pretty' = output

instance Pretty (GHC.Hs.DerivDecl GHC.Hs.GhcPs) where
  pretty' GHC.Hs.DerivDecl { deriv_strategy = (Just deriv_strategy@(L _ GHC.Hs.ViaStrategy {}))
                           , ..
                           } =
    spaced
      [ string "deriving"
      , pretty deriv_strategy
      , string "instance"
      , pretty deriv_type
      ]
  pretty' GHC.Hs.DerivDecl {..} = do
    string "deriving "
    whenJust deriv_strategy $ \x -> do
      pretty x
      space
    string "instance "
    pretty deriv_type

-- | 'Pretty' for 'LHsSigWcType GhcPs'.
instance Pretty
           (GHC.Hs.HsWildCardBndrs
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsSigType GHC.Hs.GhcPs))) where
  pretty' GHC.Hs.HsWC {..} = pretty hswc_body

-- | 'Pretty' for 'LHsWcType'
instance Pretty
           (GHC.Hs.HsWildCardBndrs
              GHC.Hs.GhcPs
              (GenLocated GHC.Hs.SrcSpanAnnA (GHC.Hs.HsType GHC.Hs.GhcPs))) where
  pretty' GHC.Hs.HsWC {..} = pretty hswc_body

instance Pretty (GHC.Hs.StandaloneKindSig GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.StandaloneKindSig _ name kind) =
    spaced [string "type", pretty name, string "::", pretty kind]

instance Pretty (GHC.Hs.DefaultDecl GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.DefaultDecl _ xs) =
    spaced [string "default", hTuple $ fmap pretty xs]

instance Pretty (GHC.Hs.ForeignDecl GHC.Hs.GhcPs) where
  pretty' GHC.Hs.ForeignImport {..} =
    spaced
      [ string "foreign import"
      , pretty fd_fi
      , pretty fd_name
      , string "::"
      , pretty fd_sig_ty
      ]
  pretty' GHC.Hs.ForeignExport {..} =
    spaced
      [ string "foreign export"
      , pretty fd_fe
      , pretty fd_name
      , string "::"
      , pretty fd_sig_ty
      ]
#if MIN_VERSION_ghc_lib_parser(9,8,0)
instance Pretty (ForeignImport GhcPs) where
  pretty' (CImport (L _ (SourceText s)) conv safety _ _) =
    spaced [pretty conv, pretty safety, output s]
  pretty' (CImport _ conv safety _ _) = spaced [pretty conv, pretty safety]
#elif MIN_VERSION_ghc_lib_parser(9,6,0)
instance Pretty (ForeignImport GhcPs) where
  pretty' (CImport (L _ (SourceText s)) conv safety _ _) =
    spaced [pretty conv, pretty safety, string s]
  pretty' (CImport _ conv safety _ _) = spaced [pretty conv, pretty safety]
#else
instance Pretty GHC.Hs.ForeignImport where
  pretty' (GHC.Hs.CImport conv safety _ _ (L _ (SourceText s))) =
    spaced [pretty conv, pretty safety, string s]
  pretty' (GHC.Hs.CImport conv safety _ _ _) =
    spaced [pretty conv, pretty safety]
#endif

#if MIN_VERSION_ghc_lib_parser(9,8,0)
instance Pretty (ForeignExport GhcPs) where
  pretty' (CExport (L _ (SourceText s)) conv) = spaced [pretty conv, output s]
  pretty' (CExport _ conv) = pretty conv
#elif MIN_VERSION_ghc_lib_parser(9,6,0)
instance Pretty (ForeignExport GhcPs) where
  pretty' (CExport (L _ (SourceText s)) conv) = spaced [pretty conv, string s]
  pretty' (CExport _ conv) = pretty conv
#else
instance Pretty GHC.Hs.ForeignExport where
  pretty' (GHC.Hs.CExport conv (L _ (SourceText s))) =
    spaced [pretty conv, string s]
  pretty' (GHC.Hs.CExport conv _) = pretty conv
#endif
instance Pretty CExportSpec where
  pretty' (CExportStatic _ _ x) = pretty x

instance Pretty Safety where
  pretty' PlaySafe = string "safe"
  pretty' PlayInterruptible = string "interruptible"
  pretty' PlayRisky = string "unsafe"
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (AnnDecl GhcPs) where
  pretty' (HsAnnotation _ (ValueAnnProvenance name) expr) =
    spaced [string "{-# ANN", pretty name, pretty expr, string "#-}"]
  pretty' (HsAnnotation _ (TypeAnnProvenance name) expr) =
    spaced [string "{-# ANN type", pretty name, pretty expr, string "#-}"]
  pretty' (HsAnnotation _ ModuleAnnProvenance expr) =
    spaced [string "{-# ANN module", pretty expr, string "#-}"]
#else
instance Pretty (GHC.Hs.AnnDecl GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.HsAnnotation _ _ (GHC.Hs.ValueAnnProvenance name) expr) =
    spaced [string "{-# ANN", pretty name, pretty expr, string "#-}"]
  pretty' (GHC.Hs.HsAnnotation _ _ (GHC.Hs.TypeAnnProvenance name) expr) =
    spaced [string "{-# ANN type", pretty name, pretty expr, string "#-}"]
  pretty' (GHC.Hs.HsAnnotation _ _ GHC.Hs.ModuleAnnProvenance expr) =
    spaced [string "{-# ANN module", pretty expr, string "#-}"]
#endif
instance Pretty (GHC.Hs.RoleAnnotDecl GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.RoleAnnotDecl _ name roles) =
    spaced
      $ [string "type role", pretty name]
          ++ fmap (maybe (string "_") pretty . unLoc) roles

instance Pretty Role where
  pretty' Nominal = string "nominal"
  pretty' Representational = string "representational"
  pretty' Phantom = string "phantom"

instance Pretty (GHC.Hs.TyFamInstDecl GHC.Hs.GhcPs) where
  pretty' GHC.Hs.TyFamInstDecl {..} = string "type " >> pretty tfid_eqn

instance Pretty TopLevelTyFamInstDecl where
  pretty' (TopLevelTyFamInstDecl GHC.Hs.TyFamInstDecl {..}) =
    string "type instance " >> pretty tfid_eqn

instance Pretty (GHC.Hs.DataFamInstDecl GHC.Hs.GhcPs) where
  pretty' = pretty' . DataFamInstDeclTopLevel

instance Pretty DataFamInstDecl' where
  pretty' DataFamInstDecl' {dataFamInstDecl = GHC.Hs.DataFamInstDecl {..}, ..} =
    pretty $ FamEqn' dataFamInstDeclFor dfid_eqn

instance Pretty (GHC.Hs.PatSynBind GHC.Hs.GhcPs GHC.Hs.GhcPs) where
  pretty' GHC.Hs.PSB {..} = do
    string "pattern "
    case psb_args of
      GHC.Hs.InfixCon l r ->
        spaced [pretty l, pretty $ fmap InfixOp psb_id, pretty r]
      GHC.Hs.PrefixCon _ [] -> pretty psb_id
      _ -> spaced [pretty psb_id, pretty psb_args]
    spacePrefixed [pretty psb_dir, pretty $ fmap PatInsidePatDecl psb_def]
    case psb_dir of
      GHC.Hs.ExplicitBidirectional matches -> do
        newline
        indentedBlock $ string "where " |=> pretty matches
      _ -> pure ()

-- | 'Pretty' for 'HsPatSynDetails'.
instance Pretty
           (GHC.Hs.HsConDetails
              Void
              (GenLocated GHC.Hs.SrcSpanAnnN RdrName)
              [GHC.Hs.RecordPatSynField GHC.Hs.GhcPs]) where
  pretty' (GHC.Hs.PrefixCon _ xs) = spaced $ fmap pretty xs
  pretty' (GHC.Hs.RecCon rec) = hFields $ fmap pretty rec
  pretty' GHC.Hs.InfixCon {} =
    error
      "Cannot handle here because `InfixCon` does not have the information of the constructor."

instance Pretty (GHC.Hs.FixitySig GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.FixitySig _ names fixity) =
    spaced [pretty fixity, hCommaSep $ fmap (pretty . fmap InfixOp) names]

instance Pretty GHC.Hs.Fixity where
  pretty' (Fixity _ level dir) = spaced [pretty dir, string $ show level]

instance Pretty FixityDirection where
  pretty' InfixL = string "infixl"
  pretty' InfixR = string "infixr"
  pretty' InfixN = string "infix"

instance Pretty InlinePragma where
  pretty' InlinePragma {..} = do
    pretty inl_inline
    case inl_act of
      ActiveBefore _ x -> space >> brackets (string $ "~" ++ show x)
      ActiveAfter _ x -> space >> brackets (string $ show x)
      _ -> pure ()

instance Pretty InlineSpec where
  pretty' = prettyInlineSpec

prettyInlineSpec :: InlineSpec -> Printer ()
prettyInlineSpec Inline {} = string "INLINE"
prettyInlineSpec Inlinable {} = string "INLINABLE"
prettyInlineSpec NoInline {} = string "NOINLINE"
prettyInlineSpec NoUserInlinePrag =
  error
    "This branch is executed if the inline pragma is not written, but executing this branch means that the pragma is already about to be output, which indicates something goes wrong."
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyInlineSpec Opaque {} = string "OPAQUE"
#endif
instance Pretty (GHC.Hs.HsPatSynDir GHC.Hs.GhcPs) where
  pretty' GHC.Hs.Unidirectional = string "<-"
  pretty' GHC.Hs.ImplicitBidirectional = string "="
  pretty' GHC.Hs.ExplicitBidirectional {} = string "<-"

instance Pretty (GHC.Hs.HsOverLit GHC.Hs.GhcPs) where
  pretty' GHC.Hs.OverLit {..} = pretty ol_val

instance Pretty GHC.Hs.OverLitVal where
  pretty' (GHC.Hs.HsIntegral x) = pretty x
  pretty' (GHC.Hs.HsFractional x) = pretty x
  pretty' (GHC.Hs.HsIsString _ x) = string $ unpackFS x
#if MIN_VERSION_ghc_lib_parser(9,8,1)
instance Pretty IntegralLit where
  pretty' IL {il_text = SourceText s} = output s
  pretty' IL {..} = string $ show il_value
#else
instance Pretty IntegralLit where
  pretty' IL {il_text = SourceText s} = string s
  pretty' IL {..} = string $ show il_value
#endif
instance Pretty FractionalLit where
  pretty' = output

instance Pretty (GHC.Hs.HsLit GHC.Hs.GhcPs) where
  pretty' x@(GHC.Hs.HsChar _ _) = output x
  pretty' x@GHC.Hs.HsCharPrim {} = output x
  pretty' GHC.Hs.HsInt {} = notUsedInParsedStage
  pretty' (GHC.Hs.HsIntPrim _ x) = string $ show x ++ "#"
  pretty' GHC.Hs.HsWordPrim {} = notUsedInParsedStage
  pretty' GHC.Hs.HsInt64Prim {} = notUsedInParsedStage
  pretty' GHC.Hs.HsWord64Prim {} = notUsedInParsedStage
  pretty' GHC.Hs.HsInteger {} = notUsedInParsedStage
  pretty' GHC.Hs.HsRat {} = notUsedInParsedStage
  pretty' (GHC.Hs.HsFloatPrim _ x) = pretty x >> string "#"
  pretty' GHC.Hs.HsDoublePrim {} = notUsedInParsedStage
  pretty' x =
    case x of
      GHC.Hs.HsString {} -> prettyString
      GHC.Hs.HsStringPrim {} -> prettyString
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
instance Pretty (HsPragE GhcPs) where
  pretty' (HsPragSCC _ x) = spaced [string "{-# SCC", pretty x, string "#-}"]
#else
instance Pretty (GHC.Hs.HsPragE GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.HsPragSCC _ _ x) =
    spaced [string "{-# SCC", pretty x, string "#-}"]
#endif
instance Pretty GHC.Hs.HsIPName where
  pretty' (GHC.Hs.HsIPName x) = string $ unpackFS x
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (HsTyLit GhcPs) where
  pretty' (HsNumTy _ x) = string $ show x
  pretty' (HsStrTy _ x) = string $ ushow x
  pretty' (HsCharTy _ x) = string $ show x
#else
instance Pretty GHC.Hs.HsTyLit where
  pretty' (GHC.Hs.HsNumTy _ x) = string $ show x
  pretty' (GHC.Hs.HsStrTy _ x) = string $ ushow x
  pretty' (GHC.Hs.HsCharTy _ x) = string $ show x
#endif
instance Pretty (GHC.Hs.HsPatSigType GHC.Hs.GhcPs) where
  pretty' GHC.Hs.HsPS {..} = pretty hsps_body

instance Pretty (GHC.Hs.HsIPBinds GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.IPBinds _ xs) = lined $ fmap pretty xs

instance Pretty (GHC.Hs.IPBind GHC.Hs.GhcPs) where
  pretty' = prettyIPBind

prettyIPBind :: GHC.Hs.IPBind GHC.Hs.GhcPs -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyIPBind (IPBind _ l r) =
  spaced [string "?" >> pretty l, string "=", pretty r]
#else
prettyIPBind (GHC.Hs.IPBind _ (Right _) _) = notUsedInParsedStage
prettyIPBind (GHC.Hs.IPBind _ (Left l) r) =
  spaced [string "?" >> pretty l, string "=", pretty r]
#endif
instance Pretty (GHC.Hs.DerivStrategy GHC.Hs.GhcPs) where
  pretty' GHC.Hs.StockStrategy {} = string "stock"
  pretty' GHC.Hs.AnyclassStrategy {} = string "anyclass"
  pretty' GHC.Hs.NewtypeStrategy {} = string "newtype"
  pretty' (GHC.Hs.ViaStrategy x) = string "via " >> pretty x

instance Pretty GHC.Hs.XViaStrategyPs where
  pretty' (GHC.Hs.XViaStrategyPs _ ty) = pretty ty

instance Pretty (GHC.Hs.RecordPatSynField GHC.Hs.GhcPs) where
  pretty' GHC.Hs.RecordPatSynField {..} = pretty recordPatSynField

instance Pretty (GHC.Hs.HsCmdTop GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.HsCmdTop _ cmd) = pretty cmd

instance Pretty (GHC.Hs.HsCmd GHC.Hs.GhcPs) where
  pretty' = prettyHsCmd

prettyHsCmd :: GHC.Hs.HsCmd GHC.Hs.GhcPs -> Printer ()
prettyHsCmd (GHC.Hs.HsCmdArrApp _ f arg GHC.Hs.HsHigherOrderApp True) =
  spaced [pretty f, string "-<<", pretty arg]
prettyHsCmd (GHC.Hs.HsCmdArrApp _ f arg GHC.Hs.HsHigherOrderApp False) =
  spaced [pretty arg, string ">>-", pretty f]
prettyHsCmd (GHC.Hs.HsCmdArrApp _ f arg GHC.Hs.HsFirstOrderApp True) =
  spaced [pretty f, string "-<", pretty arg]
prettyHsCmd (GHC.Hs.HsCmdArrApp _ f arg GHC.Hs.HsFirstOrderApp False) =
  spaced [pretty arg, string ">-", pretty f]
prettyHsCmd (GHC.Hs.HsCmdArrForm _ f _ _ args) =
  bananaBrackets $ spaced $ pretty f : fmap pretty args
prettyHsCmd (GHC.Hs.HsCmdApp _ f arg) = spaced [pretty f, pretty arg]
prettyHsCmd (GHC.Hs.HsCmdLam _ x) = pretty x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsCmd (HsCmdPar _ _ x _) = parens $ pretty x
#else
prettyHsCmd (GHC.Hs.HsCmdPar _ x) = parens $ pretty x
#endif
prettyHsCmd (GHC.Hs.HsCmdCase _ cond arms) = do
  spaced [string "case", pretty cond, string "of"]
  newline
  indentedBlock $ pretty arms
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsCmd (HsCmdLamCase _ _ arms) = do
  string "\\case"
  newline
  indentedBlock $ pretty arms
#else
prettyHsCmd (GHC.Hs.HsCmdLamCase _ arms) = do
  string "\\case"
  newline
  indentedBlock $ pretty arms
#endif
prettyHsCmd (GHC.Hs.HsCmdIf _ _ cond t f) = do
  string "if "
  pretty cond
  newline
  indentedBlock $ lined [string "then " >> pretty t, string "else " >> pretty f]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsCmd (HsCmdLet _ _ binds _ expr) =
  lined [string "let " |=> pretty binds, string " in " |=> pretty expr]
#else
prettyHsCmd (GHC.Hs.HsCmdLet _ binds expr) =
  lined [string "let " |=> pretty binds, string " in " |=> pretty expr]
#endif
prettyHsCmd (GHC.Hs.HsCmdDo _ stmts) = do
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
    lined [string "let " |=> pretty letBinds, string " in " |=> pretty inExpr]

instance Pretty (GHC.Hs.RuleBndr GHC.Hs.GhcPs) where
  pretty' (GHC.Hs.RuleBndr _ name) = pretty name
  pretty' (GHC.Hs.RuleBndrSig _ name sig) =
    parens $ spaced [pretty name, string "::", pretty sig]

instance Pretty CCallConv where
  pretty' CCallConv = string "ccall"
  pretty' CApiConv = string "capi"
  pretty' StdCallConv = string "stdcall"
  pretty' PrimCallConv = string "prim"
  pretty' JavaScriptCallConv = string "javascript"

instance Pretty GHC.Hs.HsSrcBang where
  pretty' (GHC.Hs.HsSrcBang _ unpack strictness) = do
    pretty unpack
    unless (unpack == GHC.Hs.NoSrcUnpack) space
    pretty strictness

instance Pretty GHC.Hs.SrcUnpackedness where
  pretty' GHC.Hs.SrcUnpack = string "{-# UNPACK #-}"
  pretty' GHC.Hs.SrcNoUnpack = string "{-# NOUNPACK #-}"
  pretty' GHC.Hs.NoSrcUnpack = pure ()

instance Pretty GHC.Hs.SrcStrictness where
  pretty' GHC.Hs.SrcLazy = string "~"
  pretty' GHC.Hs.SrcStrict = string "!"
  pretty' GHC.Hs.NoSrcStrict = pure ()

instance Pretty (GHC.Hs.HsOuterSigTyVarBndrs GHC.Hs.GhcPs) where
  pretty' GHC.Hs.HsOuterImplicit {} = pure ()
  pretty' GHC.Hs.HsOuterExplicit {..} = do
    string "forall"
    spacePrefixed $ fmap pretty hso_bndrs
    dot
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty FieldLabelString where
  pretty' = output

instance Pretty (HsUntypedSplice GhcPs) where
  pretty' (HsUntypedSpliceExpr _ x) = string "$" >> pretty x
  -- The body of a quasi-quote must not be changed by a formatter.
  -- Changing it will modify the actual behavior of the code.
  --
  -- TODO: Remove duplicated code
  pretty' (HsQuasiQuote _ l r) =
    brackets $ do
      pretty l
      printCommentsAnd
        r
        (wrapWithBars
           . indentedWithFixedLevel 0
           . sequence_
           . printers [] ""
           . unpackFS)
    where
      printers ps s [] = reverse (string (reverse s) : ps)
      printers ps s ('\n':xs) =
        printers (newline : string (reverse s) : ps) "" xs
      printers ps s (x:xs) = printers ps (x : s) xs
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
