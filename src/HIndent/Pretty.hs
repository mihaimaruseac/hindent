{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}

-- | Pretty printing.
--
-- Some instances define top-level functions to handle CPP.
--
-- Some value constructors never appear in an AST. GHC has three stages for
-- using an AST: parsing, renaming, and type checking, and GHC uses these
-- constructors only in remaining and type checking.
module HIndent.Pretty
  ( pretty
  ) where

import           Control.Monad
import           Control.Monad.RWS
import           Data.Maybe
import           Data.Void
import           GHC.Core.Coercion
import           GHC.Core.InstEnv
import           GHC.Data.Bag
import           GHC.Data.BooleanFormula
import           GHC.Data.FastString
import           GHC.Hs
import           GHC.Stack
import           GHC.Types.Basic
import           GHC.Types.Fixity
import           GHC.Types.ForeignCall
import           GHC.Types.Name
import           GHC.Types.Name.Reader
import           GHC.Types.SourceText
import           GHC.Types.SrcLoc
import           GHC.Unit
import           GHC.Unit.Module.Warnings
import           HIndent.Applicative
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Import
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Pragma
import           HIndent.Pretty.SigBindFamily
import           HIndent.Pretty.Types
import           HIndent.Types
import           Text.Show.Unicode
#if MIN_VERSION_ghc_lib_parser(9,4,1)
import           GHC.Types.PkgQual
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
           (fromIntegral $ srcSpanStartCol $ anchor $ getLoc c) $
         spaced $ fmap pretty $ c : cs
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

instance Pretty HsModule where
  pretty' m = blanklined printers >> newline
    where
      printers = snd <$> filter fst pairs
      pairs =
        [ (pragmaExists m, prettyPragmas m)
        , (moduleDeclExists m, prettyModuleDecl m)
        , (importsExist m, prettyImports)
        , (declsExist m, prettyDecls)
        ]
      prettyModuleDecl HsModule {hsmodName = Nothing} =
        error "The module declaration does not exist."
      prettyModuleDecl HsModule { hsmodName = Just name
                                , hsmodExports = Nothing
                                , ..
                                } = do
        pretty $ fmap ModuleNameWithPrefix name
        whenJust hsmodDeprecMessage $ \x -> do
          space
          pretty $ fmap ModuleDeprecatedPragma x
        string " where"
      prettyModuleDecl HsModule { hsmodName = Just name
                                , hsmodExports = Just exports
                                , ..
                                } = do
        pretty $ fmap ModuleNameWithPrefix name
        whenJust hsmodDeprecMessage $ \x -> do
          space
          pretty $ fmap ModuleDeprecatedPragma x
        newline
        indentedBlock $ do
          printCommentsAnd exports (vTuple . fmap pretty)
          string " where"
      moduleDeclExists HsModule {hsmodName = Nothing} = False
      moduleDeclExists _                              = True
      prettyDecls =
        mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp) $
        addDeclSeparator $ hsmodDecls m
      addDeclSeparator [] = []
      addDeclSeparator [x] = [(x, Nothing)]
      addDeclSeparator (x:xs) =
        (x, Just $ declSeparator $ unLoc x) : addDeclSeparator xs
      declSeparator (SigD _ TypeSig {})   = newline
      declSeparator (SigD _ InlineSig {}) = newline
      declSeparator _                     = blankline
      declsExist = not . null . hsmodDecls
      prettyImports = importDecls >>= blanklined . fmap outputImportGroup
      outputImportGroup = lined . fmap pretty
      importDecls =
        gets (configSortImports . psConfig) >>= \case
          True  -> pure $ extractImportsSorted m
          False -> pure $ extractImports m

instance (CommentExtraction l, Pretty e) => Pretty (GenLocated l e) where
  pretty' (L _ e) = pretty e

instance Pretty (HsDecl GhcPs) where
  pretty' (TyClD _ d)      = pretty d
  pretty' (InstD _ inst)   = pretty inst
  pretty' (DerivD _ x)     = pretty x
  pretty' (ValD _ bind)    = pretty bind
  pretty' (SigD _ s)       = pretty $ DeclSig s
  pretty' (KindSigD _ x)   = pretty x
  pretty' (DefD _ x)       = pretty x
  pretty' (ForD _ x)       = pretty x
  pretty' (WarningD _ x)   = pretty x
  pretty' (AnnD _ x)       = pretty x
  pretty' (RuleD _ x)      = pretty x
  pretty' (SpliceD _ sp)   = pretty sp
  pretty' DocD {}          = docNode
  pretty' (RoleAnnotD _ x) = pretty x

instance Pretty (TyClDecl GhcPs) where
  pretty' = prettyTyClDecl

prettyTyClDecl :: TyClDecl GhcPs -> Printer ()
prettyTyClDecl (FamDecl _ x) = pretty x
prettyTyClDecl SynDecl {..} = do
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
    hor = do
      string " = "
      pretty tcdRhs
    ver =
      indentedBlock $ do
        newline
        string " = "
        indentedBlock $ pretty tcdRhs
#if MIN_VERSION_ghc_lib_parser(9,4,1)
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
        NewType  -> string "newtype "
#else
prettyTyClDecl DataDecl {..} = do
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
        DataType -> string "data "
        NewType  -> string "newtype "
#endif
prettyTyClDecl ClassDecl {..} = do
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
        forM_ tcdFDs $ \x@(L _ FunDep {}) ->
          printCommentsAnd x $ \(FunDep _ from to) ->
            spaced $ fmap pretty from ++ [string "->"] ++ fmap pretty to
      unless (null sigsMethodsFamilies) $ string " where"
    verHead = do
      string "class " |=> do
        whenJust tcdCtxt $ \ctx -> do
          printCommentsAnd ctx $ \case
            []  -> string "()"
            [x] -> pretty x
            xs  -> hvTuple $ fmap pretty xs
          string " =>"
          newline
        printNameAndTypeVariables
      unless (null tcdFDs) $ do
        newline
        indentedBlock $
          string "| " |=>
          vCommaSep
            (flip fmap tcdFDs $ \x@(L _ FunDep {}) ->
               printCommentsAnd x $ \(FunDep _ from to) ->
                 spaced $ fmap pretty from ++ [string "->"] ++ fmap pretty to)
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
              parens $
                spaced [pretty l, pretty $ fmap InfixOp tcdLName, pretty r]
              spacePrefixed $ fmap pretty xs
            _ -> error "Not enough parameters are given."
    sigsMethodsFamilies =
      mkSortedLSigBindFamilyList tcdSigs (bagToList tcdMeths) tcdATs []

instance Pretty (InstDecl GhcPs) where
  pretty' ClsInstD {..}     = pretty cid_inst
  pretty' DataFamInstD {..} = pretty dfid_inst
  pretty' TyFamInstD {..}   = pretty $ TopLevelTyFamInstDecl tfid_inst

instance Pretty (HsBind GhcPs) where
  pretty' = prettyHsBind

prettyHsBind :: HsBind GhcPs -> Printer ()
prettyHsBind FunBind {..}     = pretty fun_matches
prettyHsBind PatBind {..}     = pretty pat_lhs >> pretty pat_rhs
prettyHsBind VarBind {}       = notGeneratedByParser
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsBind AbsBinds {}      = notGeneratedByParser
#endif
prettyHsBind (PatSynBind _ x) = pretty x

instance Pretty (Sig GhcPs) where
  pretty' (TypeSig _ funName params) = do
    printFunName
    string " ::"
    horizontal <-|> vertical
    where
      horizontal = space >> pretty (hswc_body params)
      vertical = do
        headLen <- printerLength printFunName
        indentSpaces <- getIndentSpaces
        if headLen < indentSpaces
          then space
          else newline
        indentedBlock $
          indentedWithSpace 3 $
          pretty $ HsSigTypeInsideVerticalFuncSig <$> hswc_body params
      printFunName = pretty $ head funName
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
        indentedBlock $
          indentedWithSpace 3 $
          printCommentsAnd params (pretty . HsSigTypeInsideDeclSig)
  pretty' IdSig {} = notGeneratedByParser
  pretty' (FixSig _ x) = pretty x
  pretty' (InlineSig _ name detail) =
    spaced [string "{-#", pretty detail, pretty name, string "#-}"]
  pretty' (SpecSig _ name sig _) =
    spaced
      [ string "{-# SPECIALISE"
      , pretty name
      , string "::"
      , pretty $ head sig
      , string "#-}"
      ]
  pretty' (SpecInstSig _ _ sig) =
    spaced [string "{-# SPECIALISE instance", pretty sig, string "#-}"]
  pretty' (MinimalSig _ _ xs) =
    string "{-# MINIMAL " |=> do
      pretty xs
      string " #-}"
  pretty' (SCCFunSig _ _ name _) =
    spaced [string "{-# SCC", pretty name, string "#-}"]
  pretty' (CompleteMatchSig _ _ names _) =
    spaced
      [ string "{-# COMPLETE"
      , printCommentsAnd names (hCommaSep . fmap pretty)
      , string "#-}"
      ]

instance Pretty DeclSig where
  pretty' (DeclSig (TypeSig _ funName params)) = do
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
            indentedBlock $
              indentedWithSpace 3 $
              pretty $ HsSigTypeInsideDeclSig <$> hswc_body params
      printFunName = hCommaSep $ fmap pretty funName
  pretty' (DeclSig x) = pretty x

instance Pretty (HsDataDefn GhcPs) where
  pretty' HsDataDefn {..} =
    if isGADT
      then do
        whenJust dd_kindSig $ \x -> do
          string " :: "
          pretty x
        string " where"
        indentedBlock $ newlinePrefixed $ fmap pretty dd_cons
      else indentedBlock $ do
             case dd_cons of
               [] -> pure ()
               [x] -> do
                 string " ="
                 newline
                 pretty x
               _ -> do
                 newline
                 string "= " |=> vBarSep (fmap pretty dd_cons)
             unless (null dd_derivs) $ do
               newline
               lined $ fmap pretty dd_derivs
    where
      isGADT =
        case dd_cons of
          (L _ ConDeclGADT {}:_) -> True
          _                      -> False

instance Pretty (ClsInstDecl GhcPs) where
  pretty' ClsInstDecl {..} = do
    string "instance " |=> do
      whenJust cid_overlap_mode $ \x -> do
        pretty x
        space
      pretty (fmap HsSigTypeInsideInstDecl cid_poly_ty) |=>
        unless (null sigsAndMethods) (string " where")
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

instance Pretty (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' MG {..} = printCommentsAnd mg_alts (lined . fmap pretty)

instance Pretty (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs))) where
  pretty' MG {..} = printCommentsAnd mg_alts (lined . fmap pretty)

instance Pretty (HsExpr GhcPs) where
  pretty' = prettyHsExpr

prettyHsExpr :: HsExpr GhcPs -> Printer ()
prettyHsExpr (HsVar _ bind) = pretty $ fmap PrefixOp bind
prettyHsExpr (HsUnboundVar _ x) = pretty x
prettyHsExpr (HsOverLabel _ l) = string "#" >> string (unpackFS l)
prettyHsExpr (HsIPVar _ var) = string "?" >> pretty var
prettyHsExpr (HsOverLit _ x) = pretty x
prettyHsExpr (HsLit _ l) = pretty l
prettyHsExpr (HsLam _ body) = pretty body
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (HsLamCase _ LamCase matches) = pretty $ LambdaCase matches Case
prettyHsExpr (HsLamCase _ LamCases matches) = pretty $ LambdaCase matches Cases
#else
prettyHsExpr (HsLamCase _ matches) = pretty $ LambdaCase matches Case
#endif
prettyHsExpr (HsApp _ l r) = horizontal <-|> vertical
  where
    horizontal = spaced [pretty l, pretty r]
    vertical = do
      let (f, args) =
            case flatten l ++ [r] of
              []         -> error "Invalid function application."
              (f':args') -> (f', args')
      col <- gets psColumn
      spaces <- getIndentSpaces
      pretty f
      col' <- gets psColumn
      let diff =
            col' - col -
            if col == 0
              then spaces
              else 0
      if diff + 1 <= spaces
        then space
        else newline
      spaces' <- getIndentSpaces
      indentedWithSpace spaces' $ lined $ fmap pretty args
    flatten :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    flatten (L (SrcSpanAnn (EpAnn _ _ cs) _) (HsApp _ l' r')) =
      flatten l' ++ [insertComments cs r']
    flatten x = [x]
    insertComments :: EpAnnComments -> LHsExpr GhcPs -> LHsExpr GhcPs
    insertComments cs (L s@SrcSpanAnn {ann = e@EpAnn {comments = cs'}} r') =
      L (s {ann = e {comments = cs <> cs'}}) r'
    insertComments _ x = x
prettyHsExpr (HsAppType _ l r) = do
  pretty l
  string " @"
  pretty r
prettyHsExpr (OpApp _ l o r) = pretty (InfixApp l o r False)
prettyHsExpr (NegApp _ x _) = string "-" >> pretty x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (HsPar _ _ expr _) = parens $ pretty expr
#else
prettyHsExpr (HsPar _ expr) = parens $ pretty expr
#endif
prettyHsExpr (SectionL _ l o) = spaced [pretty l, pretty (InfixExpr o)]
prettyHsExpr (SectionR _ o r) = (pretty (InfixExpr o) >> space) |=> pretty r
prettyHsExpr (ExplicitTuple _ full _) = horizontal <-|> vertical
  where
    horizontal = hTuple $ fmap pretty full
    vertical =
      parens $
      prefixedLined "," $
      fmap (\e -> unless (isMissing e) (space |=> pretty e)) full
    isMissing Missing {} = True
    isMissing _          = False
prettyHsExpr (ExplicitSum _ _ _ expr) =
  unboxedSums $ spaced [string "|", pretty expr]
prettyHsExpr (HsCase _ cond arms) = do
  string "case " |=> do
    pretty cond
    string " of"
  if null $ unLoc $ mg_alts arms
    then string " {}"
    else do
      newline
      indentedBlock $ pretty arms
prettyHsExpr (HsIf _ cond t f) = do
  string "if " |=> pretty cond
  indentedBlock $ newlinePrefixed [branch "then " t, branch "else " f]
  where
    branch :: String -> LHsExpr GhcPs -> Printer ()
    branch str e =
      case e of
        (L _ (HsDo _ DoExpr {} xs))  -> doStmt "do" xs
        (L _ (HsDo _ MDoExpr {} xs)) -> doStmt "mdo" xs
        _                            -> string str |=> pretty e
      where
        doStmt pref stmts = do
          string $ str ++ pref
          newline
          indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
prettyHsExpr (HsMultiIf _ guards) =
  string "if " |=>
  lined (fmap (pretty . fmap (GRHSExpr GRHSExprMultiWayIf)) guards)
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (HsLet _ _ binds _ exprs) = pretty $ LetIn binds exprs
#else
prettyHsExpr (HsLet _ binds exprs) = pretty $ LetIn binds exprs
#endif
prettyHsExpr (HsDo _ ListComp {} (L _ [])) =
  error "Not enough arguments are passed to pretty-print a list comprehension."
prettyHsExpr (HsDo _ ListComp {} (L l (lhs:rhs))) =
  pretty $ L l $ ListComprehension lhs rhs
-- While the name contains 'Monad', 'MonadComp' is for list comprehensions.
prettyHsExpr (HsDo _ MonadComp {} (L _ [])) =
  error "Not enough arguments are passed to pretty-print a list comprehension."
prettyHsExpr (HsDo _ MonadComp {} (L l (lhs:rhs))) =
  pretty $ L l $ ListComprehension lhs rhs
prettyHsExpr (HsDo _ DoExpr {} (L l xs)) = pretty $ L l $ DoExpression xs Do
prettyHsExpr (HsDo _ MDoExpr {} (L l xs)) = pretty $ L l $ DoExpression xs Mdo
prettyHsExpr (HsDo _ GhciStmtCtxt {} _) = error "We're not using GHCi, are we?"
prettyHsExpr (ExplicitList _ xs) = horizontal <-|> vertical
  where
    horizontal = brackets $ hCommaSep $ fmap pretty xs
    vertical = vList $ fmap pretty xs
prettyHsExpr (RecordCon _ name fields) = horizontal <-|> vertical
  where
    horizontal = spaced [pretty name, pretty fields]
    vertical = do
      pretty name
      (space >> pretty fields) <-|> (newline >> indentedBlock (pretty fields))
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (RecordUpd _ name fields) = hor <-|> ver
  where
    hor = spaced [pretty name, either printHorFields printHorFields fields]
    ver = do
      pretty name
      newline
      indentedBlock $
        either printHorFields printHorFields fields <-|>
        either printVerFields printVerFields fields
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
prettyHsExpr (RecordUpd _ name fields) = hor <-|> ver
  where
    hor = spaced [pretty name, either printHorFields printHorFields fields]
    ver = do
      pretty name
      newline
      indentedBlock $
        either printHorFields printHorFields fields <-|>
        either printVerFields printVerFields fields
    printHorFields ::
         (Pretty a, Pretty b, CommentExtraction l)
      => [GenLocated l (HsRecField' a b)]
      -> Printer ()
    printHorFields = hFields . fmap (`printCommentsAnd` horField)
    printVerFields ::
         (Pretty a, Pretty b, CommentExtraction l)
      => [GenLocated l (HsRecField' a b)]
      -> Printer ()
    printVerFields = vFields . fmap printField
    printField x = printCommentsAnd x $ (<-|>) <$> horField <*> verField
    horField HsRecField {..} = do
      pretty hsRecFieldLbl
      string " = "
      pretty hsRecFieldArg
    verField HsRecField {..} = do
      pretty hsRecFieldLbl
      string " ="
      newline
      indentedBlock $ pretty hsRecFieldArg
#endif
prettyHsExpr (HsGetField _ e f) = do
  pretty e
  dot
  pretty f
prettyHsExpr HsProjection {..} =
  parens $
  forM_ proj_flds $ \x -> do
    string "."
    pretty x
prettyHsExpr (ExprWithTySig _ e sig) = do
  pretty e
  string " :: "
  pretty $ hswc_body sig
prettyHsExpr (ArithSeq _ _ x) = pretty x
prettyHsExpr (HsSpliceE _ x) = pretty x
prettyHsExpr (HsProc _ pat x@(L _ (HsCmdTop _ (L _ (HsCmdDo _ xs))))) = do
  spaced [string "proc", pretty pat, string "-> do"]
  newline
  indentedBlock $
    printCommentsAnd x (const (printCommentsAnd xs (lined . fmap pretty)))
prettyHsExpr (HsProc _ pat body) = hor <-|> ver
  where
    hor = spaced [string "proc", pretty pat, string "->", pretty body]
    ver = do
      spaced [string "proc", pretty pat, string "->"]
      newline
      indentedBlock (pretty body)
prettyHsExpr (HsStatic _ x) = spaced [string "static", pretty x]
prettyHsExpr (HsPragE _ p x) = spaced [pretty p, pretty x]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr HsRecSel {} = notGeneratedByParser
prettyHsExpr (HsTypedBracket _ inner) = typedBrackets $ pretty inner
prettyHsExpr (HsUntypedBracket _ inner) = pretty inner
#else
prettyHsExpr HsConLikeOut {} = notGeneratedByParser
prettyHsExpr HsRecFld {} = notGeneratedByParser
prettyHsExpr (HsDo _ ArrowExpr {} _) = notGeneratedByParser
prettyHsExpr (HsDo _ PatGuard {} _) = notGeneratedByParser
prettyHsExpr (HsDo _ ParStmtCtxt {} _) = notGeneratedByParser
prettyHsExpr (HsDo _ TransStmtCtxt {} _) = notGeneratedByParser
prettyHsExpr HsTick {} = forHpc
prettyHsExpr HsBinTick {} = forHpc
prettyHsExpr (HsBracket _ inner) = pretty inner
prettyHsExpr HsRnBracketOut {} = notGeneratedByParser
prettyHsExpr HsTcBracketOut {} = notGeneratedByParser
#endif
instance Pretty LambdaCase where
  pretty' (LambdaCase matches caseOrCases) = do
    case caseOrCases of
      Case  -> string "\\case"
      Cases -> string "\\cases"
    if null $ unLoc $ mg_alts matches
      then string " {}"
      else do
        newline
        indentedBlock $ pretty matches

instance Pretty (HsSigType GhcPs) where
  pretty' = pretty' . HsSigType' HsTypeForNormalDecl HsTypeNoDir

instance Pretty HsSigType' where
  pretty' (HsSigTypeInsideDeclSig HsSig {..}) =
    case sig_bndrs of
      HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        case unLoc sig_body of
          HsQualTy {..} ->
            printCommentsAnd sig_body $ \_ ->
              let hor = do
                    space
                    pretty $ HorizontalContext hst_ctxt
                  ver = do
                    newline
                    pretty $ VerticalContext hst_ctxt
               in do hor <-|> ver
                     newline
                     prefixed "=> " $
                       prefixedLined "-> " $ pretty <$> flatten hst_body
          _ ->
            let hor = space >> pretty (fmap HsTypeInsideDeclSig sig_body)
                ver =
                  newline >> prefixedLined "-> " (pretty <$> flatten sig_body)
             in hor <-|> ver
      _ -> pretty $ fmap HsTypeInsideDeclSig sig_body
    where
      flatten :: LHsType GhcPs -> [LHsType GhcPs]
      flatten (L _ (HsFunTy _ _ l r)) = flatten l ++ flatten r
      flatten x                       = [x]
  pretty' (HsSigTypeInsideVerticalFuncSig HsSig {..}) =
    case sig_bndrs of
      HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        printCommentsAnd sig_body $ \case
          HsQualTy {..} -> do
            (space >> pretty (HorizontalContext hst_ctxt)) <-|>
              (newline >> pretty (VerticalContext hst_ctxt))
            newline
            prefixed "=> " $ pretty hst_body
          x -> pretty $ HsTypeInsideDeclSig x
      _ -> pretty $ fmap HsTypeInsideDeclSig sig_body
  pretty' (HsSigType' for dir HsSig {..}) = do
    case sig_bndrs of
      HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        space
      _ -> return ()
    pretty $ HsType' for dir <$> sig_body

instance Pretty (ConDecl GhcPs) where
  pretty' = prettyConDecl

prettyConDecl :: ConDecl GhcPs -> Printer ()
prettyConDecl ConDeclGADT {..} = do
  hCommaSep $ fmap pretty con_names
  hor <-|> ver
  where
    hor = string " :: " |=> body
    ver = do
      newline
      indentedBlock (string ":: " |=> body)
    body =
      case (forallNeeded, con_mb_cxt) of
        (True, Just ctx)  -> withForallCtx ctx
        (True, Nothing)   -> withForallOnly
        (False, Just ctx) -> withCtxOnly ctx
        (False, Nothing)  -> noForallCtx
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
      (pretty (Context ctx) >> string " => " >> horArgs) <-|>
      (pretty (Context ctx) >> prefixed "=> " verArgs)

    horArgs =
      case con_g_args of
        PrefixConGADT xs ->
          inter (string " -> ") $
          fmap (\(HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        RecConGADT xs _ -> inter (string " -> ") [recArg xs, pretty con_res_ty]

    verArgs =
      case con_g_args of
        PrefixConGADT xs ->
          prefixedLined "-> " $
          fmap (\(HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        RecConGADT xs _ -> prefixedLined "-> " [recArg xs, pretty con_res_ty]
#else
    withForallCtx _ = do
      pretty con_bndrs
      (space >> pretty (Context con_mb_cxt)) <-|>
        (newline >> pretty (Context con_mb_cxt))
      newline
      prefixed "=> " verArgs

    withCtxOnly _ =
      (pretty (Context con_mb_cxt) >> string " => " >> horArgs) <-|>
      (pretty (Context con_mb_cxt) >> prefixed "=> " verArgs)

    horArgs =
      case con_g_args of
        PrefixConGADT xs ->
          inter (string " -> ") $
          fmap (\(HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        RecConGADT xs -> inter (string " -> ") [recArg xs, pretty con_res_ty]

    verArgs =
      case con_g_args of
        PrefixConGADT xs ->
          prefixedLined "-> " $
          fmap (\(HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        RecConGADT xs -> prefixedLined "-> " [recArg xs, pretty con_res_ty]
#endif
    recArg xs = printCommentsAnd xs $ \xs' -> vFields' $ fmap pretty xs'

    forallNeeded =
      case unLoc con_bndrs of
        HsOuterImplicit {} -> False
        HsOuterExplicit {} -> True
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyConDecl ConDeclH98 {con_forall = True, ..} =
  (do string "forall "
      spaced $ fmap pretty con_ex_tvs
      string ". ") |=>
  (do whenJust con_mb_cxt $ \c -> do
        pretty $ Context c
        string " =>"
        newline
      pretty con_name
      pretty con_args)
#else
prettyConDecl ConDeclH98 {con_forall = True, ..} =
  (do string "forall "
      spaced $ fmap pretty con_ex_tvs
      string ". ") |=>
  (do whenJust con_mb_cxt $ \_ -> do
        pretty $ Context con_mb_cxt
        string " =>"
        newline
      pretty con_name
      pretty con_args)
#endif
prettyConDecl ConDeclH98 {con_forall = False, ..} =
  case con_args of
    (InfixCon l r) ->
      spaced [pretty l, pretty $ fmap InfixOp con_name, pretty r]
    _ -> do
      pretty con_name
      pretty con_args

instance Pretty (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' = prettyMatchExpr

prettyMatchExpr :: Match GhcPs (LHsExpr GhcPs) -> Printer ()
prettyMatchExpr Match {m_ctxt = LambdaExpr, ..} = do
  string "\\"
  unless (null m_pats) $
    case unLoc $ head m_pats of
      LazyPat {} -> space
      BangPat {} -> space
      _          -> return ()
  spaced $ fmap pretty m_pats
  pretty $ GRHSsExpr GRHSExprLambda m_grhss
prettyMatchExpr Match {m_ctxt = CaseAlt, ..} = do
  mapM_ pretty m_pats
  pretty $ GRHSsExpr GRHSExprCase m_grhss
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyMatchExpr Match {m_ctxt = LamCaseAlt {}, ..} = do
  spaced $ fmap pretty m_pats
  pretty $ GRHSsExpr GRHSExprCase m_grhss
#endif
prettyMatchExpr Match {..} =
  case mc_fixity m_ctxt of
    Prefix -> do
      pretty m_ctxt
      spacePrefixed $ fmap pretty m_pats
      pretty m_grhss
    Infix -> do
      case (m_pats, m_ctxt) of
        (l:r:xs, FunRhs {..}) -> do
          spaced $
            [pretty l, pretty $ fmap InfixOp mc_fun, pretty r] ++ fmap pretty xs
          pretty m_grhss
        _ -> error "Not enough parameters are passed."

instance Pretty (Match GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs))) where
  pretty' = prettyMatchProc

prettyMatchProc :: Match GhcPs (LHsCmd GhcPs) -> Printer ()
prettyMatchProc Match {m_ctxt = LambdaExpr, ..} = do
  string "\\"
  unless (null m_pats) $
    case unLoc $ head m_pats of
      LazyPat {} -> space
      BangPat {} -> space
      _          -> return ()
  spaced $ fmap pretty m_pats ++ [pretty m_grhss]
prettyMatchProc Match {m_ctxt = CaseAlt, ..} =
  spaced [mapM_ pretty m_pats, pretty m_grhss]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyMatchProc Match {m_ctxt = LamCaseAlt {}, ..} = do
  spaced [mapM_ pretty m_pats, pretty m_grhss]
#endif
prettyMatchProc _ = notGeneratedByParser

instance Pretty (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' (LastStmt _ x _ _) = pretty x
  pretty' (BindStmt _ pat body) = do
    pretty pat
    string " <-"
    hor <-|> ver
    where
      hor = space >> pretty body
      ver = newline >> indentedBlock (pretty body)
  pretty' ApplicativeStmt {} = notGeneratedByParser
  pretty' (BodyStmt _ (L loc (OpApp _ l o r)) _ _) =
    pretty (L loc (InfixApp l o r True))
  pretty' (BodyStmt _ body _ _) = pretty body
  pretty' (LetStmt _ l) = string "let " |=> pretty l
  pretty' (ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
  pretty' TransStmt {..} =
    vCommaSep $ fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
  pretty' RecStmt {..} =
    string "rec " |=> printCommentsAnd recS_stmts (lined . fmap pretty)

instance Pretty (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs))) where
  pretty' (LastStmt _ x _ _) = pretty x
  pretty' (BindStmt _ pat body) = hor <-|> ver
    where
      hor = spaced [pretty pat, string "<-", pretty body]
      ver = do
        pretty pat
        string " <-"
        newline
        indentedBlock $ pretty body
  pretty' ApplicativeStmt {} = notGeneratedByParser
  pretty' (BodyStmt _ body _ _) = pretty body
  pretty' (LetStmt _ l) = string "let " |=> pretty l
  pretty' (ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
  pretty' TransStmt {..} =
    vCommaSep $ fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
  pretty' RecStmt {..} =
    string "rec " |=> printCommentsAnd recS_stmts (lined . fmap pretty)

instance Pretty StmtLRInsideVerticalList where
  pretty' (StmtLRInsideVerticalList (ParStmt _ xs _ _)) =
    vBarSep $ fmap (pretty . ParStmtBlockInsideVerticalList) xs
  pretty' (StmtLRInsideVerticalList x) = pretty x

-- | For pattern matching.
instance Pretty (HsRecFields GhcPs (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  pretty' HsRecFields {..} = horizontal <-|> vertical
    where
      horizontal =
        case rec_dotdot of
          Just _  -> braces $ string ".."
          Nothing -> hFields $ fmap pretty rec_flds
      vertical = vFields $ fmap pretty rec_flds

-- | For record updates
instance Pretty (HsRecFields GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' HsRecFields {..} = hvFields fieldPrinters
    where
      fieldPrinters =
        fmap pretty rec_flds ++
        maybeToList (fmap (const (string "..")) rec_dotdot)

instance Pretty (HsType GhcPs) where
  pretty' = pretty' . HsType' HsTypeForNormalDecl HsTypeNoDir

instance Pretty HsType' where
  pretty' (HsTypeInsideVerticalFuncSig (HsFunTy _ _ a b)) = do
    pretty $ HsTypeInsideVerticalFuncSig <$> a
    newline
    prefixed "-> " $ pretty $ HsTypeInsideVerticalFuncSig <$> b
  pretty' (HsTypeInsideDeclSig HsQualTy {..}) = hor <-|> ver
    where
      hor = spaced [pretty $ Context hst_ctxt, string "=>", pretty hst_body]
      ver = do
        pretty $ Context hst_ctxt
        newline
        prefixed "=> " $ pretty $ fmap HsTypeInsideVerticalFuncSig hst_body
  pretty' (HsTypeInsideDeclSig (HsFunTy _ _ a b)) = hor <-|> ver
    where
      hor = spaced [pretty a, string "->", pretty b]
      ver = do
        pretty $ fmap HsTypeInsideVerticalFuncSig a
        newline
        prefixed "-> " $ pretty $ fmap HsTypeInsideVerticalFuncSig b
  pretty' (HsTypeInsideInstDecl HsQualTy {..}) = hor <-|> ver
    where
      hor = spaced [pretty (Context hst_ctxt), string "=>", pretty hst_body]
      ver = do
        pretty (Context hst_ctxt)
        string " =>"
        newline
        pretty hst_body
  pretty' (HsType' _ _ x) = prettyHsType x

prettyHsType :: HsType GhcPs -> Printer ()
prettyHsType (HsForAllTy _ tele body) = (pretty tele >> space) |=> pretty body
prettyHsType HsQualTy {..} = do
  pretty $ Context hst_ctxt
  lined [string " =>", indentedBlock $ pretty hst_body]
prettyHsType (HsTyVar _ NotPromoted x) = pretty x
prettyHsType (HsTyVar _ IsPromoted x) = string "'" >> pretty x
prettyHsType (HsAppTy _ l r) = spaced $ fmap pretty [l, r]
prettyHsType (HsAppKindTy _ l r) = pretty l >> string " @" >> pretty r
prettyHsType (HsFunTy _ _ a b) = (pretty a >> string " -> ") |=> pretty b
prettyHsType (HsListTy _ xs) = brackets $ pretty xs
prettyHsType (HsTupleTy _ _ []) = string "()"
prettyHsType (HsTupleTy _ _ xs) = hvTuple' $ fmap pretty xs
prettyHsType (HsSumTy _ xs) = unboxedSums $ hBarSep $ fmap pretty xs
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
prettyHsType (HsOpTy _ l op r) = do
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
prettyHsType (HsParTy _ inside) = parens $ pretty inside
prettyHsType (HsIParamTy _ x ty) =
  spaced [string "?" >> pretty x, string "::", pretty ty]
prettyHsType HsStarTy {} = string "*"
prettyHsType (HsKindSig _ t k) = spaced [pretty t, string "::", pretty k]
prettyHsType (HsSpliceTy _ sp) = pretty sp
prettyHsType HsDocTy {} = docNode
prettyHsType (HsBangTy _ pack x) = pretty pack >> pretty x
prettyHsType (HsRecTy _ xs) = hvFields $ fmap pretty xs
prettyHsType (HsExplicitListTy _ _ xs) =
  case xs of
    [] -> string "'[]"
    _  -> hPromotedList $ fmap pretty xs
prettyHsType (HsExplicitTupleTy _ xs) = hPromotedTuple $ fmap pretty xs
prettyHsType (HsTyLit _ x) = pretty x
prettyHsType HsWildCardTy {} = string "_"
prettyHsType XHsType {} = notGeneratedByParser

instance Pretty (GRHSs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' = pretty' . GRHSsExpr GRHSExprNormal

instance Pretty GRHSsExpr where
  pretty' (GRHSsExpr {grhssExpr = GRHSs {..}, ..}) = do
    mapM_ (pretty . fmap (GRHSExpr grhssExprType)) grhssGRHSs
    case (grhssLocalBinds, grhssExprType) of
      (HsValBinds {}, GRHSExprCase) ->
        indentedBlock $ do
          newline
          string "where " |=> pretty grhssLocalBinds
      (HsValBinds epa lr, _) ->
        indentedWithSpace 2 $
        newlinePrefixed
          [ string "where"
          , printCommentsAnd (L epa lr) (indentedWithSpace 2 . pretty)
          ]
      _ -> return ()

instance Pretty (GRHSs GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs))) where
  pretty' GRHSs {..} = do
    mapM_ (pretty . fmap GRHSProc) grhssGRHSs
    case grhssLocalBinds of
      (HsValBinds epa lr) ->
        indentedWithSpace 2 $
        newlinePrefixed
          [ string "where"
          , printCommentsAnd (L epa lr) (indentedWithSpace 2 . pretty)
          ]
      _ -> return ()

instance Pretty (HsMatchContext GhcPs) where
  pretty' = prettyHsMatchContext

prettyHsMatchContext :: HsMatchContext GhcPs -> Printer ()
prettyHsMatchContext FunRhs {..}       = pretty mc_strictness >> pretty mc_fun
prettyHsMatchContext LambdaExpr        = return ()
prettyHsMatchContext CaseAlt           = return ()
prettyHsMatchContext IfAlt {}          = notGeneratedByParser
prettyHsMatchContext ArrowMatchCtxt {} = notGeneratedByParser
prettyHsMatchContext PatBindRhs {}     = notGeneratedByParser
prettyHsMatchContext PatBindGuards {}  = notGeneratedByParser
prettyHsMatchContext RecUpd {}         = notGeneratedByParser
prettyHsMatchContext StmtCtxt {}       = notGeneratedByParser
prettyHsMatchContext ThPatSplice {}    = notGeneratedByParser
prettyHsMatchContext ThPatQuote {}     = notGeneratedByParser
prettyHsMatchContext PatSyn {}         = notGeneratedByParser
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsMatchContext LamCaseAlt {}     = notUsedInParsedStage
#endif
instance Pretty (ParStmtBlock GhcPs GhcPs) where
  pretty' (ParStmtBlock _ xs _ _) = hvCommaSep $ fmap pretty xs

instance Pretty ParStmtBlockInsideVerticalList where
  pretty' (ParStmtBlockInsideVerticalList (ParStmtBlock _ xs _ _)) =
    vCommaSep $ fmap pretty xs

instance Pretty RdrName where
  pretty' = pretty . PrefixOp

instance Pretty (GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' = pretty' . GRHSExpr GRHSExprNormal

instance Pretty GRHSExpr where
  pretty' (GRHSExpr {grhsExpr = (GRHS _ [] body), ..}) = do
    space
    rhsSeparator grhsExprType
    case unLoc body of
      HsDo _ DoExpr {} stmts ->
        printCommentsAnd body (const (doExpr "do" stmts))
      HsDo _ MDoExpr {} stmts ->
        printCommentsAnd body (const (doExpr "mdo" stmts))
      _ ->
        let hor = space >> pretty body
            ver = newline >> indentedBlock (pretty body)
         in hor <-|> ver
    where
      doExpr pref stmts = do
        space
        string pref
        newline
        indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
  pretty' (GRHSExpr {grhsExpr = (GRHS _ guards body), ..}) = do
    unless (grhsExprType == GRHSExprMultiWayIf) newline
    (if grhsExprType == GRHSExprMultiWayIf
       then id
       else indentedBlock) $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      space
      rhsSeparator grhsExprType
      printCommentsAnd body $ \case
        HsDo _ DoExpr {} stmts -> doExpr "do" stmts
        HsDo _ MDoExpr {} stmts -> doExpr "mdo" stmts
        x ->
          let hor = space >> pretty x
              ver = newline >> indentedBlock (pretty x)
           in hor <-|> ver
    where
      doExpr pref stmts = do
        space
        string pref
        let hor = space >> printCommentsAnd stmts (lined . fmap pretty)
            ver = do
              newline
              indentedBlock (printCommentsAnd stmts (lined . fmap pretty))
        hor <-|> ver

instance Pretty GRHSProc where
  pretty' (GRHSProc (GRHS _ guards body)) =
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
          HsCmdDo _ stmts ->
            let hor = space >> printCommentsAnd stmts (lined . fmap pretty)
                ver = do
                  newline
                  indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
             in hor <-|> ver
          x ->
            let hor = space >> pretty x
                ver = newline >> indentedBlock (pretty x)
             in hor <-|> ver

instance Pretty EpaCommentTok where
  pretty' (EpaLineComment c) = string c
  pretty' (EpaBlockComment c) =
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

instance Pretty (SpliceDecl GhcPs) where
  pretty' (SpliceDecl _ sp _) = pretty sp

instance Pretty (HsSplice GhcPs) where
  pretty' (HsTypedSplice _ _ _ body) = string "$$" >> pretty body
  pretty' (HsUntypedSplice _ DollarSplice _ body) = string "$" >> pretty body
  pretty' (HsUntypedSplice _ BareSplice _ body) = pretty body
  -- The body of a quasi-quote must not be changed by a formatter.
  -- Changing it will modify the actual behavior of the code.
  pretty' (HsQuasiQuote _ _ l _ r) =
    brackets $ do
      pretty l
      wrapWithBars $
        indentedWithFixedLevel 0 $ sequence_ $ printers [] "" $ unpackFS r
    where
      printers ps s [] = reverse (string (reverse s) : ps)
      printers ps s ('\n':xs) =
        printers (newline : string (reverse s) : ps) "" xs
      printers ps s (x:xs) = printers ps (x : s) xs
  pretty' HsSpliced {} = notGeneratedByParser

instance Pretty (Pat GhcPs) where
  pretty' = prettyPat

instance Pretty PatInsidePatDecl where
  pretty' (PatInsidePatDecl (ConPat {pat_args = (InfixCon l r), ..})) =
    spaced [pretty l, pretty $ fmap InfixOp pat_con, pretty r]
  pretty' (PatInsidePatDecl x) = pretty x

prettyPat :: Pat GhcPs -> Printer ()
prettyPat WildPat {} = string "_"
prettyPat (VarPat _ x) = pretty x
prettyPat (LazyPat _ x) = string "~" >> pretty x
prettyPat (AsPat _ a b) = pretty a >> string "@" >> pretty b
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyPat (ParPat _ _ inner _) = parens $ pretty inner
#else
prettyPat (ParPat _ inner) = parens $ pretty inner
#endif
prettyPat (BangPat _ x) = string "!" >> pretty x
prettyPat (ListPat _ xs) = hList $ fmap pretty xs
prettyPat (TuplePat _ pats _) = hTuple $ fmap pretty pats
prettyPat (SumPat _ x position numElem) = do
  string "(#"
  forM_ [1 .. numElem] $ \idx -> do
    if idx == position
      then string " " >> pretty x >> string " "
      else string " "
    when (idx < numElem) $ string "|"
  string "#)"
prettyPat ConPat {..} =
  case pat_args of
    PrefixCon _ as -> do
      pretty $ fmap PrefixOp pat_con
      spacePrefixed $ fmap pretty as
    RecCon rec -> (pretty pat_con >> space) |=> pretty (RecConPat rec)
    InfixCon a b -> do
      pretty a
      unlessSpecialOp (unLoc pat_con) space
      pretty $ fmap InfixOp pat_con
      unlessSpecialOp (unLoc pat_con) space
      pretty b
prettyPat (ViewPat _ l r) = spaced [pretty l, string "->", pretty r]
prettyPat (SplicePat _ x) = pretty x
prettyPat (LitPat _ x) = pretty x
prettyPat (NPat _ x _ _) = pretty x
prettyPat (NPlusKPat _ n k _ _ _) = pretty n >> string "+" >> pretty k
prettyPat (SigPat _ l r) = spaced [pretty l, string "::", pretty r]

instance Pretty RecConPat where
  pretty' (RecConPat HsRecFields {..}) =
    case fieldPrinters of
      []  -> string "{}"
      [x] -> braces x
      xs  -> hvFields xs
    where
      fieldPrinters =
        fmap (pretty . fmap RecConField) rec_flds ++
        maybeToList (fmap (const (string "..")) rec_dotdot)
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (HsBracket GhcPs) where
  pretty' (ExpBr _ expr) = brackets $ wrapWithBars $ pretty expr
  pretty' (PatBr _ expr) = brackets $ string "p" >> wrapWithBars (pretty expr)
  pretty' (DecBrL _ decls) =
    brackets $ string "d| " |=> lined (fmap pretty decls) >> string " |"
  pretty' DecBrG {} = notGeneratedByParser
  pretty' (TypBr _ expr) = brackets $ string "t" >> wrapWithBars (pretty expr)
  pretty' (VarBr _ True var) = string "'" >> pretty var
  pretty' (VarBr _ False var) = string "''" >> pretty var
  pretty' (TExpBr _ x) = typedBrackets $ pretty x
#endif
instance Pretty SigBindFamily where
  pretty' (Sig x)        = pretty $ DeclSig x
  pretty' (Bind x)       = pretty x
  pretty' (TypeFamily x) = pretty x
  pretty' (TyFamInst x)  = pretty x

instance Pretty EpaComment where
  pretty' EpaComment {..} = pretty ac_tok

instance Pretty (HsLocalBindsLR GhcPs GhcPs) where
  pretty' (HsValBinds _ lr) = pretty lr
  pretty' (HsIPBinds _ x) = pretty x
  pretty' EmptyLocalBinds {} =
    error
      "This branch indicates that the bind is empty, but since calling this code means that let or where has already been output, it cannot be handled here. It should be handled higher up in the AST."

instance Pretty (HsValBindsLR GhcPs GhcPs) where
  pretty' (ValBinds _ methods sigs) = lined $ fmap pretty sigsAndMethods
    where
      sigsAndMethods = mkSortedLSigBindFamilyList sigs (bagToList methods) [] []
  pretty' XValBindsLR {} = notUsedInParsedStage

instance Pretty (HsTupArg GhcPs) where
  pretty' (Present _ e) = pretty e
  pretty' Missing {}    = pure () -- This appears in a tuple section.
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty RecConField where
  pretty' (RecConField HsFieldBind {..}) = do
    pretty hfbLHS
    unless hfbPun $ do
      string " = "
      pretty hfbRHS
#else
-- | For pattern matching against a record.
instance Pretty (HsRecField' (FieldOcc GhcPs) (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  pretty' HsRecField {..} =
    (pretty hsRecFieldLbl >> string " = ") |=> pretty hsRecFieldArg

-- | For record updates.
instance Pretty (HsRecField' (FieldOcc GhcPs) (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' HsRecField {..} = do
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
instance Pretty (HsFieldBind (GenLocated (SrcAnn NoEpAnns) (FieldOcc GhcPs)) (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  pretty' HsFieldBind {..} = (pretty hfbLHS >> string " = ") |=> pretty hfbRHS

-- | For record updates.
instance Pretty (HsFieldBind (GenLocated (SrcAnn NoEpAnns) (FieldOcc GhcPs)) (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
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
  pretty' (RecConField HsRecField {..}) = do
    pretty hsRecFieldLbl
    unless hsRecPun $ do
      string " = "
      pretty hsRecFieldArg
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (FieldOcc GhcPs) where
  pretty' FieldOcc {..} = pretty foLabel
#else
instance Pretty (FieldOcc GhcPs) where
  pretty' FieldOcc {..} = pretty rdrNameFieldOcc
#endif
-- HsConDeclH98Details
instance Pretty (HsConDetails Void (HsScaled GhcPs (GenLocated SrcSpanAnnA (BangType GhcPs))) (GenLocated SrcSpanAnnL [GenLocated SrcSpanAnnA (ConDeclField GhcPs)])) where
  pretty' (PrefixCon _ xs) = horizontal <-|> vertical
    where
      horizontal = spacePrefixed $ fmap pretty xs
      vertical = indentedBlock $ newlinePrefixed $ fmap pretty xs
  pretty' (RecCon x) =
    printCommentsAnd x $ \rec -> do
      newline
      indentedBlock $ vFields $ fmap pretty rec
  pretty' InfixCon {} =
    error
      "Cannot handle here because 'InfixCon' does not have the information of its constructor."

instance Pretty a => Pretty (HsScaled GhcPs a) where
  pretty' (HsScaled _ x) = pretty x

instance Pretty (ConDeclField GhcPs) where
  pretty' ConDeclField {..}
    -- Here, we *ignore* the 'cd_fld_doc' field because doc strings are
    -- also stored as comments, and printing both results in duplicated
    -- comments.
   = do
    hCommaSep $ fmap pretty cd_fld_names
    string " :: "
    pretty cd_fld_type

instance Pretty InfixExpr where
  pretty' (InfixExpr (L _ (HsVar _ bind))) = pretty $ fmap InfixOp bind
  pretty' (InfixExpr x)                    = pretty' x

instance Pretty InfixApp where
  pretty' InfixApp {..} = horizontal <-|> vertical
    where
      horizontal = spaced [pretty lhs, pretty (InfixExpr op), pretty rhs]
      vertical = do
        lhsVer
        beforeRhs <-
          case unLoc lhs of
            (HsDo _ DoExpr {} _) -> do
              indentedWithSpace 3 (newline >> pretty (InfixExpr op)) -- 3 for "do "
              return space
            (HsDo _ MDoExpr {} _) -> do
              indentedWithSpace 4 (newline >> pretty (InfixExpr op)) -- 4 for "mdo "
              return space
            _ -> do
              space
              pretty (InfixExpr op)
              return newline
        case unLoc rhs of
          (HsDo _ (DoExpr _) xs) -> do
            string " do"
            newline
            indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
          (HsDo _ (MDoExpr _) xs) -> do
            string " mdo"
            newline
            indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
          HsLam {} -> do
            space
            pretty rhs
          HsLamCase {} -> do
            space
            pretty rhs
          _ ->
            (if immediatelyAfterDo
               then indentedBlock
               else id) $ do
              beforeRhs
              col <- startingColumn
              (if col == 0
                 then indentedBlock
                 else id) $
                pretty rhs
      lhsVer =
        case lhs of
          (L loc (OpApp _ l o r)) ->
            pretty (L loc (InfixApp l o r immediatelyAfterDo))
          _ -> pretty lhs

instance Pretty a => Pretty (BooleanFormula a) where
  pretty' (Var x)    = pretty x
  pretty' (And xs)   = hvCommaSep $ fmap pretty xs
  pretty' (Or xs)    = hvBarSep $ fmap pretty xs
  pretty' (Parens x) = parens $ pretty x

instance Pretty (FieldLabelStrings GhcPs) where
  pretty' (FieldLabelStrings xs) = hDotSep $ fmap pretty xs

instance Pretty (AmbiguousFieldOcc GhcPs) where
  pretty' (Unambiguous _ name) = pretty name
  pretty' (Ambiguous _ name)   = pretty name

instance Pretty (ImportDecl GhcPs) where
  pretty' decl@ImportDecl {..} = do
    string "import "
    when (ideclSource == IsBoot) $ string "{-# SOURCE #-} "
    when ideclSafe $ string "safe "
    unless (ideclQualified == NotQualified) $ string "qualified "
    whenJust (packageName decl) $ \x -> do
      pretty x
      space
    pretty ideclName
    whenJust ideclAs $ \x -> do
      string " as "
      pretty x
    whenJust ideclHiding $ \(x, ps) -> do
      when x (string " hiding")
      (string " " >> printCommentsAnd ps (hTuple . fmap pretty)) <-|>
        (newline >> indentedBlock (printCommentsAnd ps (vTuple . fmap pretty)))

packageName :: ImportDecl GhcPs -> Maybe StringLiteral
#if MIN_VERSION_ghc_lib_parser(9,4,1)
packageName (ideclPkgQual -> RawPkgQual name) = Just name
packageName _ = Nothing
#else
packageName = ideclPkgQual
#endif
instance Pretty (HsDerivingClause GhcPs) where
  pretty' HsDerivingClause { deriv_clause_strategy = Just strategy@(L _ ViaStrategy {})
                           , ..
                           } =
    spaced [string "deriving", pretty deriv_clause_tys, pretty strategy]
  pretty' HsDerivingClause {..} = do
    string "deriving "
    whenJust deriv_clause_strategy $ \x -> do
      pretty x
      space
    pretty deriv_clause_tys

instance Pretty (DerivClauseTys GhcPs) where
  pretty' (DctSingle _ ty) = parens $ pretty ty
  pretty' (DctMulti _ ts)  = hvTuple $ fmap pretty ts

instance Pretty OverlapMode where
  pretty' NoOverlap {}    = notUsedInParsedStage
  pretty' Overlappable {} = string "{-# OVERLAPPABLE #-}"
  pretty' Overlapping {}  = string "{-# OVERLAPPING #-}"
  pretty' Overlaps {}     = string "{-# OVERLAPS #-}"
  pretty' Incoherent {}   = string "{-# INCOHERENT #-}"

instance Pretty StringLiteral where
  pretty' = output

-- | This instance is for type family declarations inside a class declaration.
instance Pretty (FamilyDecl GhcPs) where
  pretty' FamilyDecl {..} = do
    string $
      case fdInfo of
        DataFamily          -> "data"
        OpenTypeFamily      -> "type"
        ClosedTypeFamily {} -> "type"
    case fdTopLevel of
      TopLevel    -> string " family "
      NotTopLevel -> space
    pretty fdLName
    spacePrefixed $ pretty <$> hsq_explicit fdTyVars
    case unLoc fdResultSig of
      NoSig {} -> pure ()
      TyVarSig {} -> do
        string " = "
        pretty fdResultSig
      _ -> do
        space
        pretty fdResultSig
    whenJust fdInjectivityAnn $ \x -> do
      string " | "
      pretty x
    case fdInfo of
      ClosedTypeFamily (Just xs) -> do
        string " where"
        newline
        indentedBlock $ lined $ fmap pretty xs
      _ -> pure ()

instance Pretty (FamilyResultSig GhcPs) where
  pretty' NoSig {}       = pure ()
  pretty' (KindSig _ x)  = string ":: " >> pretty x
  pretty' (TyVarSig _ x) = pretty x

instance Pretty (HsTyVarBndr a GhcPs) where
  pretty' (UserTyVar _ _ x) = pretty x
  pretty' (KindedTyVar _ _ name ty) =
    parens $ spaced [pretty name, string "::", pretty ty]

instance Pretty (InjectivityAnn GhcPs) where
  pretty' (InjectivityAnn _ from to) =
    spaced $ pretty from : string "->" : fmap pretty to

instance Pretty (ArithSeqInfo GhcPs) where
  pretty' (From from) = brackets $ spaced [pretty from, string ".."]
  pretty' (FromThen from next) =
    brackets $ spaced [pretty from >> comma >> pretty next, string ".."]
  pretty' (FromTo from to) =
    brackets $ spaced [pretty from, string "..", pretty to]
  pretty' (FromThenTo from next to) =
    brackets $
    spaced [pretty from >> comma >> pretty next, string "..", pretty to]

instance Pretty (HsForAllTelescope GhcPs) where
  pretty' HsForAllVis {..} = do
    string "forall "
    spaced $ fmap pretty hsf_vis_bndrs
    dot
  pretty' HsForAllInvis {..} = do
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
  pretty' (PrefixOp (Exact name)) = parensIfSymbol occ $ pretty occ
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
          (L _ [])  -> parens
          (L _ [_]) -> id
          _         -> parens

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
          Nothing        -> id
          Just (L _ [])  -> parens
          Just (L _ [_]) -> id
          Just _         -> parens

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

instance Pretty (IE GhcPs) where
  pretty' (IEVar _ name) = pretty name
  pretty' (IEThingAbs _ name) = pretty name
  pretty' (IEThingAll _ name) = do
    pretty name
    string "(..)"
  -- FIXME: Currently, pretty-printing a 'IEThingWith' uses
  -- 'ghc-lib-parser''s pretty-printer. However, we should avoid it because
  -- 'ghc-lib-parser' may suddenly change how it prints, resulting in
  -- unexpected test failures.
  pretty' x@IEThingWith {} =
    case lines $ showOutputable x of
      [] -> pure ()
      [x'] -> string x'
      xs -> do
        string $ head xs
        indentedWithFixedLevel 0 $ newlinePrefixed $ string <$> tail xs
  pretty' (IEModuleContents _ name) = pretty $ fmap ModuleNameWithPrefix name
  pretty' IEGroup {} = docNode
  pretty' IEDoc {} = docNode
  pretty' IEDocNamed {} = docNode

instance Pretty (FamEqn GhcPs (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  pretty' FamEqn {..} = do
    pretty feqn_tycon
    spacePrefixed $ fmap pretty feqn_pats
    string " = "
    pretty feqn_rhs

-- | Pretty-print a data instance.
instance Pretty (FamEqn GhcPs (HsDataDefn GhcPs)) where
  pretty' FamEqn {..} = do
    spaced $ string "data instance" : pretty feqn_tycon : fmap pretty feqn_pats
    pretty feqn_rhs

-- | HsArg (LHsType GhcPs) (LHsType GhcPs)
instance Pretty (HsArg (GenLocated SrcSpanAnnA (HsType GhcPs)) (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  pretty' (HsValArg x)    = pretty x
  pretty' (HsTypeArg _ x) = string "@" >> pretty x
  pretty' HsArgPar {}     = notUsedInParsedStage
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
instance Pretty (WarnDecls GhcPs) where
  pretty' (Warnings _ _ x) = lined $ fmap pretty x

instance Pretty (WarnDecl GhcPs) where
  pretty' (Warning _ names deprecatedOrWarning) =
    case deprecatedOrWarning of
      DeprecatedTxt _ reasons -> prettyWithTitleReasons "DEPRECATED" reasons
      WarningTxt _ reasons    -> prettyWithTitleReasons "WARNING" reasons
    where
      prettyWithTitleReasons title reasons =
        lined
          [ string $ "{-# " ++ title
          , spaced
              [hCommaSep $ fmap pretty names, hCommaSep $ fmap pretty reasons]
          , string " #-}"
          ]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (WithHsDocIdentifiers StringLiteral GhcPs) where
  pretty' WithHsDocIdentifiers {..} = pretty hsDocString
#endif
-- | 'Pretty' for 'LIEWrappedName (IdP GhcPs)'
instance Pretty (IEWrappedName RdrName) where
  pretty' (IEName name)      = pretty name
  pretty' (IEPattern _ name) = spaced [string "pattern", pretty name]
  pretty' (IEType _ name)    = string "type " >> pretty name
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (DotFieldOcc GhcPs) where
  pretty' DotFieldOcc {..} = printCommentsAnd dfoLabel (string . unpackFS)
#else
instance Pretty (HsFieldLabel GhcPs) where
  pretty' HsFieldLabel {..} = printCommentsAnd hflLabel (string . unpackFS)
#endif
instance Pretty (RuleDecls GhcPs) where
  pretty' HsRules {..} =
    lined $ string "{-# RULES" : fmap pretty rds_rules ++ [string " #-}"]

instance Pretty (RuleDecl GhcPs) where
  pretty' HsRule {..} =
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

instance Pretty OccName where
  pretty' = output

instance Pretty (DerivDecl GhcPs) where
  pretty' DerivDecl { deriv_strategy = (Just deriv_strategy@(L _ ViaStrategy {}))
                    , ..
                    } =
    spaced
      [ string "deriving"
      , pretty deriv_strategy
      , string "instance"
      , pretty deriv_type
      ]
  pretty' DerivDecl {..} = do
    string "deriving "
    whenJust deriv_strategy $ \x -> do
      pretty x
      space
    string "instance "
    pretty deriv_type

-- | 'Pretty' for 'LHsSigWcType GhcPs'.
instance Pretty (HsWildCardBndrs GhcPs (GenLocated SrcSpanAnnA (HsSigType GhcPs))) where
  pretty' HsWC {..} = pretty hswc_body

-- | 'Pretty' for 'LHsWcType'
instance Pretty (HsWildCardBndrs GhcPs (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  pretty' HsWC {..} = pretty hswc_body

instance Pretty (StandaloneKindSig GhcPs) where
  pretty' (StandaloneKindSig _ name kind) =
    spaced [string "type", pretty name, string "::", pretty kind]

instance Pretty (DefaultDecl GhcPs) where
  pretty' (DefaultDecl _ xs) =
    spaced [string "default", hTuple $ fmap pretty xs]

instance Pretty (ForeignDecl GhcPs) where
  pretty' ForeignImport {..} =
    spaced
      [ string "foreign import"
      , pretty fd_fi
      , pretty fd_name
      , string "::"
      , pretty fd_sig_ty
      ]
  pretty' ForeignExport {..} =
    spaced
      [ string "foreign export"
      , pretty fd_fe
      , pretty fd_name
      , string "::"
      , pretty fd_sig_ty
      ]

instance Pretty ForeignImport where
  pretty' (CImport conv safety _ _ (L _ (SourceText s))) =
    spaced [pretty conv, pretty safety, string s]
  pretty' (CImport conv safety _ _ _) = spaced [pretty conv, pretty safety]

instance Pretty ForeignExport where
  pretty' (CExport conv (L _ (SourceText s))) = spaced [pretty conv, string s]
  pretty' (CExport conv _)                    = pretty conv

instance Pretty CExportSpec where
  pretty' (CExportStatic _ _ x) = pretty x

instance Pretty Safety where
  pretty' PlaySafe          = string "safe"
  pretty' PlayInterruptible = string "interruptible"
  pretty' PlayRisky         = string "unsafe"

instance Pretty (AnnDecl GhcPs) where
  pretty' (HsAnnotation _ _ (ValueAnnProvenance name) expr) =
    spaced [string "{-# ANN", pretty name, pretty expr, string "#-}"]
  pretty' (HsAnnotation _ _ (TypeAnnProvenance name) expr) =
    spaced [string "{-# ANN type", pretty name, pretty expr, string "#-}"]
  pretty' (HsAnnotation _ _ ModuleAnnProvenance expr) =
    spaced [string "{-# ANN module", pretty expr, string "#-}"]

instance Pretty (RoleAnnotDecl GhcPs) where
  pretty' (RoleAnnotDecl _ name roles) =
    spaced $
    [string "type role", pretty name] ++
    fmap (maybe (string "_") pretty . unLoc) roles

instance Pretty Role where
  pretty' Nominal          = string "nominal"
  pretty' Representational = string "representational"
  pretty' Phantom          = string "phantom"

instance Pretty (TyFamInstDecl GhcPs) where
  pretty' TyFamInstDecl {..} = string "type " >> pretty tfid_eqn

instance Pretty TopLevelTyFamInstDecl where
  pretty' (TopLevelTyFamInstDecl TyFamInstDecl {..}) =
    string "type instance " >> pretty tfid_eqn

instance Pretty (DataFamInstDecl GhcPs) where
  pretty' DataFamInstDecl {..} = pretty dfid_eqn

instance Pretty (PatSynBind GhcPs GhcPs) where
  pretty' PSB {..} = do
    string "pattern "
    case psb_args of
      InfixCon l r -> spaced [pretty l, pretty $ fmap InfixOp psb_id, pretty r]
      PrefixCon _ [] -> pretty psb_id
      _ -> spaced [pretty psb_id, pretty psb_args]
    spacePrefixed [pretty psb_dir, pretty $ fmap PatInsidePatDecl psb_def]
    case psb_dir of
      ExplicitBidirectional matches -> do
        newline
        indentedBlock $ string "where " |=> pretty matches
      _ -> pure ()

-- | 'Pretty' for 'HsPatSynDetails'.
instance Pretty (HsConDetails Void (GenLocated SrcSpanAnnN RdrName) [RecordPatSynField GhcPs]) where
  pretty' (PrefixCon _ xs) = spaced $ fmap pretty xs
  pretty' (RecCon rec) = hFields $ fmap pretty rec
  pretty' InfixCon {} =
    error
      "Cannot handle here because `InfixCon` does not have the information of the constructor."

instance Pretty (FixitySig GhcPs) where
  pretty' (FixitySig _ names fixity) =
    spaced [pretty fixity, hCommaSep $ fmap (pretty . fmap InfixOp) names]

instance Pretty Fixity where
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
      ActiveAfter _ x  -> space >> brackets (string $ show x)
      _                -> pure ()

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
instance Pretty (HsPatSynDir GhcPs) where
  pretty' Unidirectional           = string "<-"
  pretty' ImplicitBidirectional    = string "="
  pretty' ExplicitBidirectional {} = string "<-"

instance Pretty (HsOverLit GhcPs) where
  pretty' OverLit {..} = pretty ol_val

instance Pretty OverLitVal where
  pretty' (HsIntegral x)   = pretty x
  pretty' (HsFractional x) = pretty x
  pretty' (HsIsString _ x) = string $ unpackFS x

instance Pretty IntegralLit where
  pretty' IL {il_text = SourceText s} = string s
  pretty' IL {..}                     = string $ show il_value

instance Pretty FractionalLit where
  pretty' = output

instance Pretty (HsLit GhcPs) where
  pretty' x@(HsChar _ _) = output x
  pretty' x@HsCharPrim {} = output x
  pretty' HsInt {} = notUsedInParsedStage
  pretty' (HsIntPrim _ x) = string $ show x ++ "#"
  pretty' HsWordPrim {} = notUsedInParsedStage
  pretty' HsInt64Prim {} = notUsedInParsedStage
  pretty' HsWord64Prim {} = notUsedInParsedStage
  pretty' HsInteger {} = notUsedInParsedStage
  pretty' HsRat {} = notUsedInParsedStage
  pretty' (HsFloatPrim _ x) = pretty x >> string "#"
  pretty' HsDoublePrim {} = notUsedInParsedStage
  pretty' x =
    case x of
      HsString {}     -> prettyString
      HsStringPrim {} -> prettyString
    where
      prettyString =
        case lines $ showOutputable x of
          [] -> pure ()
          [l] -> string l
          (s:ss) ->
            string "" |=> do
              string s
              newline
              indentedWithSpace (-1) $
                lined $ fmap (string . dropWhile (/= '\\')) ss

instance Pretty (HsPragE GhcPs) where
  pretty' (HsPragSCC _ _ x) = spaced [string "{-# SCC", pretty x, string "#-}"]

instance Pretty HsIPName where
  pretty' (HsIPName x) = string $ unpackFS x

instance Pretty HsTyLit where
  pretty' (HsNumTy _ x)  = string $ show x
  pretty' (HsStrTy _ x)  = string $ ushow x
  pretty' (HsCharTy _ x) = string $ show x

instance Pretty (HsPatSigType GhcPs) where
  pretty' HsPS {..} = pretty hsps_body

instance Pretty (HsIPBinds GhcPs) where
  pretty' (IPBinds _ xs) = lined $ fmap pretty xs

instance Pretty (IPBind GhcPs) where
  pretty' = prettyIPBind

prettyIPBind :: IPBind GhcPs -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyIPBind (IPBind _ l r) =
  spaced [string "?" >> pretty l, string "=", pretty r]
#else
prettyIPBind (IPBind _ (Right _) _) = notUsedInParsedStage
prettyIPBind (IPBind _ (Left l) r) =
  spaced [string "?" >> pretty l, string "=", pretty r]
#endif
instance Pretty (DerivStrategy GhcPs) where
  pretty' StockStrategy {}    = string "stock"
  pretty' AnyclassStrategy {} = string "anyclass"
  pretty' NewtypeStrategy {}  = string "newtype"
  pretty' (ViaStrategy x)     = string "via " >> pretty x

instance Pretty XViaStrategyPs where
  pretty' (XViaStrategyPs _ ty) = pretty ty

instance Pretty (RecordPatSynField GhcPs) where
  pretty' RecordPatSynField {..} = pretty recordPatSynField

instance Pretty (HsCmdTop GhcPs) where
  pretty' (HsCmdTop _ cmd) = pretty cmd

instance Pretty (HsCmd GhcPs) where
  pretty' = prettyHsCmd

prettyHsCmd :: HsCmd GhcPs -> Printer ()
prettyHsCmd (HsCmdArrApp _ f arg HsHigherOrderApp True) =
  spaced [pretty f, string "-<<", pretty arg]
prettyHsCmd (HsCmdArrApp _ f arg HsHigherOrderApp False) =
  spaced [pretty arg, string ">>-", pretty f]
prettyHsCmd (HsCmdArrApp _ f arg HsFirstOrderApp True) =
  spaced [pretty f, string "-<", pretty arg]
prettyHsCmd (HsCmdArrApp _ f arg HsFirstOrderApp False) =
  spaced [pretty arg, string ">-", pretty f]
prettyHsCmd (HsCmdArrForm _ f _ _ args) =
  bananaBrackets $ spaced $ pretty f : fmap pretty args
prettyHsCmd (HsCmdApp _ f arg) = spaced [pretty f, pretty arg]
prettyHsCmd (HsCmdLam _ x) = pretty x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsCmd (HsCmdPar _ _ x _) = parens $ pretty x
#else
prettyHsCmd (HsCmdPar _ x) = parens $ pretty x
#endif
prettyHsCmd (HsCmdCase _ cond arms) = do
  spaced [string "case", pretty cond, string "of"]
  newline
  indentedBlock $ pretty arms
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsCmd (HsCmdLamCase _ _ arms) = do
  string "\\case"
  newline
  indentedBlock $ pretty arms
#else
prettyHsCmd (HsCmdLamCase _ arms) = do
  string "\\case"
  newline
  indentedBlock $ pretty arms
#endif
prettyHsCmd (HsCmdIf _ _ cond t f) = do
  string "if "
  pretty cond
  newline
  indentedBlock $ lined [string "then " >> pretty t, string "else " >> pretty f]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsCmd (HsCmdLet _ _ binds _ expr) =
  lined [string "let " |=> pretty binds, string " in " |=> pretty expr]
#else
prettyHsCmd (HsCmdLet _ binds expr) =
  lined [string "let " |=> pretty binds, string " in " |=> pretty expr]
#endif
prettyHsCmd (HsCmdDo _ stmts) = do
  string "do"
  newline
  indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)

instance Pretty ListComprehension where
  pretty' ListComprehension {..} = horizontal <-|> vertical
    where
      horizontal =
        brackets $
        spaced
          [pretty listCompLhs, string "|", hCommaSep $ fmap pretty listCompRhs]
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
  pretty' DoExpression {..} =
    (string pref >> space) |=> lined (fmap pretty doStmts)
    where
      pref =
        case doOrMdo of
          Do  -> "do"
          Mdo -> "mdo"

instance Pretty LetIn where
  pretty' LetIn {..} =
    lined [string "let " |=> pretty letBinds, string " in " |=> pretty inExpr]

instance Pretty (RuleBndr GhcPs) where
  pretty' (RuleBndr _ name) = pretty name
  pretty' (RuleBndrSig _ name sig) =
    parens $ spaced [pretty name, string "::", pretty sig]

instance Pretty CCallConv where
  pretty' CCallConv          = string "ccall"
  pretty' CApiConv           = string "capi"
  pretty' StdCallConv        = string "stdcall"
  pretty' PrimCallConv       = string "prim"
  pretty' JavaScriptCallConv = string "javascript"

instance Pretty ModuleDeprecatedPragma where
  pretty' (ModuleDeprecatedPragma (WarningTxt _ xs)) =
    spaced [string "{-# WARNING", spaced $ fmap pretty xs, string "#-}"]
  pretty' (ModuleDeprecatedPragma (DeprecatedTxt _ xs)) =
    spaced [string "{-# DEPRECATED", spaced $ fmap pretty xs, string "#-}"]

instance Pretty HsSrcBang where
  pretty' (HsSrcBang _ unpack strictness) = do
    pretty unpack
    unless (unpack == NoSrcUnpack) space
    pretty strictness

instance Pretty SrcUnpackedness where
  pretty' SrcUnpack   = string "{-# UNPACK #-}"
  pretty' SrcNoUnpack = string "{-# NOUNPACK #-}"
  pretty' NoSrcUnpack = pure ()

instance Pretty SrcStrictness where
  pretty' SrcLazy     = string "~"
  pretty' SrcStrict   = string "!"
  pretty' NoSrcStrict = pure ()

instance Pretty (HsOuterSigTyVarBndrs GhcPs) where
  pretty' HsOuterImplicit {} = pure ()
  pretty' HsOuterExplicit {..} = do
    string "forall"
    spacePrefixed $ fmap pretty hso_bndrs
    dot

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
