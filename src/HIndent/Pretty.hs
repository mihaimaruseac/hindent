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
  ( Pretty(..)
  , pretty
  , printCommentsAnd
  ) where

import           Control.Monad
import           Control.Monad.RWS
import           Data.Maybe
import           Data.Void
import qualified GHC.Core.Coercion                                   as GHC
import qualified GHC.Data.Bag                                        as GHC
import HIndent.Ast.Declaration.TypeSynonym.Lhs
import qualified HIndent.Ast.Context
import HIndent.Ast.Declaration.Family.Type.Injectivity
import HIndent.Ast.Type.Variable
import HIndent.Ast.Declaration.Data.GADT.Constructor.Signature
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import HIndent.Ast.Declaration.Data.Header
import HIndent.Ast.Declaration.Data.GADT.Constructor
import qualified GHC.Data.BooleanFormula                             as GHC
import qualified GHC.Data.FastString                                 as GHC
import qualified GHC.Hs                                              as GHC
import           GHC.Stack
import qualified GHC.Types.Basic                                     as GHC
import qualified GHC.Types.Fixity                                    as GHC
import qualified GHC.Types.ForeignCall                               as GHC
import qualified GHC.Types.Name                                      as GHC
import qualified GHC.Types.Name.Reader                               as GHC
import qualified GHC.Types.SourceText                                as GHC
import qualified GHC.Types.SrcLoc                                    as GHC
import qualified GHC.Unit.Module.Warnings                            as GHC
import           HIndent.Applicative
import           HIndent.Ast.Declaration
import           HIndent.Ast.Declaration.Data
import           HIndent.Ast.Declaration.Family.Data
import           HIndent.Ast.Declaration.Family.Type
import           HIndent.Ast.Declaration.Family.Type.ResultSignature
import           HIndent.Ast.Declaration.Instance.Class
import           HIndent.Ast.Declaration.TypeSynonym
import           HIndent.Ast.NodeComments
import HIndent.Ast.Declaration.Data.NewOrData
import           HIndent.Config
import           HIndent.Fixity
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments
import qualified HIndent.Pretty.SigBindFamily                        as SBF
import           HIndent.Pretty.Types
import           HIndent.Printer
import qualified Language.Haskell.GhclibParserEx.GHC.Hs.Expr         as GHC
import           Text.Show.Unicode
#if MIN_VERSION_ghc_lib_parser(9,6,1)
import qualified Data.Foldable                                       as NonEmpty
import qualified GHC.Core.DataCon                                    as GHC
#endif
#if !MIN_VERSION_ghc_lib_parser(9,6,1)
import qualified GHC.Unit                                            as GHC
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
           (fromIntegral $ GHC.srcSpanStartCol $ GHC.anchor $ GHC.getLoc c) $
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

instance Pretty Declaration where
  pretty' (HIndent.Ast.Declaration.DataFamily x)  = pretty x
  pretty' (HIndent.Ast.Declaration.TypeFamily x)  = pretty x
  pretty' (DataDeclaration x)                     = pretty x
  pretty' (HIndent.Ast.Declaration.TypeSynonym x) = pretty x
  pretty' (TyClDecl x)                            = pretty x
  pretty' (HIndent.Ast.Declaration.ClassInstance x)                       = pretty x
  pretty' (InstDecl x)                            = pretty x
  pretty' (DerivDecl x)                           = pretty x
  pretty' (ValDecl x)                             = pretty x
  pretty' (SigDecl x)                             = pretty x
  pretty' (KindSigDecl x)                         = pretty x
  pretty' (DefDecl x)                             = pretty x
  pretty' (ForDecl x)                             = pretty x
  pretty' (WarningDecl x)                         = pretty x
  pretty' (AnnDecl x)                             = pretty x
  pretty' (RuleDecl x)                            = pretty x
  pretty' (SpliceDecl x)                          = pretty x
  pretty' (RoleAnnotDecl x)                       = pretty x

instance Pretty DataDeclaration where
  pretty' GADT {..} = do
    pretty header
    whenJust kind $ \x -> string " :: " >> pretty x
    string " where"
    indentedBlock $ newlinePrefixed $ fmap pretty constructors
  pretty' HIndent.Ast.Declaration.Data.Record {..} = do
    pretty header
    case dd_cons of
      [] -> indentedBlock derivingsAfterNewline
      [x@(GHC.L _ GHC.ConDeclH98 {con_args = GHC.RecCon {}})] -> do
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
      derivingsAfterNewline =
        unless (null dd_derivs) $ newline >> printDerivings
      printDerivings = lined $ fmap pretty dd_derivs

instance Pretty DataFamily where
  pretty' HIndent.Ast.Declaration.Family.Data.DataFamily {..} = do
    string "data "
    when isTopLevel $ string "family "
    string name
    spacePrefixed $ fmap pretty typeVariables
    whenJust signature $ \sig -> space >> pretty sig

instance Pretty TypeFamily where
  pretty' HIndent.Ast.Declaration.Family.Type.TypeFamily {..} = do
    string "type "
    when isTopLevel $ string "family "
    string name
    spacePrefixed $ fmap pretty typeVariables
    case getNode signature of
      ResultSignature GHC.NoSig {}    -> pure ()
      ResultSignature GHC.TyVarSig {} -> string " = " >> pretty signature
      _                               -> space >> pretty signature
    whenJust injectivity $ \x -> string " | " >> pretty x
    whenJust equations $ \xs ->
      string " where" >> newline >> indentedBlock (lined $ fmap pretty xs)

instance Pretty ClassInstance where
  pretty' (HIndent.Ast.Declaration.Instance.Class.ClassInstance GHC.ClsInstDecl {..}) = do
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
        SBF.mkSortedLSigBindFamilyList
          cid_sigs
          (GHC.bagToList cid_binds)
          []
          cid_tyfam_insts
          cid_datafam_insts

instance Pretty TypeSynonym where
  pretty' HIndent.Ast.Declaration.TypeSynonym.TypeSynonym {..} = do
    string "type "
    pretty lhs
    hor <-|> ver
    where
      hor = string " = " >> pretty rhs
      ver = newline >> indentedBlock (string "= " |=> pretty rhs)

instance Pretty TypeSynonymLhs where
  pretty' Prefix {..} = spaced $ pretty name : fmap pretty typeVariables
  pretty' Infix {..} =
    spaced [pretty left, pretty $ fmap InfixOp name, pretty right]

instance Pretty Injectivity where
  pretty' (Injectivity x) = pretty x

instance Pretty ResultSignature where
  pretty' (ResultSignature x) = pretty x

instance Pretty TypeVariable where
  pretty' TypeVariable {kind = Just kind, ..} =
    parens $ prettyWith name string >> string " :: " >> pretty kind
  pretty' TypeVariable {kind = Nothing, ..} = prettyWith name string

instance Pretty GADTConstructor where
  pretty' (GADTConstructor {..}) = do
    hCommaSep $ fmap (`prettyWith` string) names
    hor <-|> ver
    where
      hor = string " :: " |=> body
      ver = newline >> indentedBlock (string ":: " |=> body)
      body =
        case (forallNeeded, con_mb_cxt) of
          (True, Just ctx)  -> withForallCtx ctx
          (True, Nothing)   -> withForallOnly
          (False, Just ctx) -> withCtxOnly ctx
          (False, Nothing)  -> noForallCtx
      withForallCtx ctx = do
        pretty bindings
        (space >> pretty (HIndent.Ast.Context.mkContext <$> fromGenLocated ctx)) <-|>
          (newline >> pretty (HIndent.Ast.Context.mkContext <$> fromGenLocated ctx))
        newline
        prefixed "=> " $ prettyVertically signature
      withForallOnly = do
        pretty bindings
        (space >> prettyHorizontally signature) <-|>
          (newline >> prettyVertically signature)
      withCtxOnly ctx =
        (pretty (HIndent.Ast.Context.mkContext <$> fromGenLocated ctx) >> string " => " >>
         prettyHorizontally signature) <-|>
        (pretty (HIndent.Ast.Context.mkContext <$> fromGenLocated ctx) >>
         prefixed "=> " (prettyVertically signature))
      noForallCtx = prettyHorizontally signature <-|> prettyVertically signature

instance Pretty Header where
  pretty' Header {..} = do
    (pretty newOrData >> space) |=> do
      whenJust context $ \c -> pretty c >> string " =>" >> newline
      pretty name
    spacePrefixed $ fmap pretty typeVariables

instance Pretty Type where
  pretty' (Type x) = pretty x

instance (Pretty a) => Pretty (WithComments a) where
  pretty' WithComments {..} = pretty' node

-- | Prints comments included in the location information and then the
-- AST node body.
prettyWith :: WithComments a -> (a -> Printer ()) -> Printer ()
prettyWith WithComments {..} f = do
  printCommentsBefore' comments
  f node
  printCommentOnSameLine' comments
  printCommentsAfter' comments

-- | Prints comments that are before the given AST node.
printCommentsBefore' :: NodeComments -> Printer ()
printCommentsBefore' p =
  forM_ (commentsBefore p) $ \(GHC.L loc c) -> do
    let col = fromIntegral $ GHC.srcSpanStartCol (GHC.anchor loc) - 1
    indentedWithFixedLevel col $ pretty c
    newline

-- | Prints comments that are on the same line as the given AST node.
printCommentOnSameLine' :: NodeComments -> Printer ()
printCommentOnSameLine' (commentsOnSameLine -> (c:cs)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithFixedLevel
           (fromIntegral $ GHC.srcSpanStartCol $ GHC.anchor $ GHC.getLoc c) $
         spaced $ fmap pretty $ c : cs
    else spacePrefixed $ fmap pretty $ c : cs
  eolCommentsArePrinted
printCommentOnSameLine' _ = return ()

-- | Prints comments that are after the given AST node.
printCommentsAfter' :: NodeComments -> Printer ()
printCommentsAfter' p =
  case commentsAfter p of
    [] -> return ()
    xs -> do
      isThereCommentsOnSameLine <- gets psEolComment
      unless isThereCommentsOnSameLine newline
      forM_ xs $ \(GHC.L loc c) -> do
        let col = fromIntegral $ GHC.srcSpanStartCol (GHC.anchor loc) - 1
        indentedWithFixedLevel col $ pretty c
        eolCommentsArePrinted

prettyHorizontally :: ConstructorSignature -> Printer ()
prettyHorizontally (ByArrows {..}) =
  inter (string " -> ") $ fmap pretty parameters ++ [pretty result]
prettyHorizontally (HIndent.Ast.Declaration.Data.GADT.Constructor.Signature.Record {..}) =
  inter
    (string " -> ")
    [prettyWith fields (vFields' . fmap pretty), pretty result]

prettyVertically :: ConstructorSignature -> Printer ()
prettyVertically (ByArrows {..}) =
  prefixedLined "-> " $ fmap pretty parameters ++ [pretty result]
prettyVertically (HIndent.Ast.Declaration.Data.GADT.Constructor.Signature.Record {..}) =
  prefixedLined
    "-> "
    [prettyWith fields (vFields' . fmap pretty), pretty result]

instance Pretty HIndent.Ast.Context.Context where
  pretty' (HIndent.Ast.Context.Context xs) = hor <-|> ver
    where
      hor = parensConditional $ hCommaSep $ fmap pretty xs
        where
          parensConditional =
            case xs of
              [_] -> id
              _   -> parens
      ver =
        case xs of
          []  -> string "()"
          [x] -> pretty x
          _   -> vTuple $ fmap pretty xs

instance Pretty NewOrData where
  pretty' Newtype = string "newtype"
  pretty' Data    = string "data"

-- Do nothing if there are no pragmas, module headers, imports, or
-- declarations. Otherwise, extra blank lines will be inserted if only
-- comments are present in the source code. See
-- https://github.com/mihaimaruseac/hindent/issues/586#issuecomment-1374992624.
instance (CommentExtraction l, Pretty e) => Pretty (GHC.GenLocated l e) where
  pretty' (GHC.L _ e) = pretty e

instance Pretty (GHC.HsDecl GHC.GhcPs) where
  pretty' (GHC.TyClD _ d)      = pretty d
  pretty' (GHC.InstD _ inst)   = pretty inst
  pretty' (GHC.DerivD _ x)     = pretty x
  pretty' (GHC.ValD _ bind)    = pretty bind
  pretty' (GHC.SigD _ s)       = pretty s
  pretty' (GHC.KindSigD _ x)   = pretty x
  pretty' (GHC.DefD _ x)       = pretty x
  pretty' (GHC.ForD _ x)       = pretty x
  pretty' (GHC.WarningD _ x)   = pretty x
  pretty' (GHC.AnnD _ x)       = pretty x
  pretty' (GHC.RuleD _ x)      = pretty x
  pretty' (GHC.SpliceD _ sp)   = pretty sp
  pretty' GHC.DocD {}          = docNode
  pretty' (GHC.RoleAnnotD _ x) = pretty x

instance Pretty (GHC.TyClDecl GHC.GhcPs) where
  pretty' = prettyTyClDecl

prettyTyClDecl :: GHC.TyClDecl GHC.GhcPs -> Printer ()
prettyTyClDecl (GHC.FamDecl _ x) = pretty x
prettyTyClDecl GHC.SynDecl {..} = do
  string "type "
  case tcdFixity of
    GHC.Prefix ->
      spaced $ pretty tcdLName : fmap pretty (GHC.hsq_explicit tcdTyVars)
    GHC.Infix ->
      case GHC.hsq_explicit tcdTyVars of
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
prettyTyClDecl GHC.DataDecl {..} = do
  printDataNewtype |=> do
    whenJust (GHC.dd_ctxt tcdDataDefn) $ \x -> do
      pretty $ Context x
      string " =>"
      newline
    pretty tcdLName
  spacePrefixed $ pretty <$> GHC.hsq_explicit tcdTyVars
  pretty tcdDataDefn
  where
    printDataNewtype =
      case GHC.dd_cons tcdDataDefn of
        GHC.DataTypeCons {} -> string "data "
        GHC.NewTypeCon {}   -> string "newtype "
#elif MIN_VERSION_ghc_lib_parser(9,4,1)
prettyTyClDecl GHC.DataDecl {..} = do
  printDataNewtype |=> do
    whenJust (GHC.dd_ctxt tcdDataDefn) $ \x -> do
      pretty $ Context x
      string " =>"
      newline
    pretty tcdLName
  spacePrefixed $ pretty <$> GHC.hsq_explicit tcdTyVars
  pretty tcdDataDefn
  where
    printDataNewtype =
      case GHC.dd_ND tcdDataDefn of
        GHC.DataType -> string "data "
        GHC.NewType  -> string "newtype "
#else
prettyTyClDecl GHC.DataDecl {..} = do
  printDataNewtype |=> do
    whenJust (GHC.dd_ctxt tcdDataDefn) $ \_ -> do
      pretty $ Context $ GHC.dd_ctxt tcdDataDefn
      string " =>"
      newline
    pretty tcdLName
  spacePrefixed $ pretty <$> GHC.hsq_explicit tcdTyVars
  pretty tcdDataDefn
  where
    printDataNewtype =
      case GHC.dd_ND tcdDataDefn of
        GHC.DataType -> string "data "
        GHC.NewType  -> string "newtype "
#endif
prettyTyClDecl GHC.ClassDecl {..} = do
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
        forM_ tcdFDs $ \x@(GHC.L _ GHC.FunDep {}) ->
          printCommentsAnd x $ \(GHC.FunDep _ from to) ->
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
            (flip fmap tcdFDs $ \x@(GHC.L _ GHC.FunDep {}) ->
               printCommentsAnd x $ \(GHC.FunDep _ from to) ->
                 spaced $ fmap pretty from ++ [string "->"] ++ fmap pretty to)
      unless (null sigsMethodsFamilies) $ do
        newline
        indentedBlock $ string "where"
    printNameAndTypeVariables =
      case tcdFixity of
        GHC.Prefix ->
          spaced $ pretty tcdLName : fmap pretty (GHC.hsq_explicit tcdTyVars)
        GHC.Infix ->
          case GHC.hsq_explicit tcdTyVars of
            (l:r:xs) -> do
              parens $
                spaced [pretty l, pretty $ fmap InfixOp tcdLName, pretty r]
              spacePrefixed $ fmap pretty xs
            _ -> error "Not enough parameters are given."
    sigsMethodsFamilies =
      SBF.mkSortedLSigBindFamilyList tcdSigs (GHC.bagToList tcdMeths) tcdATs [] []

instance Pretty (GHC.InstDecl GHC.GhcPs) where
  pretty' GHC.ClsInstD {..}     = pretty cid_inst
  pretty' GHC.DataFamInstD {..} = pretty dfid_inst
  pretty' GHC.TyFamInstD {..}   = pretty $ TopLevelTyFamInstDecl tfid_inst

instance Pretty (GHC.HsBind GHC.GhcPs) where
  pretty' = prettyHsBind

prettyHsBind :: GHC.HsBind GHC.GhcPs -> Printer ()
prettyHsBind GHC.FunBind {..}     = pretty fun_matches
prettyHsBind GHC.PatBind {..}     = pretty pat_lhs >> pretty pat_rhs
prettyHsBind GHC.VarBind {}       = notGeneratedByParser
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsBind GHC.AbsBinds {}      = notGeneratedByParser
#endif
prettyHsBind (GHC.PatSynBind _ x) = pretty x
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (GHC.Sig GHC.GhcPs) where
  pretty' (GHC.TypeSig _ funName params) = do
    printFunName
    string " ::"
    horizontal <-|> vertical
    where
      horizontal = do
        space
        pretty $ HsSigTypeInsideDeclSig <$> GHC.hswc_body params
      vertical = do
        headLen <- printerLength printFunName
        indentSpaces <- getIndentSpaces
        if headLen < indentSpaces
          then space |=>
               pretty (HsSigTypeInsideDeclSig <$> GHC.hswc_body params)
          else do
            newline
            indentedBlock $
              indentedWithSpace 3 $
              pretty $ HsSigTypeInsideDeclSig <$> GHC.hswc_body params
      printFunName = hCommaSep $ fmap pretty funName
  pretty' (GHC.PatSynSig _ names sig) =
    spaced
      [string "pattern", hCommaSep $ fmap pretty names, string "::", pretty sig]
  pretty' (GHC.ClassOpSig _ True funNames params) =
    spaced
      [ string "default"
      , hCommaSep $ fmap pretty funNames
      , string "::"
      , printCommentsAnd params pretty
      ]
  pretty' (GHC.ClassOpSig _ False funNames params) = do
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
  pretty' (GHC.FixSig _ x) = pretty x
  pretty' (GHC.InlineSig _ name detail) =
    spaced [string "{-#", pretty detail, pretty name, string "#-}"]
  pretty' (GHC.SpecSig _ name sigs _) =
    spaced
      [ string "{-# SPECIALISE"
      , pretty name
      , string "::"
      , hCommaSep $ fmap pretty sigs
      , string "#-}"
      ]
  pretty' (GHC.SpecInstSig _ sig) =
    spaced [string "{-# SPECIALISE instance", pretty sig, string "#-}"]
  pretty' (GHC.MinimalSig _ xs) =
    string "{-# MINIMAL " |=> do
      pretty xs
      string " #-}"
  pretty' (GHC.SCCFunSig _ name _) =
    spaced [string "{-# SCC", pretty name, string "#-}"]
  pretty' (GHC.CompleteMatchSig _ names _) =
    spaced
      [ string "{-# COMPLETE"
      , printCommentsAnd names (hCommaSep . fmap pretty)
      , string "#-}"
      ]
#else
instance Pretty (GHC.Sig GHC.GhcPs) where
  pretty' (GHC.TypeSig _ funName params) = do
    printFunName
    string " ::"
    horizontal <-|> vertical
    where
      horizontal = do
        space
        pretty $ HsSigTypeInsideDeclSig <$> GHC.hswc_body params
      vertical = do
        headLen <- printerLength printFunName
        indentSpaces <- getIndentSpaces
        if headLen < indentSpaces
          then space |=>
               pretty (HsSigTypeInsideDeclSig <$> GHC.hswc_body params)
          else do
            newline
            indentedBlock $
              indentedWithSpace 3 $
              pretty $ HsSigTypeInsideDeclSig <$> GHC.hswc_body params
      printFunName = hCommaSep $ fmap pretty funName
  pretty' (GHC.PatSynSig _ names sig) =
    spaced
      [string "pattern", hCommaSep $ fmap pretty names, string "::", pretty sig]
  pretty' (GHC.ClassOpSig _ True funNames params) =
    spaced
      [ string "default"
      , hCommaSep $ fmap pretty funNames
      , string "::"
      , printCommentsAnd params pretty
      ]
  pretty' (GHC.ClassOpSig _ False funNames params) = do
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
  pretty' GHC.IdSig {} = notGeneratedByParser
  pretty' (GHC.FixSig _ x) = pretty x
  pretty' (GHC.InlineSig _ name detail) =
    spaced [string "{-#", pretty detail, pretty name, string "#-}"]
  pretty' (GHC.SpecSig _ name sigs _) =
    spaced
      [ string "{-# SPECIALISE"
      , pretty name
      , string "::"
      , hCommaSep $ fmap pretty sigs
      , string "#-}"
      ]
  pretty' (GHC.SpecInstSig _ _ sig) =
    spaced [string "{-# SPECIALISE instance", pretty sig, string "#-}"]
  pretty' (GHC.MinimalSig _ _ xs) =
    string "{-# MINIMAL " |=> do
      pretty xs
      string " #-}"
  pretty' (GHC.SCCFunSig _ _ name _) =
    spaced [string "{-# SCC", pretty name, string "#-}"]
  pretty' (GHC.CompleteMatchSig _ _ names _) =
    spaced
      [ string "{-# COMPLETE"
      , printCommentsAnd names (hCommaSep . fmap pretty)
      , string "#-}"
      ]
#endif
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (GHC.HsDataDefn GHC.GhcPs) where
  pretty' GHC.HsDataDefn {..} =
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
          [x@(GHC.L _ GHC.ConDeclH98 {con_args = GHC.RecCon {}})] -> do
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
          GHC.NewTypeCon x      -> [x]
          GHC.DataTypeCons _ xs -> xs
      isGADT =
        case dd_cons of
          (GHC.DataTypeCons _ (GHC.L _ GHC.ConDeclGADT {}:_)) -> True
          _                                                   -> False
      derivingsAfterNewline =
        unless (null dd_derivs) $ newline >> printDerivings
      printDerivings = lined $ fmap pretty dd_derivs
#else
instance Pretty (GHC.HsDataDefn GHC.GhcPs) where
  pretty' GHC.HsDataDefn {..} =
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
          [x@(GHC.L _ GHC.ConDeclH98 {con_args = GHC.RecCon {}})] -> do
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
          (GHC.L _ GHC.ConDeclGADT {}:_) -> True
          _                              -> False
      derivingsAfterNewline =
        unless (null dd_derivs) $ newline >> printDerivings
      printDerivings = lined $ fmap pretty dd_derivs
#endif
instance Pretty (GHC.ClsInstDecl GHC.GhcPs) where
  pretty' GHC.ClsInstDecl {..} = do
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
        SBF.mkSortedLSigBindFamilyList
          cid_sigs
          (GHC.bagToList cid_binds)
          []
          cid_tyfam_insts
          cid_datafam_insts

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

instance Pretty (GHC.HsExpr GHC.GhcPs) where
  pretty' = prettyHsExpr

prettyHsExpr :: GHC.HsExpr GHC.GhcPs -> Printer ()
prettyHsExpr (GHC.HsVar _ bind) = pretty $ fmap PrefixOp bind
prettyHsExpr (GHC.HsUnboundVar _ x) = pretty x
#if MIN_VERSION_ghc_lib_parser(9,6,1)
prettyHsExpr (GHC.HsOverLabel _ _ l) = string "#" >> string (GHC.unpackFS l)
#else
prettyHsExpr (GHC.HsOverLabel _ l) = string "#" >> string (GHC.unpackFS l)
#endif
prettyHsExpr (GHC.HsIPVar _ var) = string "?" >> pretty var
prettyHsExpr (GHC.HsOverLit _ x) = pretty x
prettyHsExpr (GHC.HsLit _ l) = pretty l
prettyHsExpr (GHC.HsLam _ body) = pretty body
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (GHC.HsLamCase _ GHC.LamCase matches) =
  pretty $ LambdaCase matches Case
prettyHsExpr (GHC.HsLamCase _ GHC.LamCases matches) =
  pretty $ LambdaCase matches Cases
#else
prettyHsExpr (GHC.HsLamCase _ matches) = pretty $ LambdaCase matches Case
#endif
prettyHsExpr (GHC.HsApp _ l r) = horizontal <-|> vertical
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
    flatten :: GHC.LHsExpr GHC.GhcPs -> [GHC.LHsExpr GHC.GhcPs]
    flatten (GHC.L (GHC.SrcSpanAnn (GHC.EpAnn _ _ cs) _) (GHC.HsApp _ l' r')) =
      flatten l' ++ [insertComments cs r']
    flatten x = [x]
    insertComments ::
         GHC.EpAnnComments -> GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
    insertComments cs (GHC.L s@GHC.SrcSpanAnn {GHC.ann = e@GHC.EpAnn {comments = cs'}} r') =
      GHC.L (s {GHC.ann = e {GHC.comments = cs <> cs'}}) r'
    insertComments _ x = x
#if MIN_VERSION_ghc_lib_parser(9,6,1)
prettyHsExpr (GHC.HsAppType _ l _ r) = do
  pretty l
  string " @"
  pretty r
#else
prettyHsExpr (GHC.HsAppType _ l r) = do
  pretty l
  string " @"
  pretty r
#endif
prettyHsExpr (GHC.OpApp _ l o r) = pretty (InfixApp l o r)
prettyHsExpr (GHC.NegApp _ x _) = string "-" >> pretty x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (GHC.HsPar _ _ expr _) = parens $ pretty expr
#else
prettyHsExpr (GHC.HsPar _ expr) = parens $ pretty expr
#endif
prettyHsExpr (GHC.SectionL _ l o) = spaced [pretty l, pretty (InfixExpr o)]
prettyHsExpr (GHC.SectionR _ o r) = (pretty (InfixExpr o) >> space) |=> pretty r
prettyHsExpr (GHC.ExplicitTuple _ full _) = horizontal <-|> vertical
  where
    horizontal = hTuple $ fmap pretty full
    vertical =
      parens $
      prefixedLined "," $
      fmap (\e -> unless (isMissing e) (space |=> pretty e)) full
    isMissing GHC.Missing {} = True
    isMissing _              = False
prettyHsExpr (GHC.ExplicitSum _ position numElem expr) = do
  string "(#"
  forM_ [1 .. numElem] $ \idx -> do
    if idx == position
      then string " " >> pretty expr >> string " "
      else string " "
    when (idx < numElem) $ string "|"
  string "#)"
prettyHsExpr (GHC.HsCase _ cond arms) = do
  string "case " |=> do
    pretty cond
    string " of"
  if null $ GHC.unLoc $ GHC.mg_alts arms
    then string " {}"
    else do
      newline
      indentedBlock $ pretty arms
prettyHsExpr (GHC.HsIf _ cond t f) = do
  string "if " |=> pretty cond
  indentedBlock $ newlinePrefixed [branch "then " t, branch "else " f]
  where
    branch :: String -> GHC.LHsExpr GHC.GhcPs -> Printer ()
    branch str e =
      case e of
        (GHC.L _ (GHC.HsDo _ (GHC.DoExpr m) xs)) -> doStmt (QualifiedDo m Do) xs
        (GHC.L _ (GHC.HsDo _ (GHC.MDoExpr m) xs)) ->
          doStmt (QualifiedDo m Mdo) xs
        _ -> string str |=> pretty e
      where
        doStmt qDo stmts = do
          string str
          pretty qDo
          newline
          indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
prettyHsExpr (GHC.HsMultiIf _ guards) =
  string "if " |=>
  lined (fmap (pretty . fmap (GRHSExpr GRHSExprMultiWayIf)) guards)
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (GHC.HsLet _ _ binds _ exprs) = pretty $ LetIn binds exprs
#else
prettyHsExpr (GHC.HsLet _ binds exprs) = pretty $ LetIn binds exprs
#endif
prettyHsExpr (GHC.HsDo _ GHC.ListComp {} (GHC.L _ [])) =
  error "Not enough arguments are passed to pretty-print a list comprehension."
prettyHsExpr (GHC.HsDo _ GHC.ListComp {} (GHC.L l (lhs:rhs))) =
  pretty $ GHC.L l $ ListComprehension lhs rhs
-- While the name contains 'Monad', 'MonadComp' is for list comprehensions.
prettyHsExpr (GHC.HsDo _ GHC.MonadComp {} (GHC.L _ [])) =
  error "Not enough arguments are passed to pretty-print a list comprehension."
prettyHsExpr (GHC.HsDo _ GHC.MonadComp {} (GHC.L l (lhs:rhs))) =
  pretty $ GHC.L l $ ListComprehension lhs rhs
prettyHsExpr (GHC.HsDo _ (GHC.DoExpr m) (GHC.L l xs)) =
  pretty $ GHC.L l $ DoExpression xs (QualifiedDo m Do)
prettyHsExpr (GHC.HsDo _ (GHC.MDoExpr m) (GHC.L l xs)) =
  pretty $ GHC.L l $ DoExpression xs (QualifiedDo m Mdo)
prettyHsExpr (GHC.HsDo _ GHC.GhciStmtCtxt {} _) =
  error "We're not using GHCi, are we?"
prettyHsExpr (GHC.ExplicitList _ xs) = horizontal <-|> vertical
  where
    horizontal = brackets $ hCommaSep $ fmap pretty xs
    vertical = vList $ fmap pretty xs
prettyHsExpr (GHC.RecordCon _ name fields) = horizontal <-|> vertical
  where
    horizontal = spaced [pretty name, pretty fields]
    vertical = do
      pretty name
      (space >> pretty fields) <-|> (newline >> indentedBlock (pretty fields))
#if MIN_VERSION_ghc_lib_parser(9,8,1)
prettyHsExpr (GHC.RecordUpd _ name fields) = hor <-|> ver
  where
    hor = spaced [pretty name, printHorFields fields]
    ver = do
      pretty name
      newline
      indentedBlock $ printHorFields fields <-|> printVerFields fields
    printHorFields GHC.RegularRecUpdFields {..} =
      hFields $ fmap (`printCommentsAnd` horField) recUpdFields
    printHorFields GHC.OverloadedRecUpdFields {..} =
      hFields $ fmap (`printCommentsAnd` horField) olRecUpdFields
    printVerFields GHC.RegularRecUpdFields {..} =
      vFields $ fmap printField recUpdFields
    printVerFields GHC.OverloadedRecUpdFields {..} =
      vFields $ fmap printField olRecUpdFields
    printField x = printCommentsAnd x $ (<-|>) <$> horField <*> verField
    horField GHC.HsFieldBind {..} = do
      pretty hfbLHS
      string " = "
      pretty hfbRHS
    verField GHC.HsFieldBind {..} = do
      pretty hfbLHS
      string " ="
      newline
      indentedBlock $ pretty hfbRHS
#elif MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (GHC.RecordUpd _ name fields) = hor <-|> ver
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
      => [GHC.GenLocated l (GHC.HsFieldBind a b)]
      -> Printer ()
    printHorFields = hFields . fmap (`printCommentsAnd` horField)
    printVerFields ::
         (Pretty a, Pretty b, CommentExtraction l)
      => [GHC.GenLocated l (GHC.HsFieldBind a b)]
      -> Printer ()
    printVerFields = vFields . fmap printField
    printField x = printCommentsAnd x $ (<-|>) <$> horField <*> verField
    horField GHC.HsFieldBind {..} = do
      pretty hfbLHS
      string " = "
      pretty hfbRHS
    verField GHC.HsFieldBind {..} = do
      pretty hfbLHS
      string " ="
      newline
      indentedBlock $ pretty hfbRHS
#else
prettyHsExpr (GHC.RecordUpd _ name fields) = hor <-|> ver
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
      => [GHC.GenLocated l (GHC.HsRecField' a b)]
      -> Printer ()
    printHorFields = hFields . fmap (`printCommentsAnd` horField)
    printVerFields ::
         (Pretty a, Pretty b, CommentExtraction l)
      => [GHC.GenLocated l (GHC.HsRecField' a b)]
      -> Printer ()
    printVerFields = vFields . fmap printField
    printField x = printCommentsAnd x $ (<-|>) <$> horField <*> verField
    horField GHC.HsRecField {..} = do
      pretty hsRecFieldLbl
      string " = "
      pretty hsRecFieldArg
    verField GHC.HsRecField {..} = do
      pretty hsRecFieldLbl
      string " ="
      newline
      indentedBlock $ pretty hsRecFieldArg
#endif
prettyHsExpr (GHC.HsGetField _ e f) = do
  pretty e
  dot
  pretty f
prettyHsExpr GHC.HsProjection {..} =
  parens $
  forM_ proj_flds $ \x -> do
    string "."
    pretty x
prettyHsExpr (GHC.ExprWithTySig _ e sig) = do
  pretty e
  string " :: "
  pretty $ GHC.hswc_body sig
prettyHsExpr (GHC.ArithSeq _ _ x) = pretty x
#if !MIN_VERSION_ghc_lib_parser(9,6,1)
prettyHsExpr (GHC.HsSpliceE _ x) = pretty x
#endif
prettyHsExpr (GHC.HsProc _ pat x@(GHC.L _ (GHC.HsCmdTop _ (GHC.L _ (GHC.HsCmdDo _ xs))))) = do
  spaced [string "proc", pretty pat, string "-> do"]
  newline
  indentedBlock $
    printCommentsAnd x (const (printCommentsAnd xs (lined . fmap pretty)))
prettyHsExpr (GHC.HsProc _ pat body) = hor <-|> ver
  where
    hor = spaced [string "proc", pretty pat, string "->", pretty body]
    ver = do
      spaced [string "proc", pretty pat, string "->"]
      newline
      indentedBlock (pretty body)
prettyHsExpr (GHC.HsStatic _ x) = spaced [string "static", pretty x]
prettyHsExpr (GHC.HsPragE _ p x) = spaced [pretty p, pretty x]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr GHC.HsRecSel {} = notGeneratedByParser
prettyHsExpr (GHC.HsTypedBracket _ inner) = typedBrackets $ pretty inner
prettyHsExpr (GHC.HsUntypedBracket _ inner) = pretty inner
#else
prettyHsExpr GHC.HsConLikeOut {} = notGeneratedByParser
prettyHsExpr GHC.HsRecFld {} = notGeneratedByParser
prettyHsExpr (GHC.HsDo _ GHC.ArrowExpr {} _) = notGeneratedByParser
prettyHsExpr (GHC.HsDo _ GHC.PatGuard {} _) = notGeneratedByParser
prettyHsExpr (GHC.HsDo _ GHC.ParStmtCtxt {} _) = notGeneratedByParser
prettyHsExpr (GHC.HsDo _ GHC.TransStmtCtxt {} _) = notGeneratedByParser
prettyHsExpr GHC.HsTick {} = forHpc
prettyHsExpr GHC.HsBinTick {} = forHpc
prettyHsExpr (GHC.HsBracket _ inner) = pretty inner
prettyHsExpr GHC.HsRnBracketOut {} = notGeneratedByParser
prettyHsExpr GHC.HsTcBracketOut {} = notGeneratedByParser
#endif
#if MIN_VERSION_ghc_lib_parser(9,6,1)
prettyHsExpr (GHC.HsTypedSplice _ x) = string "$$" >> pretty x
prettyHsExpr (GHC.HsUntypedSplice _ x) = pretty x
#endif
instance Pretty LambdaCase where
  pretty' (LambdaCase matches caseOrCases) = do
    case caseOrCases of
      Case  -> string "\\case"
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
        spaced $ fmap pretty xs
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
      flatten :: GHC.LHsType GHC.GhcPs -> [GHC.LHsType GHC.GhcPs]
      flatten (GHC.L _ (GHC.HsFunTy _ _ l r)) = flatten l ++ flatten r
      flatten x                               = [x]
  pretty' (HsSigTypeInsideVerticalFuncSig GHC.HsSig {..}) =
    case sig_bndrs of
      GHC.HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        printCommentsAnd sig_body $ \case
          GHC.HsQualTy {..} -> do
            (space >> pretty (HorizontalContext hst_ctxt)) <-|>
              (newline >> pretty (VerticalContext hst_ctxt))
            newline
            prefixed "=> " $ pretty hst_body
          x -> pretty $ HsTypeInsideDeclSig x
      _ -> pretty $ fmap HsTypeInsideDeclSig sig_body
  pretty' (HsSigType' for dir GHC.HsSig {..}) = do
    case sig_bndrs of
      GHC.HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        space
      _ -> return ()
    pretty $ HsType' for dir <$> sig_body

instance Pretty (GHC.ConDecl GHC.GhcPs) where
  pretty' = prettyConDecl

prettyConDecl :: GHC.ConDecl GHC.GhcPs -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9,6,1)
prettyConDecl GHC.ConDeclGADT {..} = do
  hCommaSep $ fmap pretty $ NonEmpty.toList con_names
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
        GHC.PrefixConGADT xs ->
          inter (string " -> ") $
          fmap (\(GHC.HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        GHC.RecConGADT xs _ ->
          inter (string " -> ") [recArg xs, pretty con_res_ty]
    verArgs =
      case con_g_args of
        GHC.PrefixConGADT xs ->
          prefixedLined "-> " $
          fmap (\(GHC.HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        GHC.RecConGADT xs _ ->
          prefixedLined "-> " [recArg xs, pretty con_res_ty]
    recArg xs = printCommentsAnd xs $ \xs' -> vFields' $ fmap pretty xs'
    forallNeeded =
      case GHC.unLoc con_bndrs of
        GHC.HsOuterImplicit {} -> False
        GHC.HsOuterExplicit {} -> True
#else
prettyConDecl GHC.ConDeclGADT {..} = do
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
        GHC.PrefixConGADT xs ->
          inter (string " -> ") $
          fmap (\(GHC.HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        GHC.RecConGADT xs _ -> inter (string " -> ") [recArg xs, pretty con_res_ty]

    verArgs =
      case con_g_args of
        GHC.PrefixConGADT xs ->
          prefixedLined "-> " $
          fmap (\(GHC.HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        GHC.RecConGADT xs _ -> prefixedLined "-> " [recArg xs, pretty con_res_ty]
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
        GHC.PrefixConGADT xs ->
          inter (string " -> ") $
          fmap (\(GHC.HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        GHC.RecConGADT xs -> inter (string " -> ") [recArg xs, pretty con_res_ty]

    verArgs =
      case con_g_args of
        GHC.PrefixConGADT xs ->
          prefixedLined "-> " $
          fmap (\(GHC.HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
        GHC.RecConGADT xs -> prefixedLined "-> " [recArg xs, pretty con_res_ty]
#endif
    recArg xs = printCommentsAnd xs $ \xs' -> vFields' $ fmap pretty xs'

    forallNeeded =
      case GHC.unLoc con_bndrs of
        GHC.HsOuterImplicit {} -> False
        GHC.HsOuterExplicit {} -> True
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyConDecl GHC.ConDeclH98 {con_forall = True, ..} =
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
prettyConDecl GHC.ConDeclH98 {con_forall = True, ..} =
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
prettyConDecl GHC.ConDeclH98 {con_forall = False, ..} =
  case con_args of
    (GHC.InfixCon l r) ->
      spaced [pretty l, pretty $ fmap InfixOp con_name, pretty r]
    _ -> do
      pretty con_name
      pretty con_args

instance Pretty
           (GHC.Match
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' = prettyMatchExpr

prettyMatchExpr :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Printer ()
prettyMatchExpr GHC.Match {m_ctxt = GHC.LambdaExpr, ..} = do
  string "\\"
  unless (null m_pats) $
    case GHC.unLoc $ head m_pats of
      GHC.LazyPat {} -> space
      GHC.BangPat {} -> space
      _              -> return ()
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
          spaced $
            [pretty l, pretty $ fmap InfixOp mc_fun, pretty r] ++ fmap pretty xs
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
  unless (null m_pats) $
    case GHC.unLoc $ head m_pats of
      GHC.LazyPat {} -> space
      GHC.BangPat {} -> space
      _              -> return ()
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
  pretty' (GHC.LastStmt _ x _ _) = pretty x
  pretty' (GHC.BindStmt _ pat body) = do
    pretty pat
    string " <-"
    hor <-|> ver
    where
      hor = space >> pretty body
      ver = newline >> indentedBlock (pretty body)
  pretty' GHC.ApplicativeStmt {} = notGeneratedByParser
  pretty' (GHC.BodyStmt _ (GHC.L loc (GHC.OpApp _ l o r)) _ _) =
    pretty (GHC.L loc (InfixApp l o r))
  pretty' (GHC.BodyStmt _ body _ _) = pretty body
  pretty' (GHC.LetStmt _ l) = string "let " |=> pretty l
  pretty' (GHC.ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
  pretty' GHC.TransStmt {..} =
    vCommaSep $ fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
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
    vCommaSep $ fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
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
          Just _  -> braces $ string ".."
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
        fmap pretty rec_flds ++
        maybeToList (fmap (const (string "..")) rec_dotdot)

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
      pretty $ fmap InfixOp op
      space
      pretty r
    else spaced [pretty l, pretty $ fmap InfixOp op, pretty r]
#else
prettyHsType (GHC.HsOpTy _ l op r) = do
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
prettyHsType (GHC.HsParTy _ inside) = parens $ pretty inside
prettyHsType (GHC.HsIParamTy _ x ty) =
  spaced [string "?" >> pretty x, string "::", pretty ty]
prettyHsType GHC.HsStarTy {} = string "*"
prettyHsType (GHC.HsKindSig _ t k) = spaced [pretty t, string "::", pretty k]
prettyHsType (GHC.HsSpliceTy _ sp) = pretty sp
prettyHsType GHC.HsDocTy {} = docNode
prettyHsType (GHC.HsBangTy _ pack x) = pretty pack >> pretty x
prettyHsType (GHC.HsRecTy _ xs) = hvFields $ fmap pretty xs
prettyHsType (GHC.HsExplicitListTy _ _ xs) =
  case xs of
    [] -> string "'[]"
    _  -> hvPromotedList $ fmap pretty xs
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
        indentedWithSpace 2 $
        newlinePrefixed
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
        indentedWithSpace 2 $
        newlinePrefixed
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
  pretty' = pretty . PrefixOp

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
        space >> pretty body
      GHC.OpApp _ (GHC.L _ (GHC.HsDo _ GHC.MDoExpr {} _)) _ _ ->
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

instance Pretty (GHC.SpliceDecl GHC.GhcPs) where
  pretty' (GHC.SpliceDecl _ sp _) = pretty sp
#if !MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (GHC.HsSplice GHC.GhcPs) where
  pretty' (GHC.HsTypedSplice _ _ _ body) = string "$$" >> pretty body
  pretty' (GHC.HsUntypedSplice _ GHC.DollarSplice _ body) =
    string "$" >> pretty body
  pretty' (GHC.HsUntypedSplice _ GHC.BareSplice _ body) = pretty body
  -- The body of a quasi-quote must not be changed by a formatter.
  -- Changing it will modify the actual behavior of the code.
  pretty' (GHC.HsQuasiQuote _ _ l _ r) =
    brackets $ do
      pretty l
      wrapWithBars $
        indentedWithFixedLevel 0 $ sequence_ $ printers [] "" $ GHC.unpackFS r
    where
      printers ps s [] = reverse (string (reverse s) : ps)
      printers ps s ('\n':xs) =
        printers (newline : string (reverse s) : ps) "" xs
      printers ps s (x:xs) = printers ps (x : s) xs
  pretty' GHC.HsSpliced {} = notGeneratedByParser
#endif
instance Pretty (GHC.Pat GHC.GhcPs) where
  pretty' = prettyPat

instance Pretty PatInsidePatDecl where
  pretty' (PatInsidePatDecl (GHC.ConPat {pat_args = (GHC.InfixCon l r), ..})) =
    spaced [pretty l, pretty $ fmap InfixOp pat_con, pretty r]
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
      pretty $ fmap PrefixOp pat_con
      spacePrefixed $ fmap pretty as
    GHC.RecCon rec -> (pretty pat_con >> space) |=> pretty (RecConPat rec)
    GHC.InfixCon a b -> do
      pretty a
      unlessSpecialOp (GHC.unLoc pat_con) space
      pretty $ fmap InfixOp pat_con
      unlessSpecialOp (GHC.unLoc pat_con) space
      pretty b
prettyPat (GHC.ViewPat _ l r) = spaced [pretty l, string "->", pretty r]
prettyPat (GHC.SplicePat _ x) = pretty x
prettyPat (GHC.LitPat _ x) = pretty x
prettyPat (GHC.NPat _ x _ _) = pretty x
prettyPat (GHC.NPlusKPat _ n k _ _ _) = pretty n >> string "+" >> pretty k
prettyPat (GHC.SigPat _ l r) = spaced [pretty l, string "::", pretty r]

instance Pretty RecConPat where
  pretty' (RecConPat GHC.HsRecFields {..}) =
    case fieldPrinters of
      []  -> string "{}"
      [x] -> braces x
      xs  -> hvFields xs
    where
      fieldPrinters =
        fmap (pretty . fmap RecConField) rec_flds ++
        maybeToList (fmap (const (string "..")) rec_dotdot)
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (GHC.HsBracket GHC.GhcPs) where
  pretty' (GHC.ExpBr _ expr) = brackets $ wrapWithBars $ pretty expr
  pretty' (GHC.PatBr _ expr) =
    brackets $ string "p" >> wrapWithBars (pretty expr)
  pretty' (GHC.DecBrL _ decls) =
    brackets $ string "d| " |=> lined (fmap pretty decls) >> string " |"
  pretty' GHC.DecBrG {} = notGeneratedByParser
  pretty' (GHC.TypBr _ expr) =
    brackets $ string "t" >> wrapWithBars (pretty expr)
  pretty' (GHC.VarBr _ True var) = string "'" >> pretty var
  pretty' (GHC.VarBr _ False var) = string "''" >> pretty var
  pretty' (GHC.TExpBr _ x) = typedBrackets $ pretty x
#endif
instance Pretty SBF.SigBindFamily where
  pretty' (SBF.Sig x)         = pretty x
  pretty' (SBF.Bind x)        = pretty x
  pretty' (SBF.TypeFamily x)  = pretty x
  pretty' (SBF.TyFamInst x)   = pretty x
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
  pretty' (GHC.Present _ e) = pretty e
  pretty' GHC.Missing {}    = pure () -- This appears in a tuple section.
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
      horizontal = space >> pretty hsRecFieldArg
      vertical = newline >> indentedBlock (pretty hsRecFieldArg)
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
      horizontal = space >> pretty hfbRHS
      vertical = newline >> indentedBlock (pretty hfbRHS)
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
-- HsConDeclH98Details
instance Pretty
           (GHC.HsConDetails
              Void
              (GHC.HsScaled
                 GHC.GhcPs
                 (GHC.GenLocated GHC.SrcSpanAnnA (GHC.BangType GHC.GhcPs)))
              (GHC.GenLocated
                 GHC.SrcSpanAnnL
                 [GHC.GenLocated GHC.SrcSpanAnnA (GHC.ConDeclField GHC.GhcPs)])) where
  pretty' (GHC.PrefixCon _ xs) = horizontal <-|> vertical
    where
      horizontal = spacePrefixed $ fmap pretty xs
      vertical = indentedBlock $ newlinePrefixed $ fmap pretty xs
  pretty' (GHC.RecCon x) =
    printCommentsAnd x $ \rec -> do
      newline
      indentedBlock $ vFields $ fmap pretty rec
  pretty' GHC.InfixCon {} =
    error
      "Cannot handle here because 'InfixCon' does not have the information of its constructor."

instance Pretty a => Pretty (GHC.HsScaled GHC.GhcPs a) where
  pretty' (GHC.HsScaled _ x) = pretty x

instance Pretty (GHC.ConDeclField GHC.GhcPs) where
  pretty' GHC.ConDeclField {..}
    -- Here, we *ignore* the 'cd_fld_doc' field because doc strings are
    -- also stored as comments, and printing both results in duplicated
    -- comments.
   = do
    hCommaSep $ fmap pretty cd_fld_names
    string " :: "
    pretty cd_fld_type

instance Pretty InfixExpr where
  pretty' (InfixExpr (GHC.L _ (GHC.HsVar _ bind))) = pretty $ fmap InfixOp bind
  pretty' (InfixExpr x)                            = pretty' x

instance Pretty InfixApp where
  pretty' InfixApp {..} = horizontal <-|> vertical
    where
      horizontal = spaced [pretty lhs, pretty (InfixExpr op), pretty rhs]
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
        spaced [pretty l, pretty $ InfixExpr o, pretty $ QualifiedDo m Do]
        newline
        indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
      prettyOps [l, o, GHC.L _ (GHC.HsDo _ (GHC.MDoExpr m) xs)] = do
        spaced [pretty l, pretty $ InfixExpr o, pretty $ QualifiedDo m Mdo]
        newline
        indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
      prettyOps [l, o, r@(GHC.L _ GHC.HsLam {})] = do
        spaced [pretty l, pretty $ InfixExpr o, pretty r]
      prettyOps [l, o, r@(GHC.L _ GHC.HsLamCase {})] = do
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

instance Pretty a => Pretty (GHC.BooleanFormula a) where
  pretty' (GHC.Var x)    = pretty x
  pretty' (GHC.And xs)   = hvCommaSep $ fmap pretty xs
  pretty' (GHC.Or xs)    = hvBarSep $ fmap pretty xs
  pretty' (GHC.Parens x) = parens $ pretty x

instance Pretty (GHC.FieldLabelStrings GHC.GhcPs) where
  pretty' (GHC.FieldLabelStrings xs) = hDotSep $ fmap pretty xs

instance Pretty (GHC.AmbiguousFieldOcc GHC.GhcPs) where
  pretty' (GHC.Unambiguous _ name) = pretty name
  pretty' (GHC.Ambiguous _ name)   = pretty name

instance Pretty (GHC.HsDerivingClause GHC.GhcPs) where
  pretty' GHC.HsDerivingClause { deriv_clause_strategy = Just strategy@(GHC.L _ GHC.ViaStrategy {})
                               , ..
                               } =
    spaced [string "deriving", pretty deriv_clause_tys, pretty strategy]
  pretty' GHC.HsDerivingClause {..} = do
    string "deriving "
    whenJust deriv_clause_strategy $ \x -> do
      pretty x
      space
    pretty deriv_clause_tys

instance Pretty (GHC.DerivClauseTys GHC.GhcPs) where
  pretty' (GHC.DctSingle _ ty) = parens $ pretty ty
  pretty' (GHC.DctMulti _ ts)  = hvTuple $ fmap pretty ts

instance Pretty GHC.OverlapMode where
  pretty' GHC.NoOverlap {}    = notUsedInParsedStage
  pretty' GHC.Overlappable {} = string "{-# OVERLAPPABLE #-}"
  pretty' GHC.Overlapping {}  = string "{-# OVERLAPPING #-}"
  pretty' GHC.Overlaps {}     = string "{-# OVERLAPS #-}"
  pretty' GHC.Incoherent {}   = string "{-# INCOHERENT #-}"

instance Pretty GHC.StringLiteral where
  pretty' = output

-- | This instance is for type family declarations inside a class declaration.
instance Pretty (GHC.FamilyDecl GHC.GhcPs) where
  pretty' GHC.FamilyDecl {..} = do
    string $
      case fdInfo of
        GHC.DataFamily          -> "data"
        GHC.OpenTypeFamily      -> "type"
        GHC.ClosedTypeFamily {} -> "type"
    case fdTopLevel of
      GHC.TopLevel    -> string " family "
      GHC.NotTopLevel -> space
    pretty fdLName
    spacePrefixed $ pretty <$> GHC.hsq_explicit fdTyVars
    case GHC.unLoc fdResultSig of
      GHC.NoSig {} -> pure ()
      GHC.TyVarSig {} -> do
        string " = "
        pretty fdResultSig
      _ -> do
        space
        pretty fdResultSig
    whenJust fdInjectivityAnn $ \x -> do
      string " | "
      pretty x
    case fdInfo of
      GHC.ClosedTypeFamily (Just xs) -> do
        string " where"
        newline
        indentedBlock $ lined $ fmap pretty xs
      _ -> pure ()

instance Pretty (GHC.FamilyResultSig GHC.GhcPs) where
  pretty' GHC.NoSig {}       = pure ()
  pretty' (GHC.KindSig _ x)  = string ":: " >> pretty x
  pretty' (GHC.TyVarSig _ x) = pretty x

instance Pretty (GHC.HsTyVarBndr a GHC.GhcPs) where
  pretty' (GHC.UserTyVar _ _ x) = pretty x
  pretty' (GHC.KindedTyVar _ _ name ty) =
    parens $ spaced [pretty name, string "::", pretty ty]

instance Pretty (GHC.InjectivityAnn GHC.GhcPs) where
  pretty' (GHC.InjectivityAnn _ from to) =
    spaced $ pretty from : string "->" : fmap pretty to

instance Pretty (GHC.ArithSeqInfo GHC.GhcPs) where
  pretty' (GHC.From from) = brackets $ spaced [pretty from, string ".."]
  pretty' (GHC.FromThen from next) =
    brackets $ spaced [pretty from >> comma >> pretty next, string ".."]
  pretty' (GHC.FromTo from to) =
    brackets $ spaced [pretty from, string "..", pretty to]
  pretty' (GHC.FromThenTo from next to) =
    brackets $
    spaced [pretty from >> comma >> pretty next, string "..", pretty to]

instance Pretty (GHC.HsForAllTelescope GHC.GhcPs) where
  pretty' GHC.HsForAllVis {..} = do
    string "forall "
    spaced $ fmap pretty hsf_vis_bndrs
    dot
  pretty' GHC.HsForAllInvis {..} = do
    string "forall "
    spaced $ fmap pretty hsf_invis_bndrs
    dot

instance Pretty InfixOp where
  pretty' (InfixOp (GHC.Unqual name)) = backticksIfNotSymbol name $ pretty name
  pretty' (InfixOp (GHC.Qual modName name)) =
    backticksIfNotSymbol name $ do
      pretty modName
      string "."
      pretty name
  pretty' (InfixOp GHC.Orig {}) = notUsedInParsedStage
  pretty' (InfixOp (GHC.Exact name)) = backticksIfNotSymbol occ $ pretty occ
    where
      occ = GHC.occName name

instance Pretty PrefixOp where
  pretty' (PrefixOp (GHC.Unqual name)) = parensIfSymbol name $ pretty name
  pretty' (PrefixOp (GHC.Qual modName name)) =
    parensIfSymbol name $ do
      pretty modName
      string "."
      pretty name
  pretty' (PrefixOp GHC.Orig {}) = notUsedInParsedStage
  pretty' (PrefixOp (GHC.Exact name)) = parensIfSymbol occ $ output name
    where
      occ = GHC.occName name

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
          (GHC.L _ [])  -> parens
          (GHC.L _ [_]) -> id
          _             -> parens

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
          Nothing            -> id
          Just (GHC.L _ [])  -> parens
          Just (GHC.L _ [_]) -> id
          Just _             -> parens

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
    pretty feqn_rhs
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
    pretty feqn_rhs
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
  pretty' (GHC.HsValArg x)    = pretty x
  pretty' (GHC.HsTypeArg _ x) = string "@" >> pretty x
  pretty' GHC.HsArgPar {}     = notUsedInParsedStage
#else
instance Pretty
           (GHC.HsArg
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))) where
  pretty' (GHC.HsValArg x)    = pretty x
  pretty' (GHC.HsTypeArg _ x) = string "@" >> pretty x
  pretty' GHC.HsArgPar {}     = notUsedInParsedStage
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (GHC.HsQuote GHC.GhcPs) where
  pretty' (GHC.ExpBr _ x) = brackets $ wrapWithBars $ pretty x
  pretty' (GHC.PatBr _ x) = brackets $ string "p" >> wrapWithBars (pretty x)
  pretty' (GHC.DecBrL _ decls) =
    brackets $ string "d| " |=> lined (fmap pretty decls) >> string " |"
  pretty' GHC.DecBrG {} = notUsedInParsedStage
  pretty' (GHC.TypBr _ x) = brackets $ string "t" >> wrapWithBars (pretty x)
  pretty' (GHC.VarBr _ True x) = string "'" >> pretty x
  pretty' (GHC.VarBr _ False x) = string "''" >> pretty x
#endif
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (GHC.WarnDecls GHC.GhcPs) where
  pretty' (GHC.Warnings _ x) = lined $ fmap pretty x
#else
instance Pretty (GHC.WarnDecls GHC.GhcPs) where
  pretty' (GHC.Warnings _ _ x) = lined $ fmap pretty x
#endif
#if MIN_VERSION_ghc_lib_parser(9,8,1)
instance Pretty (GHC.WarnDecl GHC.GhcPs) where
  pretty' (GHC.Warning _ names deprecatedOrWarning) =
    case deprecatedOrWarning of
      GHC.DeprecatedTxt _ reasons -> prettyWithTitleReasons "DEPRECATED" reasons
      GHC.WarningTxt _ _ reasons  -> prettyWithTitleReasons "WARNING" reasons
    where
      prettyWithTitleReasons title reasons =
        lined
          [ string $ "{-# " ++ title
          , spaced
              [hCommaSep $ fmap pretty names, hCommaSep $ fmap pretty reasons]
          , string " #-}"
          ]
#else
instance Pretty (GHC.WarnDecl GHC.GhcPs) where
  pretty' (GHC.Warning _ names deprecatedOrWarning) =
    case deprecatedOrWarning of
      GHC.DeprecatedTxt _ reasons -> prettyWithTitleReasons "DEPRECATED" reasons
      GHC.WarningTxt _ reasons    -> prettyWithTitleReasons "WARNING" reasons
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
instance Pretty (GHC.WithHsDocIdentifiers GHC.StringLiteral GHC.GhcPs) where
  pretty' GHC.WithHsDocIdentifiers {..} = pretty hsDocString
#endif

#if MIN_VERSION_ghc_lib_parser(9,6,1)
-- | 'Pretty' for 'LIEWrappedName (IdP GhcPs)'
instance Pretty (GHC.IEWrappedName GHC.GhcPs) where
  pretty' (GHC.IEName _ name)    = pretty name
  pretty' (GHC.IEPattern _ name) = spaced [string "pattern", pretty name]
  pretty' (GHC.IEType _ name)    = string "type " >> pretty name
#else
-- | 'Pretty' for 'LIEWrappedName (IdP GhcPs)'
instance Pretty (GHC.IEWrappedName GHC.RdrName) where
  pretty' (GHC.IEName name)      = pretty name
  pretty' (GHC.IEPattern _ name) = spaced [string "pattern", pretty name]
  pretty' (GHC.IEType _ name)    = string "type " >> pretty name
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
instance Pretty (GHC.RuleDecls GHC.GhcPs) where
  pretty' GHC.HsRules {..} =
    lined $ string "{-# RULES" : fmap pretty rds_rules ++ [string " #-}"]
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (GHC.RuleDecl GHC.GhcPs) where
  pretty' GHC.HsRule {..} =
    spaced
      [ printCommentsAnd rd_name (doubleQuotes . string . GHC.unpackFS)
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
instance Pretty (GHC.RuleDecl GHC.GhcPs) where
  pretty' GHC.HsRule {..} =
    spaced
      [ printCommentsAnd rd_name (doubleQuotes . string . GHC.unpackFS . snd)
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
instance Pretty GHC.OccName where
  pretty' = output

instance Pretty (GHC.DerivDecl GHC.GhcPs) where
  pretty' GHC.DerivDecl { deriv_strategy = (Just deriv_strategy@(GHC.L _ GHC.ViaStrategy {}))
                        , ..
                        } =
    spaced
      [ string "deriving"
      , pretty deriv_strategy
      , string "instance"
      , pretty deriv_type
      ]
  pretty' GHC.DerivDecl {..} = do
    string "deriving "
    whenJust deriv_strategy $ \x -> do
      pretty x
      space
    string "instance "
    pretty deriv_type

-- | 'Pretty' for 'LHsSigWcType GhcPs'.
instance Pretty
           (GHC.HsWildCardBndrs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsSigType GHC.GhcPs))) where
  pretty' GHC.HsWC {..} = pretty hswc_body

-- | 'Pretty' for 'LHsWcType'
instance Pretty
           (GHC.HsWildCardBndrs
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsType GHC.GhcPs))) where
  pretty' GHC.HsWC {..} = pretty hswc_body

instance Pretty (GHC.StandaloneKindSig GHC.GhcPs) where
  pretty' (GHC.StandaloneKindSig _ name kind) =
    spaced [string "type", pretty name, string "::", pretty kind]

instance Pretty (GHC.DefaultDecl GHC.GhcPs) where
  pretty' (GHC.DefaultDecl _ xs) =
    spaced [string "default", hTuple $ fmap pretty xs]

instance Pretty (GHC.ForeignDecl GHC.GhcPs) where
  pretty' GHC.ForeignImport {..} =
    spaced
      [ string "foreign import"
      , pretty fd_fi
      , pretty fd_name
      , string "::"
      , pretty fd_sig_ty
      ]
  pretty' GHC.ForeignExport {..} =
    spaced
      [ string "foreign export"
      , pretty fd_fe
      , pretty fd_name
      , string "::"
      , pretty fd_sig_ty
      ]
#if MIN_VERSION_ghc_lib_parser(9,8,0)
instance Pretty (GHC.ForeignImport GHC.GhcPs) where
  pretty' (GHC.CImport (GHC.L _ (GHC.SourceText s)) conv safety _ _) =
    spaced [pretty conv, pretty safety, output s]
  pretty' (GHC.CImport _ conv safety _ _) = spaced [pretty conv, pretty safety]
#elif MIN_VERSION_ghc_lib_parser(9,6,0)
instance Pretty (GHC.ForeignImport GHC.GhcPs) where
  pretty' (GHC.CImport (GHC.L _ (GHC.SourceText s)) conv safety _ _) =
    spaced [pretty conv, pretty safety, string s]
  pretty' (GHC.CImport _ conv safety _ _) = spaced [pretty conv, pretty safety]
#else
instance Pretty GHC.ForeignImport where
  pretty' (GHC.CImport conv safety _ _ (GHC.L _ (GHC.SourceText s))) =
    spaced [pretty conv, pretty safety, string s]
  pretty' (GHC.CImport conv safety _ _ _) = spaced [pretty conv, pretty safety]
#endif

#if MIN_VERSION_ghc_lib_parser(9,8,0)
instance Pretty (GHC.ForeignExport GHC.GhcPs) where
  pretty' (GHC.CExport (GHC.L _ (GHC.SourceText s)) conv) =
    spaced [pretty conv, output s]
  pretty' (GHC.CExport _ conv) = pretty conv
#elif MIN_VERSION_ghc_lib_parser(9,6,0)
instance Pretty (GHC.ForeignExport GHC.GhcPs) where
  pretty' (GHC.CExport (GHC.L _ (GHC.SourceText s)) conv) =
    spaced [pretty conv, string s]
  pretty' (GHC.CExport _ conv) = pretty conv
#else
instance Pretty GHC.ForeignExport where
  pretty' (GHC.CExport conv (GHC.L _ (GHC.SourceText s))) =
    spaced [pretty conv, string s]
  pretty' (GHC.CExport conv _) = pretty conv
#endif
instance Pretty GHC.CExportSpec where
  pretty' (GHC.CExportStatic _ _ x) = pretty x

instance Pretty GHC.Safety where
  pretty' GHC.PlaySafe          = string "safe"
  pretty' GHC.PlayInterruptible = string "interruptible"
  pretty' GHC.PlayRisky         = string "unsafe"
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty (GHC.AnnDecl GHC.GhcPs) where
  pretty' (GHC.HsAnnotation _ (GHC.ValueAnnProvenance name) expr) =
    spaced [string "{-# ANN", pretty name, pretty expr, string "#-}"]
  pretty' (GHC.HsAnnotation _ (GHC.TypeAnnProvenance name) expr) =
    spaced [string "{-# ANN type", pretty name, pretty expr, string "#-}"]
  pretty' (GHC.HsAnnotation _ GHC.ModuleAnnProvenance expr) =
    spaced [string "{-# ANN module", pretty expr, string "#-}"]
#else
instance Pretty (GHC.AnnDecl GHC.GhcPs) where
  pretty' (GHC.HsAnnotation _ _ (GHC.ValueAnnProvenance name) expr) =
    spaced [string "{-# ANN", pretty name, pretty expr, string "#-}"]
  pretty' (GHC.HsAnnotation _ _ (GHC.TypeAnnProvenance name) expr) =
    spaced [string "{-# ANN type", pretty name, pretty expr, string "#-}"]
  pretty' (GHC.HsAnnotation _ _ GHC.ModuleAnnProvenance expr) =
    spaced [string "{-# ANN module", pretty expr, string "#-}"]
#endif
instance Pretty (GHC.RoleAnnotDecl GHC.GhcPs) where
  pretty' (GHC.RoleAnnotDecl _ name roles) =
    spaced $
    [string "type role", pretty name] ++
    fmap (maybe (string "_") pretty . GHC.unLoc) roles

instance Pretty GHC.Role where
  pretty' GHC.Nominal          = string "nominal"
  pretty' GHC.Representational = string "representational"
  pretty' GHC.Phantom          = string "phantom"

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

instance Pretty (GHC.PatSynBind GHC.GhcPs GHC.GhcPs) where
  pretty' GHC.PSB {..} = do
    string "pattern "
    case psb_args of
      GHC.InfixCon l r ->
        spaced [pretty l, pretty $ fmap InfixOp psb_id, pretty r]
      GHC.PrefixCon _ [] -> pretty psb_id
      _ -> spaced [pretty psb_id, pretty psb_args]
    spacePrefixed [pretty psb_dir, pretty $ fmap PatInsidePatDecl psb_def]
    case psb_dir of
      GHC.ExplicitBidirectional matches -> do
        newline
        indentedBlock $ string "where " |=> pretty matches
      _ -> pure ()

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

instance Pretty (GHC.FixitySig GHC.GhcPs) where
  pretty' (GHC.FixitySig _ names fixity) =
    spaced [pretty fixity, hCommaSep $ fmap (pretty . fmap InfixOp) names]

instance Pretty GHC.Fixity where
  pretty' (GHC.Fixity _ level dir) = spaced [pretty dir, string $ show level]

instance Pretty GHC.FixityDirection where
  pretty' GHC.InfixL = string "infixl"
  pretty' GHC.InfixR = string "infixr"
  pretty' GHC.InfixN = string "infix"

instance Pretty GHC.InlinePragma where
  pretty' GHC.InlinePragma {..} = do
    pretty inl_inline
    case inl_act of
      GHC.ActiveBefore _ x -> space >> brackets (string $ "~" ++ show x)
      GHC.ActiveAfter _ x  -> space >> brackets (string $ show x)
      _                    -> pure ()

instance Pretty GHC.InlineSpec where
  pretty' = prettyInlineSpec

prettyInlineSpec :: GHC.InlineSpec -> Printer ()
prettyInlineSpec GHC.Inline {} = string "INLINE"
prettyInlineSpec GHC.Inlinable {} = string "INLINABLE"
prettyInlineSpec GHC.NoInline {} = string "NOINLINE"
prettyInlineSpec GHC.NoUserInlinePrag =
  error
    "This branch is executed if the inline pragma is not written, but executing this branch means that the pragma is already about to be output, which indicates something goes wrong."
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyInlineSpec GHC.Opaque {} = string "OPAQUE"
#endif
instance Pretty (GHC.HsPatSynDir GHC.GhcPs) where
  pretty' GHC.Unidirectional           = string "<-"
  pretty' GHC.ImplicitBidirectional    = string "="
  pretty' GHC.ExplicitBidirectional {} = string "<-"

instance Pretty (GHC.HsOverLit GHC.GhcPs) where
  pretty' GHC.OverLit {..} = pretty ol_val

instance Pretty GHC.OverLitVal where
  pretty' (GHC.HsIntegral x)   = pretty x
  pretty' (GHC.HsFractional x) = pretty x
  pretty' (GHC.HsIsString _ x) = string $ GHC.unpackFS x
#if MIN_VERSION_ghc_lib_parser(9,8,1)
instance Pretty GHC.IntegralLit where
  pretty' GHC.IL {il_text = GHC.SourceText s} = output s
  pretty' GHC.IL {..}                         = string $ show il_value
#else
instance Pretty GHC.IntegralLit where
  pretty' GHC.IL {il_text = GHC.SourceText s} = string s
  pretty' GHC.IL {..}                         = string $ show il_value
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
      GHC.HsString {}     -> prettyString
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
              indentedWithSpace (-1) $
                lined $ fmap (string . dropWhile (/= '\\')) ss
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
  pretty' (GHC.HsNumTy _ x)  = string $ show x
  pretty' (GHC.HsStrTy _ x)  = string $ ushow x
  pretty' (GHC.HsCharTy _ x) = string $ show x
#else
instance Pretty GHC.HsTyLit where
  pretty' (GHC.HsNumTy _ x)  = string $ show x
  pretty' (GHC.HsStrTy _ x)  = string $ ushow x
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
  spaced [string "?" >> pretty l, string "=", pretty r]
#else
prettyIPBind (GHC.IPBind _ (Right _) _) = notUsedInParsedStage
prettyIPBind (GHC.IPBind _ (Left l) r) =
  spaced [string "?" >> pretty l, string "=", pretty r]
#endif
instance Pretty (GHC.DerivStrategy GHC.GhcPs) where
  pretty' GHC.StockStrategy {}    = string "stock"
  pretty' GHC.AnyclassStrategy {} = string "anyclass"
  pretty' GHC.NewtypeStrategy {}  = string "newtype"
  pretty' (GHC.ViaStrategy x)     = string "via " >> pretty x

instance Pretty GHC.XViaStrategyPs where
  pretty' (GHC.XViaStrategyPs _ ty) = pretty ty

instance Pretty (GHC.RecordPatSynField GHC.GhcPs) where
  pretty' GHC.RecordPatSynField {..} = pretty recordPatSynField

instance Pretty (GHC.HsCmdTop GHC.GhcPs) where
  pretty' (GHC.HsCmdTop _ cmd) = pretty cmd

instance Pretty (GHC.HsCmd GHC.GhcPs) where
  pretty' = prettyHsCmd

prettyHsCmd :: GHC.HsCmd GHC.GhcPs -> Printer ()
prettyHsCmd (GHC.HsCmdArrApp _ f arg GHC.HsHigherOrderApp True) =
  spaced [pretty f, string "-<<", pretty arg]
prettyHsCmd (GHC.HsCmdArrApp _ f arg GHC.HsHigherOrderApp False) =
  spaced [pretty arg, string ">>-", pretty f]
prettyHsCmd (GHC.HsCmdArrApp _ f arg GHC.HsFirstOrderApp True) =
  spaced [pretty f, string "-<", pretty arg]
prettyHsCmd (GHC.HsCmdArrApp _ f arg GHC.HsFirstOrderApp False) =
  spaced [pretty arg, string ">-", pretty f]
prettyHsCmd (GHC.HsCmdArrForm _ f _ _ args) =
  bananaBrackets $ spaced $ pretty f : fmap pretty args
prettyHsCmd (GHC.HsCmdApp _ f arg) = spaced [pretty f, pretty arg]
prettyHsCmd (GHC.HsCmdLam _ x) = pretty x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsCmd (GHC.HsCmdPar _ _ x _) = parens $ pretty x
#else
prettyHsCmd (GHC.HsCmdPar _ x) = parens $ pretty x
#endif
prettyHsCmd (GHC.HsCmdCase _ cond arms) = do
  spaced [string "case", pretty cond, string "of"]
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
  pretty cond
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
  pretty' DoExpression {..} = do
    pretty qualifiedDo
    newline
    indentedBlock $ lined $ fmap pretty doStmts

instance Pretty DoOrMdo where
  pretty' Do  = string "do"
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

instance Pretty (GHC.RuleBndr GHC.GhcPs) where
  pretty' (GHC.RuleBndr _ name) = pretty name
  pretty' (GHC.RuleBndrSig _ name sig) =
    parens $ spaced [pretty name, string "::", pretty sig]

instance Pretty GHC.CCallConv where
  pretty' GHC.CCallConv          = string "ccall"
  pretty' GHC.CApiConv           = string "capi"
  pretty' GHC.StdCallConv        = string "stdcall"
  pretty' GHC.PrimCallConv       = string "prim"
  pretty' GHC.JavaScriptCallConv = string "javascript"

instance Pretty GHC.HsSrcBang where
  pretty' (GHC.HsSrcBang _ unpack strictness) = do
    pretty unpack
    unless (unpack == GHC.NoSrcUnpack) space
    pretty strictness

instance Pretty GHC.SrcUnpackedness where
  pretty' GHC.SrcUnpack   = string "{-# UNPACK #-}"
  pretty' GHC.SrcNoUnpack = string "{-# NOUNPACK #-}"
  pretty' GHC.NoSrcUnpack = pure ()

instance Pretty GHC.SrcStrictness where
  pretty' GHC.SrcLazy     = string "~"
  pretty' GHC.SrcStrict   = string "!"
  pretty' GHC.NoSrcStrict = pure ()

instance Pretty (GHC.HsOuterSigTyVarBndrs GHC.GhcPs) where
  pretty' GHC.HsOuterImplicit {} = pure ()
  pretty' GHC.HsOuterExplicit {..} = do
    string "forall"
    spacePrefixed $ fmap pretty hso_bndrs
    dot
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty GHC.FieldLabelString where
  pretty' = output

instance Pretty (GHC.HsUntypedSplice GHC.GhcPs) where
  pretty' (GHC.HsUntypedSpliceExpr _ x) = string "$" >> pretty x
  -- The body of a quasi-quote must not be changed by a formatter.
  -- Changing it will modify the actual behavior of the code.
  --
  -- TODO: Remove duplicated code
  pretty' (GHC.HsQuasiQuote _ l r) =
    brackets $ do
      pretty l
      printCommentsAnd
        r
        (wrapWithBars .
         indentedWithFixedLevel 0 . sequence_ . printers [] "" . GHC.unpackFS)
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
