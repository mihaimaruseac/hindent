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
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
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
import HIndent.Ast.Expression.Bracket
import HIndent.Ast.Expression.Splice
import HIndent.Ast.Module.Name (mkModuleName)
import HIndent.Ast.Name.Infix
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
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
prettyHsExpr (GHC.HsVar _ bind) = pretty $ fmap mkPrefixName bind
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
prettyHsExpr (GHC.HsUnboundVar _ x) = pretty $ mkPrefixName x
#else
prettyHsExpr (GHC.HsUnboundVar _ x) = pretty x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 6, 1) && !MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyHsExpr (GHC.HsOverLabel _ _ l) = string "#" >> string (GHC.unpackFS l)
#else
prettyHsExpr (GHC.HsOverLabel _ l) = string "#" >> string (GHC.unpackFS l)
#endif
prettyHsExpr (GHC.HsIPVar _ var) = string "?" >> pretty var
prettyHsExpr (GHC.HsOverLit _ x) = pretty x
prettyHsExpr (GHC.HsLit _ l) = pretty l
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyHsExpr (GHC.HsLam _ GHC.LamSingle body) = pretty body
prettyHsExpr (GHC.HsLam _ GHC.LamCase body) = pretty $ LambdaCase body Case
prettyHsExpr (GHC.HsLam _ GHC.LamCases body) = pretty $ LambdaCase body Cases
#else
prettyHsExpr (GHC.HsLam _ body) = pretty body
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
-- No `HsLamCase` since 9.10.1.
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
prettyHsExpr (GHC.HsLamCase _ GHC.LamCase matches) =
  pretty $ LambdaCase matches Case
prettyHsExpr (GHC.HsLamCase _ GHC.LamCases matches) =
  pretty $ LambdaCase matches Cases
#else
prettyHsExpr (GHC.HsLamCase _ matches) = pretty $ LambdaCase matches Case
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyHsExpr (GHC.HsApp _ l r) = horizontal <-|> vertical
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
    flatten :: GHC.LHsExpr GHC.GhcPs -> [GHC.LHsExpr GHC.GhcPs]
    flatten (GHC.L (GHC.EpAnn _ _ cs) (GHC.HsApp _ l' r')) =
      flatten l' ++ [insertComments cs r']
    flatten x = [x]
    insertComments ::
         GHC.EpAnnComments -> GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
    insertComments cs (GHC.L s@GHC.EpAnn {comments = cs'} r') =
      GHC.L (s {GHC.comments = cs <> cs'}) r'
    insertComments _ x = x
#else
prettyHsExpr (GHC.HsApp _ l r) = horizontal <-|> vertical
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
    flatten :: GHC.LHsExpr GHC.GhcPs -> [GHC.LHsExpr GHC.GhcPs]
    flatten (GHC.L (GHC.SrcSpanAnn (GHC.EpAnn _ _ cs) _) (GHC.HsApp _ l' r')) =
      flatten l' ++ [insertComments cs r']
    flatten x = [x]
    insertComments ::
         GHC.EpAnnComments -> GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
    insertComments cs (GHC.L s@GHC.SrcSpanAnn {GHC.ann = e@GHC.EpAnn {comments = cs'}} r') =
      GHC.L (s {GHC.ann = e {GHC.comments = cs <> cs'}}) r'
    insertComments _ x = x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 6, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
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
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyHsExpr (GHC.HsPar _ _ expr _) = parens $ pretty expr
#else
prettyHsExpr (GHC.HsPar _ expr) = parens $ pretty expr
#endif
prettyHsExpr (GHC.SectionL _ l o) = spaced [pretty l, pretty (InfixExpr o)]
prettyHsExpr (GHC.SectionR _ o r) = (pretty (InfixExpr o) >> space) |=> pretty r
prettyHsExpr (GHC.ExplicitTuple _ full boxity) = horizontal <-|> vertical
  where
    horizontal = parH $ fmap pretty full
    vertical =
      parV
        $ prefixedLined ","
        $ fmap (\e -> unless (isMissing e) (space |=> pretty e)) full
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
        (GHC.L _ (GHC.HsDo _ (GHC.DoExpr m) xs)) ->
          doStmt (QualifiedDo (fmap mkModuleName m) Do) xs
        (GHC.L _ (GHC.HsDo _ (GHC.MDoExpr m) xs)) ->
          doStmt (QualifiedDo (fmap mkModuleName m) Mdo) xs
        _ -> string str |=> pretty e
      where
        doStmt qDo stmts = do
          string str
          pretty qDo
          newline
          indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)
prettyHsExpr (GHC.HsMultiIf _ guards) =
  string "if "
    |=> lined (fmap (pretty . fmap (GRHSExpr GRHSExprMultiWayIf)) guards)
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyHsExpr (GHC.HsLet _ _ binds _ exprs) = pretty $ LetIn binds exprs
#else
prettyHsExpr (GHC.HsLet _ binds exprs) = pretty $ LetIn binds exprs
#endif
prettyHsExpr (GHC.HsDo _ GHC.ListComp {} (GHC.L _ [])) =
  error "Not enough arguments are passed to pretty-print a list comprehension."
prettyHsExpr (GHC.HsDo _ GHC.ListComp {} (GHC.L _ [_])) =
  error "Not enough arguments are passed to pretty-print a list comprehension."
prettyHsExpr (GHC.HsDo _ GHC.ListComp {} (GHC.L l (lhs:rhs:rhss))) =
  pretty $ GHC.L l $ ListComprehension lhs (rhs :| rhss)
-- While the name contains 'Monad', 'MonadComp' is for list comprehensions.
prettyHsExpr (GHC.HsDo _ GHC.MonadComp {} (GHC.L _ [])) =
  error "Not enough arguments are passed to pretty-print a list comprehension."
prettyHsExpr (GHC.HsDo _ GHC.MonadComp {} (GHC.L _ [_])) =
  error "Not enough arguments are passed to pretty-print a list comprehension."
prettyHsExpr (GHC.HsDo _ GHC.MonadComp {} (GHC.L l (lhs:rhs:rhss))) =
  pretty $ GHC.L l $ ListComprehension lhs (rhs :| rhss)
prettyHsExpr (GHC.HsDo _ (GHC.DoExpr m) (GHC.L l xs)) =
  pretty $ GHC.L l $ DoExpression xs (QualifiedDo (fmap mkModuleName m) Do)
prettyHsExpr (GHC.HsDo _ (GHC.MDoExpr m) (GHC.L l xs)) =
  pretty $ GHC.L l $ DoExpression xs (QualifiedDo (fmap mkModuleName m) Mdo)
prettyHsExpr (GHC.HsDo _ GHC.GhciStmtCtxt {} _) =
  error "We're not using GHCi, are we?"
prettyHsExpr (GHC.ExplicitList _ xs) = horizontal <-|> vertical
  where
    horizontal = brackets $ hCommaSep $ fmap pretty xs
    vertical = vList $ fmap pretty xs
prettyHsExpr (GHC.RecordCon _ name fields) = horizontal <-|> vertical
  where
    horizontal = spaced [pretty $ fmap mkPrefixName name, pretty fields]
    vertical = do
      pretty $ fmap mkPrefixName name
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
      indentedBlock
        $ either printHorFields printHorFields fields
            <-|> either printVerFields printVerFields fields
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
      indentedBlock
        $ either printHorFields printHorFields fields
            <-|> either printVerFields printVerFields fields
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
  parens
    $ forM_ proj_flds
    $ \x -> do
        string "."
        pretty x
prettyHsExpr (GHC.ExprWithTySig _ e sig) = do
  pretty e
  string " :: "
  pretty $ GHC.hswc_body sig
prettyHsExpr (GHC.ArithSeq _ _ x) = pretty x
#if !MIN_VERSION_ghc_lib_parser(9,6,1)
prettyHsExpr (GHC.HsSpliceE _ x) = pretty $ mkSplice x
#endif
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
prettyHsExpr (GHC.HsStatic _ x) = spaced [string "static", pretty x]
prettyHsExpr (GHC.HsPragE _ p x) = spaced [pretty p, pretty x]
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyHsExpr GHC.HsRecSel {} = notGeneratedByParser
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
prettyHsExpr (GHC.HsTypedBracket _ inner) = typedBrackets $ pretty inner
prettyHsExpr (GHC.HsUntypedBracket _ inner) = pretty $ mkBracket inner
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
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyMatchExpr GHC.Match {m_ctxt = GHC.LamAlt GHC.LamSingle, ..} = do
  string "\\"
  case GHC.unLoc m_pats of
    p:_ ->
      case GHC.unLoc p of
        GHC.LazyPat {} -> space
        GHC.BangPat {} -> space
        _ -> return ()
    _ -> return ()
  prettyWith (fromGenLocated m_pats) (spaced . fmap pretty)
  pretty $ GRHSsExpr GRHSExprLambda m_grhss
prettyMatchExpr GHC.Match {m_ctxt = GHC.LamAlt GHC.LamCase, ..} = do
  prettyWith (fromGenLocated m_pats) (spaced . fmap pretty)
  pretty $ GRHSsExpr GRHSExprCase m_grhss
prettyMatchExpr GHC.Match {m_ctxt = GHC.LamAlt GHC.LamCases, ..} = do
  prettyWith (fromGenLocated m_pats) (spaced . fmap pretty)
  pretty $ GRHSsExpr GRHSExprCase m_grhss
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyMatchExpr GHC.Match {m_ctxt = GHC.LamAlt GHC.LamSingle, ..} = do
  string "\\"
  case m_pats of
    p:_ ->
      case GHC.unLoc p of
        GHC.LazyPat {} -> space
        GHC.BangPat {} -> space
        _ -> return ()
    _ -> return ()
  spaced $ fmap pretty m_pats
  pretty $ GRHSsExpr GRHSExprLambda m_grhss
prettyMatchExpr GHC.Match {m_ctxt = GHC.LamAlt GHC.LamCase, ..} = do
  spaced $ fmap pretty m_pats
  pretty $ GRHSsExpr GRHSExprCase m_grhss
prettyMatchExpr GHC.Match {m_ctxt = GHC.LamAlt GHC.LamCases, ..} = do
  spaced $ fmap pretty m_pats
  pretty $ GRHSsExpr GRHSExprCase m_grhss
#else
prettyMatchExpr GHC.Match {m_ctxt = GHC.LambdaExpr, ..} = do
  string "\\"
  case m_pats of
    p:_ ->
      case GHC.unLoc p of
        GHC.LazyPat {} -> space
        GHC.BangPat {} -> space
        _ -> return ()
    _ -> return ()
  spaced $ fmap pretty m_pats
  pretty $ GRHSsExpr GRHSExprLambda m_grhss
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyMatchExpr GHC.Match {m_ctxt = GHC.LamCaseAlt {}, ..} = do
  spaced $ fmap pretty m_pats
  pretty $ GRHSsExpr GRHSExprCase m_grhss
#endif
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyMatchExpr GHC.Match {m_ctxt = GHC.CaseAlt, ..} = do
  prettyWith (fromGenLocated m_pats) (mapM_ pretty)
  pretty $ GRHSsExpr GRHSExprCase m_grhss
#else
prettyMatchExpr GHC.Match {m_ctxt = GHC.CaseAlt, ..} = do
  mapM_ pretty m_pats
  pretty $ GRHSsExpr GRHSExprCase m_grhss
#endif
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyMatchExpr GHC.Match {..} =
  case GHC.mc_fixity m_ctxt of
    GHC.Prefix -> do
      pretty m_ctxt
      prettyWith (fromGenLocated m_pats) (spacePrefixed . fmap pretty)
      pretty m_grhss
    GHC.Infix -> do
      case (GHC.unLoc m_pats, m_ctxt) of
        (l:r:xs, GHC.FunRhs {..}) -> do
          spaced
            $ [pretty l, pretty $ fmap mkInfixName mc_fun, pretty r]
                ++ fmap pretty xs
          pretty m_grhss
        _ -> error "Not enough parameters are passed."
#else
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
            $ [pretty l, pretty $ fmap mkInfixName mc_fun, pretty r]
                ++ fmap pretty xs
          pretty m_grhss
        _ -> error "Not enough parameters are passed."
#endif
instance Pretty
           (GHC.Match
              GHC.GhcPs
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs))) where
  pretty' = prettyMatchProc

prettyMatchProc :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyMatchProc GHC.Match {m_ctxt = GHC.LamAlt GHC.LamSingle, ..} = do
  string "\\"
  case GHC.unLoc m_pats of
    p:_ ->
      case GHC.unLoc p of
        GHC.LazyPat {} -> space
        GHC.BangPat {} -> space
        _ -> return ()
    _ -> return ()
  spaced [prettyWith (fromGenLocated m_pats) (mapM_ pretty), pretty m_grhss]
prettyMatchProc GHC.Match {m_ctxt = GHC.LamAlt GHC.LamCase, ..} = do
  spaced [prettyWith (fromGenLocated m_pats) (mapM_ pretty), pretty m_grhss]
prettyMatchProc GHC.Match {m_ctxt = GHC.LamAlt GHC.LamCases, ..} = do
  spaced [prettyWith (fromGenLocated m_pats) (mapM_ pretty), pretty m_grhss]
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyMatchProc GHC.Match {m_ctxt = GHC.LamAlt GHC.LamSingle, ..} = do
  string "\\"
  case m_pats of
    p:_ ->
      case GHC.unLoc p of
        GHC.LazyPat {} -> space
        GHC.BangPat {} -> space
        _ -> return ()
    _ -> return ()
  spaced $ fmap pretty m_pats ++ [pretty m_grhss]
prettyMatchProc GHC.Match {m_ctxt = GHC.LamAlt GHC.LamCase, ..} = do
  spaced [mapM_ pretty m_pats, pretty m_grhss]
prettyMatchProc GHC.Match {m_ctxt = GHC.LamAlt GHC.LamCases, ..} = do
  spaced [mapM_ pretty m_pats, pretty m_grhss]
#else
prettyMatchProc GHC.Match {m_ctxt = GHC.LambdaExpr, ..} = do
  string "\\"
  case m_pats of
    p:_ ->
      case GHC.unLoc p of
        GHC.LazyPat {} -> space
        GHC.BangPat {} -> space
        _ -> return ()
    _ -> return ()
  spaced $ fmap pretty m_pats ++ [pretty m_grhss]
#endif
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyMatchProc GHC.Match {m_ctxt = GHC.CaseAlt, ..} =
  spaced [prettyWith (fromGenLocated m_pats) (mapM_ pretty), pretty m_grhss]
#else
prettyMatchProc GHC.Match {m_ctxt = GHC.CaseAlt, ..} =
  spaced [mapM_ pretty m_pats, pretty m_grhss]
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyMatchProc GHC.Match {m_ctxt = GHC.LamCaseAlt {}, ..} = do
  spaced [mapM_ pretty m_pats, pretty m_grhss]
#endif
prettyMatchProc _ = notGeneratedByParser

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
prettyStmtLRExpr (GHC.LastStmt _ x _ _) = pretty x
prettyStmtLRExpr (GHC.BindStmt _ pat body) = do
  pretty pat
  string " <-"
  hor <-|> ver
  where
    hor = space >> pretty body
    ver = newline >> indentedBlock (pretty body)
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyStmtLRExpr GHC.ApplicativeStmt {} = notGeneratedByParser
#endif
prettyStmtLRExpr (GHC.BodyStmt _ (GHC.L loc (GHC.OpApp _ l o r)) _ _) =
  pretty (GHC.L loc (InfixApp l o r))
prettyStmtLRExpr (GHC.BodyStmt _ body _ _) = pretty body
prettyStmtLRExpr (GHC.LetStmt _ l) = string "let " |=> pretty l
prettyStmtLRExpr (GHC.ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
prettyStmtLRExpr GHC.TransStmt {..} =
  vCommaSep $ fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
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
    hor = spaced [pretty pat, string "<-", pretty body]
    ver = do
      pretty pat
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
  vCommaSep $ fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
prettyStmtLRCmd GHC.RecStmt {..} =
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
prettyHsType (GHC.HsTyVar _ GHC.NotPromoted x) = pretty $ fmap mkPrefixName x
prettyHsType (GHC.HsTyVar _ GHC.IsPromoted x) =
  string "'" >> pretty (fmap mkPrefixName x)
prettyHsType x@(GHC.HsAppTy _ l r) = hor <-|> ver
  where
    hor = spaced $ fmap pretty [l, r]
    ver = pretty $ HsTypeWithVerticalAppTy x
#if MIN_VERSION_ghc_lib_parser(9, 8, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
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
      pretty $ fmap mkInfixName op
      space
      pretty r
    else spaced [pretty l, pretty $ fmap mkInfixName op, pretty r]
#else
prettyHsType (GHC.HsOpTy _ l op r) = do
  lineBreak <- gets (configLineBreaks . psConfig)
  if showOutputable op `elem` lineBreak
    then do
      pretty l
      newline
      pretty $ fmap mkInfixName op
      space
      pretty r
    else spaced [pretty l, pretty $ fmap mkInfixName op, pretty r]
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
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyHsType (GHC.HsExplicitTupleTy _ _ xs) = hPromotedTuple $ fmap pretty xs
#else
prettyHsType (GHC.HsExplicitTupleTy _ xs) = hPromotedTuple $ fmap pretty xs
#endif
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
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance Pretty
           (GHC.HsMatchContext (GHC.GenLocated GHC.SrcSpanAnnN GHC.RdrName)) where
  pretty' = prettyHsMatchContext

prettyHsMatchContext ::
     GHC.HsMatchContext (GHC.GenLocated GHC.SrcSpanAnnN GHC.RdrName)
  -> Printer ()
prettyHsMatchContext GHC.FunRhs {..} =
  pretty mc_strictness >> pretty (fmap mkPrefixName mc_fun)
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
#else
instance Pretty (GHC.HsMatchContext GHC.GhcPs) where
  pretty' = prettyHsMatchContext

prettyHsMatchContext :: GHC.HsMatchContext GHC.GhcPs -> Printer ()
prettyHsMatchContext GHC.FunRhs {..} =
  pretty mc_strictness >> pretty (fmap mkPrefixName mc_fun)
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

instance Pretty ParStmtBlockInsideVerticalList where
  pretty' (ParStmtBlockInsideVerticalList (GHC.ParStmtBlock _ xs _ _)) =
    vCommaSep $ fmap pretty xs

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
        printCommentsAnd
          body
          (const (doExpr (QualifiedDo (fmap mkModuleName m) Do) stmts))
      GHC.HsDo _ (GHC.MDoExpr m) stmts ->
        printCommentsAnd
          body
          (const (doExpr (QualifiedDo (fmap mkModuleName m) Mdo) stmts))
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
        GHC.HsDo _ (GHC.DoExpr m) stmts ->
          doExpr (QualifiedDo (fmap mkModuleName m) Do) stmts
        GHC.HsDo _ (GHC.MDoExpr m) stmts ->
          doExpr (QualifiedDo (fmap mkModuleName m) Mdo) stmts
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

instance Pretty (GHC.Pat GHC.GhcPs) where
  pretty' = prettyPat

instance Pretty PatInsidePatDecl where
  pretty' (PatInsidePatDecl (GHC.ConPat {pat_args = (GHC.InfixCon l r), ..})) =
    spaced [pretty l, pretty $ fmap mkInfixName pat_con, pretty r]
  pretty' (PatInsidePatDecl x) = pretty x

prettyPat :: GHC.Pat GHC.GhcPs -> Printer ()
prettyPat GHC.WildPat {} = string "_"
prettyPat (GHC.VarPat _ x) = pretty $ fmap mkPrefixName x
prettyPat (GHC.LazyPat _ x) = string "~" >> pretty x
#if MIN_VERSION_ghc_lib_parser(9, 6, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyPat (GHC.AsPat _ a _ b) =
  pretty (fmap mkPrefixName a) >> string "@" >> pretty b
#else
prettyPat (GHC.AsPat _ a b) =
  pretty (fmap mkPrefixName a) >> string "@" >> pretty b
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
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
      pretty $ fmap mkPrefixName pat_con
      spacePrefixed $ fmap pretty as
    GHC.RecCon rec ->
      (pretty (fmap mkPrefixName pat_con) >> space) |=> pretty (RecConPat rec)
    GHC.InfixCon a b -> do
      pretty a
      unlessSpecialOp (GHC.unLoc pat_con) space
      pretty $ fmap mkInfixName pat_con
      unlessSpecialOp (GHC.unLoc pat_con) space
      pretty b
prettyPat (GHC.ViewPat _ l r) = spaced [pretty l, string "->", pretty r]
prettyPat (GHC.SplicePat _ x) = pretty $ mkSplice x
prettyPat (GHC.LitPat _ x) = pretty x
prettyPat (GHC.NPat _ x _ _) = pretty x
prettyPat (GHC.NPlusKPat _ n k _ _ _) =
  pretty (fmap mkPrefixName n) >> string "+" >> pretty k
prettyPat (GHC.SigPat _ l r) = spaced [pretty l, string "::", pretty r]
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyPat (GHC.OrPat _ pats) =
  inter (string "; ") (fmap pretty $ NE.toList pats)
#endif
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
instance Pretty (GHC.HsTupArg GHC.GhcPs) where
  pretty' (GHC.Present _ e) = pretty e
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
    (pretty hfbLHS >> string " = ") |=> pretty hfbRHS

-- | For record updates.
instance Pretty
           (GHC.HsFieldBind
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.FieldOcc GHC.GhcPs))
              (GHC.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs))) where
  pretty' GHC.HsFieldBind {..} = do
    pretty hfbLHS
    unless hfbPun $ do
      string " ="
      horizontal <-|> vertical
    where
      horizontal = space >> pretty hfbRHS
      vertical = newline >> indentedBlock (pretty hfbRHS)
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
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
  pretty' GHC.FieldOcc {..} = pretty $ fmap mkPrefixName foLabel
#else
instance Pretty (GHC.FieldOcc GHC.GhcPs) where
  pretty' GHC.FieldOcc {..} = pretty $ fmap mkPrefixName rdrNameFieldOcc
#endif
instance Pretty a => Pretty (GHC.HsScaled GHC.GhcPs a) where
  pretty' (GHC.HsScaled _ x) = pretty x

instance Pretty InfixExpr where
  pretty' (InfixExpr (GHC.L _ (GHC.HsVar _ bind))) =
    pretty $ fmap mkInfixName bind
  pretty' (InfixExpr x) = pretty' x

instance Pretty InfixApp where
  pretty' = prettyInfixApp

prettyInfixApp :: InfixApp -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyInfixApp InfixApp {..} = horizontal <-|> vertical
  where
    horizontal = spaced [pretty lhs, pretty (InfixExpr op), pretty rhs]
    vertical =
      case findFixity op of
        GHC.Fixity _ GHC.InfixL -> leftAssoc
        GHC.Fixity _ GHC.InfixR -> rightAssoc
        GHC.Fixity _ GHC.InfixN -> noAssoc
    leftAssoc = prettyOps allOperantsAndOperatorsLeftAssoc
    rightAssoc = prettyOps allOperantsAndOperatorsRightAssoc
    noAssoc
      | GHC.L _ (GHC.OpApp _ _ o _) <- lhs
      , isSameAssoc o = leftAssoc
      | otherwise = rightAssoc
    prettyOps [l, o, GHC.L _ (GHC.HsDo _ (GHC.DoExpr m) xs)] = do
      spaced
        [ pretty l
        , pretty $ InfixExpr o
        , pretty $ QualifiedDo (fmap mkModuleName m) Do
        ]
      newline
      indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
    prettyOps [l, o, GHC.L _ (GHC.HsDo _ (GHC.MDoExpr m) xs)] = do
      spaced
        [ pretty l
        , pretty $ InfixExpr o
        , pretty $ QualifiedDo (fmap mkModuleName m) Mdo
        ]
      newline
      indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
    prettyOps [l, o, r@(GHC.L _ GHC.HsLam {})] = do
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
          error "The number of the sum of operants and operators should be odd."
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
    isSameAssoc (findFixity -> GHC.Fixity lv d) = lv == level && d == dir
    GHC.Fixity level dir = findFixity op
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyInfixApp InfixApp {..} = horizontal <-|> vertical
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
      spaced
        [ pretty l
        , pretty $ InfixExpr o
        , pretty $ QualifiedDo (fmap mkModuleName m) Do
        ]
      newline
      indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
    prettyOps [l, o, GHC.L _ (GHC.HsDo _ (GHC.MDoExpr m) xs)] = do
      spaced
        [ pretty l
        , pretty $ InfixExpr o
        , pretty $ QualifiedDo (fmap mkModuleName m) Mdo
        ]
      newline
      indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
    prettyOps [l, o, r@(GHC.L _ GHC.HsLam {})] = do
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
          error "The number of the sum of operants and operators should be odd."
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
#else
prettyInfixApp InfixApp {..} = horizontal <-|> vertical
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
      spaced
        [ pretty l
        , pretty $ InfixExpr o
        , pretty $ QualifiedDo (fmap mkModuleName m) Do
        ]
      newline
      indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
    prettyOps [l, o, GHC.L _ (GHC.HsDo _ (GHC.MDoExpr m) xs)] = do
      spaced
        [ pretty l
        , pretty $ InfixExpr o
        , pretty $ QualifiedDo (fmap mkModuleName m) Mdo
        ]
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
          error "The number of the sum of operants and operators should be odd."
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
#endif
instance Pretty (GHC.FieldLabelStrings GHC.GhcPs) where
  pretty' (GHC.FieldLabelStrings xs) = hDotSep $ fmap pretty xs
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
instance Pretty (GHC.AmbiguousFieldOcc GHC.GhcPs) where
  pretty' (GHC.Unambiguous _ name) = pretty $ fmap mkPrefixName name
  pretty' (GHC.Ambiguous _ name) = pretty $ fmap mkPrefixName name
#endif
instance Pretty (GHC.DerivClauseTys GHC.GhcPs) where
  pretty' (GHC.DctSingle _ ty) = parens $ pretty ty
  pretty' (GHC.DctMulti _ ts) = hvTuple $ fmap pretty ts
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance Pretty GHC.StringLiteral where
  pretty' GHC.StringLiteral {sl_st = GHC.SourceText s} = string $ GHC.unpackFS s
  pretty' GHC.StringLiteral {..} = string $ GHC.unpackFS sl_fs
#else
instance Pretty GHC.StringLiteral where
  pretty' = output
#endif
instance Pretty (GHC.ArithSeqInfo GHC.GhcPs) where
  pretty' (GHC.From from) = brackets $ spaced [pretty from, string ".."]
  pretty' (GHC.FromThen from next) =
    brackets $ spaced [pretty from >> comma >> pretty next, string ".."]
  pretty' (GHC.FromTo from to) =
    brackets $ spaced [pretty from, string "..", pretty to]
  pretty' (GHC.FromThenTo from next to) =
    brackets
      $ spaced [pretty from >> comma >> pretty next, string "..", pretty to]

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
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance Pretty (GHC.IE GHC.GhcPs) where
  pretty' (GHC.IEVar _ name _) = pretty name
  pretty' (GHC.IEThingAbs _ name _) = pretty name
  pretty' (GHC.IEThingAll _ name _) = do
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
    pretty feqn_rhs

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
  pretty' (GHC.HsValArg _ x) = pretty x
  pretty' (GHC.HsTypeArg _ x) = string "@" >> pretty x
  pretty' GHC.HsArgPar {} = notUsedInParsedStage
#elif MIN_VERSION_ghc_lib_parser(9, 8, 1)
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
  pretty' (GHC.IEName _ name) = pretty $ fmap mkPrefixName name
  pretty' (GHC.IEPattern _ name) =
    spaced [string "pattern", pretty $ fmap mkPrefixName name]
  pretty' (GHC.IEType _ name) =
    string "type " >> pretty (fmap mkPrefixName name)
#else
-- | 'Pretty' for 'LIEWrappedName (IdP GhcPs)'
instance Pretty (GHC.IEWrappedName GHC.RdrName) where
  pretty' (GHC.IEName name) = pretty $ fmap mkPrefixName name
  pretty' (GHC.IEPattern _ name) =
    spaced [string "pattern", pretty $ fmap mkPrefixName name]
  pretty' (GHC.IEType _ name) =
    string "type " >> pretty (fmap mkPrefixName name)
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
  pretty' (GHC.PrefixCon _ xs) = spaced $ fmap (pretty . fmap mkPrefixName) xs
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
  spaced [string "?" >> pretty l, string "=", pretty r]
#else
prettyIPBind (GHC.IPBind _ (Right _) _) = notUsedInParsedStage
prettyIPBind (GHC.IPBind _ (Left l) r) =
  spaced [string "?" >> pretty l, string "=", pretty r]
#endif
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
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyHsCmd (GHC.HsCmdArrForm _ f _ args) =
  bananaBrackets $ spaced $ pretty f : fmap pretty args
#else
prettyHsCmd (GHC.HsCmdArrForm _ f _ _ args) =
  bananaBrackets $ spaced $ pretty f : fmap pretty args
#endif
prettyHsCmd (GHC.HsCmdApp _ f arg) = spaced [pretty f, pretty arg]
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyHsCmd (GHC.HsCmdLam _ GHC.LamSingle x) = pretty x
prettyHsCmd (GHC.HsCmdLam _ GHC.LamCase arms) = do
  string "\\case"
  newline
  indentedBlock $ pretty arms
prettyHsCmd (GHC.HsCmdLam _ GHC.LamCases arms) = do
  string "\\cases"
  newline
  indentedBlock $ pretty arms
#else
prettyHsCmd (GHC.HsCmdLam _ x) = pretty x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
prettyHsCmd (GHC.HsCmdPar _ _ x _) = parens $ pretty x
#else
prettyHsCmd (GHC.HsCmdPar _ x) = parens $ pretty x
#endif
prettyHsCmd (GHC.HsCmdCase _ cond arms) = do
  spaced [string "case", pretty cond, string "of"]
  newline
  indentedBlock $ pretty arms
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
-- No `HsCmdLamCase` since 9.10.1
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
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

instance Pretty ListComprehension where
  pretty' ListComprehension {..} = horizontal <-|> vertical
    where
      horizontal =
        brackets
          $ spaced
              [ pretty listCompLhs
              , string "|"
              , hCommaSep $ NE.toList $ NE.map pretty listCompRhs
              ]
      vertical = do
        string "[ "
        pretty $ fmap StmtLRInsideVerticalList listCompLhs
        newline
        forM_ (stmtsAndPrefixes listCompRhs) $ \(p, x) -> do
          string p |=> pretty (fmap StmtLRInsideVerticalList x)
          newline
        string "]"
      stmtsAndPrefixes (s :| ss) = ("| ", s) : fmap (", ", ) ss

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

instance Pretty GHC.HsSrcBang where
  pretty' = prettyHsSrcBang

prettyHsSrcBang :: GHC.HsSrcBang -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
prettyHsSrcBang (GHC.HsSrcBang _ bang) = pretty bang
#else
prettyHsSrcBang (GHC.HsSrcBang _ unpack strictness) = do
  pretty unpack
  unless (unpack == GHC.NoSrcUnpack) space
  pretty strictness
#endif

#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
instance Pretty GHC.HsBang where
  pretty' (GHC.HsBang unpack strictness) = do
    pretty unpack
    unless (unpack == GHC.NoSrcUnpack) space
    pretty strictness
#endif
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
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
-- | Marks an AST node as it is used only for Haskell Program Coverage.
forHpc :: HasCallStack => a
forHpc = error "This AST type is for the use of Haskell Program Coverage."
#endif

#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
getAnc (GHC.EpaSpan (GHC.RealSrcSpan x _)) = x
getAnc _ = undefined
#else
getAnc = GHC.anchor
#endif
