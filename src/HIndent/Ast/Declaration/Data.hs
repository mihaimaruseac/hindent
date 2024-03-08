{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data
  ( DataDeclaration
  , mkDataDeclaration
  ) where

import           Control.Monad
import qualified GHC.Types.SrcLoc                    as GHC
import           HIndent.Applicative
import           HIndent.Ast.Declaration.Data.Header
import           HIndent.Ast.NodeComments
import           HIndent.Ast.Type
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs  as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types
import           HIndent.Printer

newtype GADTConstructor =
  GADTConstructor (GHC.ConDecl GHC.GhcPs)

instance CommentExtraction GADTConstructor where
  nodeComments (GADTConstructor x) = nodeComments x

instance Pretty GADTConstructor where
  pretty' (GADTConstructor x) = pretty x

data DataDeclaration
  = GADT
      { header       :: Header
      , kind         :: Maybe (WithComments Type)
      , decl         :: GHC.TyClDecl GHC.GhcPs
      , constructors :: [WithComments GADTConstructor]
      }
  | Record
      { header :: Header
      , decl   :: GHC.TyClDecl GHC.GhcPs
      }

instance CommentExtraction DataDeclaration where
  nodeComments GADT {}   = NodeComments [] [] []
  nodeComments Record {} = NodeComments [] [] []
#if MIN_VERSION_ghc_lib_parser(9,6,1)
instance Pretty Data where
  pretty' Data {decl = DataDecl {..}} = do
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
          NewTypeCon {}   -> string "newtype "
#elif MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty Data where
  pretty' Data {decl = DataDecl {..}} = do
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
instance Pretty DataDeclaration where
  pretty' GADT {..} = do
    pretty header
    whenJust kind $ \x -> string " :: " >> pretty x
    string " where"
    indentedBlock $
      newlinePrefixed $ fmap (`prettyWith` prettyGADT) constructors
  pretty' Record {decl = GHC.DataDecl {tcdDataDefn = GHC.HsDataDefn {..}}, ..} = do
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
  pretty' _ = error "Not a data declaration."
#endif
mkDataDeclaration :: GHC.TyClDecl GHC.GhcPs -> Maybe DataDeclaration
mkDataDeclaration decl@GHC.DataDecl {tcdDataDefn = GHC.HsDataDefn {..}}
  | Just header <- mkHeader decl =
    Just $
    if isGADT
      then GADT {..}
      else Record {..}
  where
    kind = fmap mkType . fromGenLocated <$> dd_kindSig
    isGADT =
      case dd_cons of
        (GHC.L _ GHC.ConDeclGADT {}:_) -> True
        _                              -> False
    constructors = fmap (fmap GADTConstructor . fromGenLocated) dd_cons
mkDataDeclaration _ = Nothing

prettyGADT :: GADTConstructor -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9,6,1)
prettyGADT ConDeclGADT {..} = do
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
    recArg xs = printCommentsAnd xs $ \xs' -> vFields' $ fmap pretty xs'
    forallNeeded =
      case unLoc con_bndrs of
        HsOuterImplicit {} -> False
        HsOuterExplicit {} -> True
#else
prettyGADT (GADTConstructor GHC.ConDeclGADT {..}) = do
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
prettyGADT _ = error "Not a GADT constructor."
