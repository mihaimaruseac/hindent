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
import qualified HIndent.GhcLibParserWrapper.GHC.Hs  as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data DataDeclaration
  = GADT
      { header :: Header
      , decl   :: GHC.TyClDecl GHC.GhcPs
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
  pretty' GADT {decl = GHC.DataDecl {tcdDataDefn = GHC.HsDataDefn {..}}, ..} = do
    pretty header
    whenJust dd_kindSig $ \x -> string " :: " >> pretty x
    string " where"
    indentedBlock $ newlinePrefixed $ fmap pretty dd_cons
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
mkDataDeclaration decl@GHC.DataDecl {tcdDataDefn = GHC.HsDataDefn {..}} =
  if isGADT
    then GADT <$> header <*> pure decl
    else Record <$> header <*> pure decl
  where
    header = mkHeader decl
    isGADT =
      case dd_cons of
        (GHC.L _ GHC.ConDeclGADT {}:_) -> True
        _                              -> False
mkDataDeclaration _ = Nothing
