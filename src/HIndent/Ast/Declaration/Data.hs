{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data
  ( DataDeclaration
  , mkDataDeclaration
  ) where

import           HIndent.Applicative
import           HIndent.Ast.Declaration.Data.NewOrData
import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs     as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types

data DataDeclaration = DataDeclaration
  { newOrData :: NewOrData
  , decl      :: GHC.TyClDecl GHC.GhcPs
  }

instance CommentExtraction DataDeclaration where
  nodeComments (DataDeclaration {}) = NodeComments [] [] []
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
  pretty' DataDeclaration {decl = GHC.DataDecl {..}, ..} = do
    (pretty newOrData >> space) |=> do
      whenJust (GHC.dd_ctxt tcdDataDefn) $ \_ -> do
        pretty $ Context $ GHC.dd_ctxt tcdDataDefn
        string " =>"
        newline
      pretty tcdLName
    spacePrefixed $ pretty <$> GHC.hsq_explicit tcdTyVars
    pretty tcdDataDefn
  pretty' _ = error "Not a data declaration."
#endif
mkDataDeclaration :: GHC.TyClDecl GHC.GhcPs -> DataDeclaration
mkDataDeclaration decl@GHC.DataDecl {tcdDataDefn = GHC.HsDataDefn {..}} =
  DataDeclaration {..}
  where
    newOrData = mkNewOrData dd_ND
mkDataDeclaration _ = error "Not a data declaration."
