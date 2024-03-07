{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data
  ( DataDeclaration
  , mkDataDeclaration
  ) where

import           Control.Monad
import qualified GHC.Types.SrcLoc                       as GHC
import           HIndent.Applicative
import           HIndent.Ast.Declaration.Data.NewOrData
import           HIndent.Ast.NodeComments
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs     as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types

data DataDeclaration = DataDeclaration
  { newOrData :: NewOrData
  , name      :: WithComments (GHC.IdP GHC.GhcPs)
  , context   :: Context
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
  pretty' DataDeclaration { decl = GHC.DataDecl { tcdDataDefn = GHC.HsDataDefn {..}
                                                , ..
                                                }
                          , ..
                          } = do
    (pretty newOrData >> space) |=> do
      case context of
        Context (Just _) -> pretty context >> string " =>" >> newline
        Context Nothing  -> pure ()
      pretty name
    spacePrefixed $ pretty <$> GHC.hsq_explicit tcdTyVars
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
  pretty' _ = error "Not a data declaration."
#endif
mkDataDeclaration :: GHC.TyClDecl GHC.GhcPs -> DataDeclaration
mkDataDeclaration decl@GHC.DataDecl {tcdDataDefn = GHC.HsDataDefn {..}, ..} =
  DataDeclaration {..}
  where
    newOrData = mkNewOrData dd_ND
    context = Context dd_ctxt
    name = fromGenLocated tcdLName
mkDataDeclaration _ = error "Not a data declaration."
