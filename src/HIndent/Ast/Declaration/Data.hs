{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data
  ( DataDeclaration
  , mkDataDeclaration
  ) where

import           Control.Monad
import           Data.Maybe
import qualified GHC.Types.SrcLoc                    as GHC
import           HIndent.Applicative
import           HIndent.Ast.Declaration.Data.GADT
import           HIndent.Ast.Declaration.Data.Header
import           HIndent.Ast.NodeComments
import           HIndent.Ast.Type
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs  as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

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

instance Pretty DataDeclaration where
  pretty' GADT {..} = do
    pretty header
    whenJust kind $ \x -> string " :: " >> pretty x
    string " where"
    indentedBlock $ newlinePrefixed $ fmap pretty constructors
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
    constructors =
      fromMaybe (error "Some constructors are not GADT ones.") $
      mapM (traverse mkGADTConstructor . fromGenLocated) dd_cons
mkDataDeclaration _ = Nothing
