{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data
  ( DataDeclaration
  , mkDataDeclaration
  ) where

import           Control.Monad
import           Data.Maybe
import qualified GHC.Types.SrcLoc                              as GHC
import           HIndent.Applicative
import           HIndent.Ast.Declaration.Data.GADT.Constructor
import           HIndent.Ast.Declaration.Data.Header
import           HIndent.Ast.NodeComments
import           HIndent.Ast.Type
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs            as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data DataDeclaration
  = GADT
      { header       :: Header
      , kind         :: Maybe (WithComments Type)
      , constructors :: [WithComments GADTConstructor]
      }
  | Record
      { header    :: Header
      , dd_cons   :: [GHC.LConDecl GHC.GhcPs]
      , dd_derivs :: GHC.HsDeriving GHC.GhcPs
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
  pretty' Record {..} = do
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

mkDataDeclaration :: GHC.TyClDecl GHC.GhcPs -> Maybe DataDeclaration
mkDataDeclaration decl@GHC.DataDecl {tcdDataDefn = defn@GHC.HsDataDefn {..}}
  | Just header <- mkHeader decl =
    Just $
    if isGADT defn
      then GADT
             { constructors =
                 fromMaybe (error "Some constructors are not GADT ones.") $
                 mapM (traverse mkGADTConstructor . fromGenLocated) dd_cons
             , ..
             }
      else Record {..}
  where
    kind = fmap mkType . fromGenLocated <$> dd_kindSig
mkDataDeclaration _ = Nothing

isGADT :: GHC.HsDataDefn GHC.GhcPs -> Bool
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
isGADT GHC.HsDataDefn {..} = undefined
#else
isGADT GHC.HsDataDefn {..} =
  case dd_cons of
    (GHC.L _ GHC.ConDeclGADT {}:_) -> True
    _                              -> False
#endif
