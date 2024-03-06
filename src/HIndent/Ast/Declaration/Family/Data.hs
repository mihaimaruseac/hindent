{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family.Data
  ( DataFamily
  , mkDataFamily
  ) where

import Control.Monad
import qualified GHC.Types.Basic as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Applicative
import HIndent.Ast.NodeComments hiding (fromEpAnn)
import HIndent.Ast.Type
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
data DataFamily = DataFamily
  { isTopLevel :: Bool
  , name :: String
  , typeVariables :: [WithComments (TypeVariable (GHC.HsBndrVis GHC.GhcPs))]
  , signature :: Maybe (WithComments Type)
  }
#else
data DataFamily = DataFamily
  { isTopLevel :: Bool
  , name :: String
  , typeVariables :: [WithComments (TypeVariable ())]
  , signature :: Maybe (WithComments Type)
  }
#endif
instance CommentExtraction DataFamily where
  nodeComments DataFamily {} = NodeComments [] [] []

instance Pretty DataFamily where
  pretty' DataFamily {..} = do
    string "data "
    when isTopLevel $ string "family "
    string name
    spacePrefixed $ fmap pretty typeVariables
    whenJust signature $ \sig -> space >> pretty sig

mkDataFamily :: GHC.FamilyDecl GHC.GhcPs -> DataFamily
mkDataFamily GHC.FamilyDecl {fdTyVars = GHC.HsQTvs {..}, ..}
  | GHC.DataFamily <- fdInfo
  , Nothing <- fdInjectivityAnn = DataFamily {..}
  | otherwise = error "Not a DataFamily"
  where
    isTopLevel =
      case fdTopLevel of
        GHC.TopLevel -> True
        GHC.NotTopLevel -> False
    name = showOutputable fdLName
    typeVariables = fmap (fmap mkTypeVariable . fromGenLocated) hsq_explicit
    signature =
      case GHC.unLoc fdResultSig of
        GHC.NoSig {} -> Nothing
        GHC.KindSig _ kind -> Just $ mkType <$> fromGenLocated kind
        GHC.TyVarSig {} ->
          error
            "Data family should never have this AST node. If you see this error, please report it to the HIndent maintainers."
