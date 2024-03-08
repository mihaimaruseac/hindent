{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family.Data
  ( DataFamily
  , mkDataFamily
  ) where

import           Control.Monad
import qualified GHC.Types.Basic                    as GHC
import qualified GHC.Types.SrcLoc                   as GHC
import           HIndent.Applicative
import           HIndent.Ast.NodeComments           hiding (fromEpAnn)
import           HIndent.Ast.Type
import           HIndent.Ast.Type.Variable
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data DataFamily = DataFamily
  { isTopLevel    :: Bool
  , name          :: String
  , typeVariables :: [WithComments TypeVariable]
  , signature     :: Maybe (WithComments Type)
  }

instance CommentExtraction DataFamily where
  nodeComments DataFamily {} = NodeComments [] [] []

instance Pretty DataFamily where
  pretty' DataFamily {..} = do
    string "data "
    when isTopLevel $ string "family "
    string name
    spacePrefixed $ fmap pretty typeVariables
    whenJust signature $ \sig -> space >> pretty sig

mkDataFamily :: GHC.FamilyDecl GHC.GhcPs -> Maybe DataFamily
mkDataFamily GHC.FamilyDecl {fdTyVars = GHC.HsQTvs {..}, ..}
  | GHC.DataFamily <- fdInfo
  , Nothing <- fdInjectivityAnn = Just DataFamily {..}
  | otherwise = Nothing
  where
    isTopLevel =
      case fdTopLevel of
        GHC.TopLevel    -> True
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
