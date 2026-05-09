{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family.Data
  ( DataFamily
  , mkDataFamily
  ) where

import Control.Monad
import qualified GHC.Types.Basic as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Applicative
import HIndent.Ast.Name.Prefix
import HIndent.Ast.Type
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data DataFamily = DataFamily
  { isTopLevel :: Bool
  , name :: WithComments PrefixName
  , typeVariables :: [WithComments TypeVariable]
  , signature :: Maybe (WithComments Type)
  }

instance Pretty DataFamily where
  pretty DataFamily {..} = do
    string "data "
    when isTopLevel $ string "family "
    pretty name
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
        GHC.TopLevel -> True
        GHC.NotTopLevel -> False
    name = mkWithCommentsFromGenLocated $ fmap mkPrefixName fdLName
    typeVariables =
      fmap (fmap mkTypeVariable . mkWithCommentsFromGenLocated) hsq_explicit
    signature =
      case GHC.unLoc fdResultSig of
        GHC.NoSig {} -> Nothing
        GHC.KindSig _ kind ->
          Just $ mkType <$> mkWithCommentsFromGenLocated kind
        GHC.TyVarSig {} ->
          error
            "Data family should never have this AST node. If you see this error, please report it to the HIndent maintainers."
