{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family
  ( FamilyDeclaration
  , mkFamilyDeclaration
  ) where

import qualified GHC.Types.Basic                    as GHC
import qualified GHC.Types.SrcLoc                   as GHC
import           HIndent.Applicative
import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data FamilyDeclaration = FamilyDeclaration
  { dataOrType :: DataOrType
  , family'    :: GHC.FamilyDecl GHC.GhcPs
  }

data DataOrType
  = Data
  | Type

instance CommentExtraction FamilyDeclaration where
  nodeComments FamilyDeclaration {} = NodeComments [] [] []

instance Pretty FamilyDeclaration where
  pretty' FamilyDeclaration {family' = GHC.FamilyDecl {..}} = do
    string $
      case fdInfo of
        GHC.DataFamily          -> "data"
        GHC.OpenTypeFamily      -> "type"
        GHC.ClosedTypeFamily {} -> "type"
    case fdTopLevel of
      GHC.TopLevel    -> string " family "
      GHC.NotTopLevel -> space
    pretty fdLName
    spacePrefixed $ pretty <$> GHC.hsq_explicit fdTyVars
    case GHC.unLoc fdResultSig of
      GHC.NoSig {} -> pure ()
      GHC.TyVarSig {} -> do
        string " = "
        pretty fdResultSig
      _ -> do
        space
        pretty fdResultSig
    whenJust fdInjectivityAnn $ \x -> do
      string " | "
      pretty x
    case fdInfo of
      GHC.ClosedTypeFamily (Just xs) -> do
        string " where"
        newline
        indentedBlock $ lined $ fmap pretty xs
      _ -> pure ()

mkFamilyDeclaration :: GHC.FamilyDecl GHC.GhcPs -> FamilyDeclaration
mkFamilyDeclaration family' = FamilyDeclaration {..}
  where
    dataOrType =
      case GHC.fdInfo family' of
        GHC.DataFamily          -> Data
        GHC.OpenTypeFamily      -> Type
        GHC.ClosedTypeFamily {} -> Type
