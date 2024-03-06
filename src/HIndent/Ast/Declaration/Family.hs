{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family
  ( FamilyDeclaration
  , mkFamilyDeclaration
  ) where

import           Control.Monad
import qualified GHC.Types.Basic                           as GHC
import qualified GHC.Types.SrcLoc                          as GHC
import           HIndent.Applicative
import           HIndent.Ast.Declaration.Family.DataOrType
import           HIndent.Ast.NodeComments                  hiding (fromEpAnn)
import           HIndent.Ast.Type.Variable
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs        as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data FamilyDeclaration = FamilyDeclaration
  { dataOrType    :: DataOrType
  , isTopLevel    :: Bool
  , name          :: String
  , typeVariables :: [WithComments (TypeVariable ())]
  , signature     :: WithComments ResultSignature
  , family'       :: GHC.FamilyDecl GHC.GhcPs
  }

newtype ResultSignature =
  ResultSignature (GHC.FamilyResultSig GHC.GhcPs)

instance CommentExtraction FamilyDeclaration where
  nodeComments FamilyDeclaration {} = NodeComments [] [] []

instance Pretty FamilyDeclaration where
  pretty' FamilyDeclaration {family' = GHC.FamilyDecl {..}, ..} = do
    pretty dataOrType
    space
    when isTopLevel $ string "family "
    string name
    spacePrefixed $ fmap pretty typeVariables
    case GHC.unLoc fdResultSig of
      GHC.NoSig {}    -> pure ()
      GHC.TyVarSig {} -> string " = " >> pretty fdResultSig
      _               -> space >> pretty fdResultSig
    whenJust fdInjectivityAnn $ \x -> string " | " >> pretty x
    case fdInfo of
      GHC.ClosedTypeFamily (Just xs) ->
        string " where" >> newline >> indentedBlock (lined $ fmap pretty xs)
      _ -> pure ()

mkFamilyDeclaration :: GHC.FamilyDecl GHC.GhcPs -> FamilyDeclaration
mkFamilyDeclaration family'@GHC.FamilyDecl {fdTyVars = GHC.HsQTvs {..}, ..} =
  FamilyDeclaration {..}
  where
    dataOrType = mkDataOrType fdInfo
    isTopLevel =
      case fdTopLevel of
        GHC.TopLevel    -> True
        GHC.NotTopLevel -> False
    name = showOutputable fdLName
    typeVariables = fmap (fmap mkTypeVariable . fromGenLocated) hsq_explicit
    signature = ResultSignature <$> fromGenLocated fdResultSig
