{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family
  ( FamilyDeclaration
  , mkFamilyDeclaration
  ) where

import           Control.Monad
import qualified GHC.Types.Basic                                as GHC
import           HIndent.Applicative
import           HIndent.Ast.Declaration.Family.DataOrType
import           HIndent.Ast.Declaration.Family.Injectivity
import           HIndent.Ast.Declaration.Family.ResultSignature
import           HIndent.Ast.NodeComments                       hiding
                                                                (fromEpAnn)
import           HIndent.Ast.Type.Variable
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs             as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data FamilyDeclaration = FamilyDeclaration
  { dataOrType    :: DataOrType
  , isTopLevel    :: Bool
  , name          :: String
  , typeVariables :: [WithComments (TypeVariable ())]
  , signature     :: WithComments ResultSignature
  , injectivity   :: Maybe (WithComments Injectivity)
  , openOrClosed  :: OpenOrClosed
  }

instance CommentExtraction FamilyDeclaration where
  nodeComments FamilyDeclaration {} = NodeComments [] [] []

data OpenOrClosed
  = Open
  | Closed [GHC.LTyFamInstEqn GHC.GhcPs]

instance Pretty FamilyDeclaration where
  pretty' FamilyDeclaration {..} = do
    pretty dataOrType
    space
    when isTopLevel $ string "family "
    string name
    spacePrefixed $ fmap pretty typeVariables
    case getNode signature of
      ResultSignature GHC.NoSig {}    -> pure ()
      ResultSignature GHC.TyVarSig {} -> string " = " >> pretty signature
      _                               -> space >> pretty signature
    whenJust injectivity $ \x -> string " | " >> pretty x
    case openOrClosed of
      Open -> pure ()
      Closed xs ->
        string " where" >> newline >> indentedBlock (lined $ fmap pretty xs)

mkFamilyDeclaration :: GHC.FamilyDecl GHC.GhcPs -> FamilyDeclaration
mkFamilyDeclaration GHC.FamilyDecl {fdTyVars = GHC.HsQTvs {..}, ..} =
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
    injectivity = fmap (fmap Injectivity . fromGenLocated) fdInjectivityAnn
    openOrClosed =
      case fdInfo of
        GHC.ClosedTypeFamily (Just xs) -> Closed xs
        _                              -> Open
