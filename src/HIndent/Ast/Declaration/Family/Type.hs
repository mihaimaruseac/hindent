{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family.Type
  ( TypeFamily
  , mkTypeFamily
  ) where

import           Control.Monad
import qualified GHC.Types.Basic                                     as GHC
import           HIndent.Applicative
import           HIndent.Ast.Declaration.Family.Type.Injectivity
import           HIndent.Ast.Declaration.Family.Type.ResultSignature
import           HIndent.Ast.NodeComments                            hiding
                                                                     (fromEpAnn)
import           HIndent.Ast.Type.Variable
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs                  as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
data TypeFamily = TypeFamily
  { isTopLevel    :: Bool
  , name          :: String
  , typeVariables :: [WithComments (TypeVariable (GHC.HsBndrVis GHC.GhcPs))]
  , signature     :: WithComments ResultSignature
  , injectivity   :: Maybe (WithComments Injectivity)
  , equations     :: Maybe [GHC.LTyFamInstEqn GHC.GhcPs]
  }
#else
data TypeFamily = TypeFamily
  { isTopLevel    :: Bool
  , name          :: String
  , typeVariables :: [WithComments (TypeVariable ())]
  , signature     :: WithComments ResultSignature
  , injectivity   :: Maybe (WithComments Injectivity)
  , equations     :: Maybe [GHC.LTyFamInstEqn GHC.GhcPs]
  }
#endif
instance CommentExtraction TypeFamily where
  nodeComments TypeFamily {} = NodeComments [] [] []

instance Pretty TypeFamily where
  pretty' TypeFamily {..} = do
    string "type "
    when isTopLevel $ string "family "
    string name
    spacePrefixed $ fmap pretty typeVariables
    case getNode signature of
      ResultSignature GHC.NoSig {}    -> pure ()
      ResultSignature GHC.TyVarSig {} -> string " = " >> pretty signature
      _                               -> space >> pretty signature
    whenJust injectivity $ \x -> string " | " >> pretty x
    whenJust equations $ \xs ->
      string " where" >> newline >> indentedBlock (lined $ fmap pretty xs)

mkTypeFamily :: GHC.FamilyDecl GHC.GhcPs -> TypeFamily
mkTypeFamily GHC.FamilyDecl {fdTyVars = GHC.HsQTvs {..}, ..} = TypeFamily {..}
  where
    isTopLevel =
      case fdTopLevel of
        GHC.TopLevel    -> True
        GHC.NotTopLevel -> False
    name = showOutputable fdLName
    typeVariables = fmap (fmap mkTypeVariable . fromGenLocated) hsq_explicit
    signature = ResultSignature <$> fromGenLocated fdResultSig
    injectivity = fmap (fmap Injectivity . fromGenLocated) fdInjectivityAnn
    equations =
      case fdInfo of
        GHC.DataFamily                 -> error "Not a TypeFamily"
        GHC.OpenTypeFamily             -> Nothing
        GHC.ClosedTypeFamily Nothing   -> Just []
        GHC.ClosedTypeFamily (Just xs) -> Just xs
