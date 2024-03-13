{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family.Type
  ( TypeFamily(..)
  , mkTypeFamily
  ) where

import qualified GHC.Types.Basic as GHC
import HIndent.Ast.Declaration.Family.Type.Injectivity
import HIndent.Ast.Declaration.Family.Type.ResultSignature
import HIndent.Ast.NodeComments hiding (fromEpAnn)
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data TypeFamily = TypeFamily
  { isTopLevel :: Bool
  , name :: String
  , typeVariables :: [WithComments TypeVariable]
  , signature :: WithComments ResultSignature
  , injectivity :: Maybe (WithComments Injectivity)
  , equations :: Maybe [GHC.LTyFamInstEqn GHC.GhcPs]
  }

instance CommentExtraction TypeFamily where
  nodeComments TypeFamily {} = NodeComments [] [] []

mkTypeFamily :: GHC.FamilyDecl GHC.GhcPs -> Maybe TypeFamily
mkTypeFamily GHC.FamilyDecl {fdTyVars = GHC.HsQTvs {..}, ..}
  | GHC.DataFamily <- fdInfo = Nothing
  | otherwise = Just TypeFamily {..}
  where
    isTopLevel =
      case fdTopLevel of
        GHC.TopLevel -> True
        GHC.NotTopLevel -> False
    name = showOutputable fdLName
    typeVariables = fmap (fmap mkTypeVariable . fromGenLocated) hsq_explicit
    signature = ResultSignature <$> fromGenLocated fdResultSig
    injectivity = fmap (fmap Injectivity . fromGenLocated) fdInjectivityAnn
    equations =
      case fdInfo of
        GHC.DataFamily -> error "Not a TypeFamily"
        GHC.OpenTypeFamily -> Nothing
        GHC.ClosedTypeFamily Nothing -> Just []
        GHC.ClosedTypeFamily (Just xs) -> Just xs
