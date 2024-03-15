{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Family.Type
  ( TypeFamilyInstance(..)
  , mkTypeFamilyInstance
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty.NodeComments

data TypeFamilyInstance = TypeFamilyInstance
  { name  :: GHC.LIdP GHC.GhcPs
  , types :: GHC.HsTyPats GHC.GhcPs
  , bind  :: GHC.LHsType GHC.GhcPs
  }

instance CommentExtraction TypeFamilyInstance where
  nodeComments TypeFamilyInstance {} = NodeComments [] [] []

mkTypeFamilyInstance :: GHC.InstDecl GHC.GhcPs -> Maybe TypeFamilyInstance
mkTypeFamilyInstance GHC.TyFamInstD {GHC.tfid_inst = GHC.TyFamInstDecl {GHC.tfid_eqn = GHC.FamEqn {..}}} =
  Just $ TypeFamilyInstance {..}
  where
    name = feqn_tycon
    types = feqn_pats
    bind = feqn_rhs
mkTypeFamilyInstance _ = Nothing
