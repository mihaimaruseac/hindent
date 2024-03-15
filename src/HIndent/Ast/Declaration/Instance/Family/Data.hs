{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Family.Data
  ( DataFamilyInstance(..)
  , mkDataFamilyInstance
  ) where

import qualified GHC.Hs                                 as GG
import           HIndent.Ast.Declaration.Data.NewOrData
import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs     as GHC
import           HIndent.Pretty.NodeComments

data DataFamilyInstance = DataFamilyInstance
  { newOrData :: NewOrData
  , name      :: GHC.LIdP GHC.GhcPs
  , types     :: GHC.HsTyPats GHC.GhcPs
  , inst      :: GHC.DataFamInstDecl GHC.GhcPs
  }

instance CommentExtraction DataFamilyInstance where
  nodeComments DataFamilyInstance {} = NodeComments [] [] []

mkDataFamilyInstance :: GHC.InstDecl GHC.GhcPs -> Maybe DataFamilyInstance
mkDataFamilyInstance GHC.DataFamInstD {..} = Just $ DataFamilyInstance {..}
  where
    newOrData = mkNewOrData $ GHC.feqn_rhs $ GHC.dfid_eqn dfid_inst
    name = GHC.feqn_tycon $ GHC.dfid_eqn dfid_inst
    types = GHC.feqn_pats $ GHC.dfid_eqn dfid_inst
    inst = dfid_inst
mkDataFamilyInstance _ = Nothing
