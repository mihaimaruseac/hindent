{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Family.Data
  ( DataFamilyInstance
  , mkDataFamilyInstance
  ) where

import qualified GHC.Hs as GG
import HIndent.Ast.Declaration.Data.Body
import HIndent.Ast.Declaration.Data.NewOrData
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data DataFamilyInstance = DataFamilyInstance
  { newOrData :: NewOrData
  , name :: GHC.LIdP GHC.GhcPs
  , types :: GHC.HsTyPats GHC.GhcPs
  , body :: DataBody
  }

instance CommentExtraction DataFamilyInstance where
  nodeComments DataFamilyInstance {} = NodeComments [] [] []

instance Pretty DataFamilyInstance where
  pretty' DataFamilyInstance {..} = do
    spaced
      $ pretty newOrData : string "instance" : pretty name : fmap pretty types
    pretty body

mkDataFamilyInstance ::
     GHC.FamEqn GHC.GhcPs (GHC.HsDataDefn GHC.GhcPs) -> DataFamilyInstance
mkDataFamilyInstance GHC.FamEqn {..} = DataFamilyInstance {..}
  where
    newOrData = mkNewOrData feqn_rhs
    name = feqn_tycon
    types = feqn_pats
    body = mkDataBody feqn_rhs
