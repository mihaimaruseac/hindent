{-# LANGUAGE RecordWildCards, CPP #-}

module HIndent.Ast.Declaration.Instance.Family.Data
  ( DataFamilyInstance
  , mkDataFamilyInstance
  ) where

import qualified GHC.Hs as GG
import HIndent.Ast.Declaration.Data.Body
import HIndent.Ast.Declaration.Data.NewOrData
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
data DataFamilyInstance = DataFamilyInstance
  { newOrData :: NewOrData
  , name :: WithComments PrefixName
  , types :: GHC.HsFamEqnPats GHC.GhcPs
  , body :: DataBody
  }
#else
data DataFamilyInstance = DataFamilyInstance
  { newOrData :: NewOrData
  , name :: WithComments PrefixName
  , types :: GHC.HsTyPats GHC.GhcPs
  , body :: DataBody
  }
#endif
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
    name = fromGenLocated $ fmap mkPrefixName feqn_tycon
    types = feqn_pats
    body = mkDataBody feqn_rhs
