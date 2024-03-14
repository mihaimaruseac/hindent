{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Family.Data
  ( DataFamilyInstance(..)
  , mkDataFamilyInstance
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments

newtype DataFamilyInstance = DataFamilyInstance
  { inst :: GHC.DataFamInstDecl GHC.GhcPs
  }

instance CommentExtraction DataFamilyInstance where
  nodeComments DataFamilyInstance {} = NodeComments [] [] []

mkDataFamilyInstance :: GHC.InstDecl GHC.GhcPs -> Maybe DataFamilyInstance
mkDataFamilyInstance GHC.DataFamInstD {..} = Just $ DataFamilyInstance dfid_inst
mkDataFamilyInstance _ = Nothing
