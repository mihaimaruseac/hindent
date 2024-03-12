{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Class
  ( ClassInstance(..)
  , mkClassInstance
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments

newtype ClassInstance =
  ClassInstance (GHC.ClsInstDecl GHC.GhcPs)

instance CommentExtraction ClassInstance where
  nodeComments ClassInstance {} = NodeComments [] [] []

mkClassInstance :: GHC.InstDecl GHC.GhcPs -> Maybe ClassInstance
mkClassInstance GHC.ClsInstD {..} = Just $ ClassInstance cid_inst
mkClassInstance _ = Nothing
