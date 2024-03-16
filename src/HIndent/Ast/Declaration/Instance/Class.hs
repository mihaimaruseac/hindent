{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Class
  ( ClassInstance
  , mkClassInstance
  ) where

import Control.Monad
import qualified GHC.Data.Bag as GHC
import HIndent.Applicative
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.SigBindFamily
import HIndent.Pretty.Types

newtype ClassInstance =
  ClassInstance (GHC.ClsInstDecl GHC.GhcPs)

instance CommentExtraction ClassInstance where
  nodeComments ClassInstance {} = NodeComments [] [] []

instance Pretty ClassInstance where
  pretty' (ClassInstance GHC.ClsInstDecl {..}) = do
    string "instance " |=> do
      whenJust cid_overlap_mode $ \x -> do
        pretty x
        space
      pretty (fmap HsSigTypeInsideInstDecl cid_poly_ty)
        |=> unless (null sigsAndMethods) (string " where")
    unless (null sigsAndMethods) $ do
      newline
      indentedBlock $ lined $ fmap pretty sigsAndMethods
    where
      sigsAndMethods =
        mkSortedLSigBindFamilyList
          cid_sigs
          (GHC.bagToList cid_binds)
          []
          cid_tyfam_insts
          cid_datafam_insts

mkClassInstance :: GHC.InstDecl GHC.GhcPs -> Maybe ClassInstance
mkClassInstance GHC.ClsInstD {..} = Just $ ClassInstance cid_inst
mkClassInstance _ = Nothing
