{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Class
  ( ClassInstance
  , mkClassInstance
  ) where

import Control.Monad
import HIndent.Applicative
import HIndent.Ast.Declaration.Instance.Class.OverlapMode
import HIndent.Ast.GhcOrdered.InstanceMember
import HIndent.Ast.NodeComments
import HIndent.Ast.Type (InstDeclType, mkInstDeclType)
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
#endif
data ClassInstance = ClassInstance
  { overlapMode :: Maybe (WithComments OverlapMode)
  , cid_sigs :: [GHC.LSig GHC.GhcPs]
  , binds :: [GHC.LocatedA (GHC.HsBindLR GHC.GhcPs GHC.GhcPs)]
  , cid_tyfam_insts :: [GHC.LTyFamInstDecl GHC.GhcPs]
  , cid_datafam_insts :: [GHC.LDataFamInstDecl GHC.GhcPs]
  , cid_poly_ty :: WithComments InstDeclType
  }

instance CommentExtraction ClassInstance where
  nodeComments ClassInstance {} = NodeComments [] [] []

instance Pretty ClassInstance where
  pretty' (ClassInstance {..}) = do
    string "instance " |=> do
      whenJust overlapMode $ \x -> do
        pretty x
        space
      pretty cid_poly_ty |=> unless (null sigsAndMethods) (string " where")
    unless (null sigsAndMethods) $ do
      newline
      indentedBlock $ lined $ fmap pretty sigsAndMethods
    where
      sigsAndMethods =
        fromGenLocated
          <$> mkSortedInstanceMembers
                cid_sigs
                binds
                cid_tyfam_insts
                cid_datafam_insts

mkClassInstance :: GHC.InstDecl GHC.GhcPs -> Maybe ClassInstance
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkClassInstance GHC.ClsInstD {cid_inst = GHC.ClsInstDecl {..}} =
  Just
    $ ClassInstance
        { cid_poly_ty =
            flattenComments $ mkInstDeclType <$> fromGenLocated cid_poly_ty
        , binds = cid_binds
        , overlapMode = fmap mkOverlapMode . fromGenLocated <$> cid_overlap_mode
        , ..
        }
#else
mkClassInstance GHC.ClsInstD {cid_inst = GHC.ClsInstDecl {..}} =
  Just
    $ ClassInstance
        { cid_poly_ty =
            flattenComments $ mkInstDeclType <$> fromGenLocated cid_poly_ty
        , binds = GHC.bagToList cid_binds
        , overlapMode = fmap mkOverlapMode . fromGenLocated <$> cid_overlap_mode
        , ..
        }
#endif
mkClassInstance _ = Nothing
