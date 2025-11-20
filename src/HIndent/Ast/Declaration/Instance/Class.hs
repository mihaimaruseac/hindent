{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Class
  ( ClassInstance
  , mkClassInstance
  ) where

import Control.Monad
import HIndent.Applicative
import HIndent.Ast.Declaration.Instance.Class.Member
  ( ClassInstanceMember
  , mkClassInstanceMembers
  )
import HIndent.Ast.Declaration.Instance.Class.OverlapMode
import HIndent.Ast.NodeComments
import HIndent.Ast.Type (InstDeclType, mkInstDeclType)
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
#endif
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data ClassInstance = ClassInstance
  { overlapMode :: Maybe (WithComments OverlapMode)
  , members :: [WithComments ClassInstanceMember]
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
      pretty cid_poly_ty |=> unless (null members) (string " where")
    unless (null members) $ do
      newline
      indentedBlock $ lined $ fmap pretty members

mkClassInstance :: GHC.InstDecl GHC.GhcPs -> Maybe ClassInstance
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkClassInstance GHC.ClsInstD {cid_inst = GHC.ClsInstDecl {..}} =
  Just
    $ ClassInstance
        { cid_poly_ty =
            flattenComments $ mkInstDeclType <$> fromGenLocated cid_poly_ty
        , overlapMode = fmap mkOverlapMode . fromGenLocated <$> cid_overlap_mode
        , members =
            mkClassInstanceMembers
              cid_sigs
              cid_binds
              cid_tyfam_insts
              cid_datafam_insts
        }
#else
mkClassInstance GHC.ClsInstD {cid_inst = GHC.ClsInstDecl {..}} =
  Just
    $ ClassInstance
        { cid_poly_ty =
            flattenComments $ mkInstDeclType <$> fromGenLocated cid_poly_ty
        , overlapMode = fmap mkOverlapMode . fromGenLocated <$> cid_overlap_mode
        , members =
            mkClassInstanceMembers
              cid_sigs
              (GHC.bagToList cid_binds)
              cid_tyfam_insts
              cid_datafam_insts
        }
#endif
mkClassInstance _ = Nothing
