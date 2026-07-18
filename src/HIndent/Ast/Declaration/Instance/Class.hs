{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Class
  ( ClassInstance
  , mkClassInstance
  ) where

import Control.Monad
import HIndent.Applicative
import HIndent.Ast.Declaration.Instance.Class.Body
import HIndent.Ast.Declaration.Instance.Class.OverlapMode
import HIndent.Ast.Type (InstDeclType, mkInstDeclType)
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
#endif
data ClassInstance = ClassInstance
  { overlapMode :: Maybe (WithComments OverlapMode)
  , body :: ClassInstanceBody
  , instanceType :: WithComments InstDeclType
  }

instance Pretty ClassInstance where
  pretty ClassInstance {..} = do
    string "instance " |=> do
      whenJust overlapMode $ \x -> do
        pretty x
        space
      pretty instanceType |=> when (hasClassInstanceBody body) (string " where")
    when (hasClassInstanceBody body) $ do
      newline
      indentedBlock $ pretty body

mkClassInstance :: GHC.InstDecl GHC.GhcPs -> Maybe ClassInstance
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkClassInstance GHC.ClsInstD {cid_inst = GHC.ClsInstDecl {..}} =
  Just
    $ ClassInstance
        { instanceType =
            flattenComments
              $ mkInstDeclType <$> mkWithCommentsFromGenLocated cid_poly_ty
        , body =
            mkClassInstanceBody
              cid_sigs
              cid_binds
              cid_tyfam_insts
              cid_datafam_insts
        , overlapMode =
            fmap mkOverlapMode . mkWithCommentsFromGenLocated
              <$> cid_overlap_mode
        }
#else
mkClassInstance GHC.ClsInstD {cid_inst = GHC.ClsInstDecl {..}} =
  Just
    $ ClassInstance
        { instanceType =
            flattenComments
              $ mkInstDeclType <$> mkWithCommentsFromGenLocated cid_poly_ty
        , body =
            mkClassInstanceBody
              cid_sigs
              (GHC.bagToList cid_binds)
              cid_tyfam_insts
              cid_datafam_insts
        , overlapMode =
            fmap mkOverlapMode . mkWithCommentsFromGenLocated
              <$> cid_overlap_mode
        }
#endif
mkClassInstance _ = Nothing
