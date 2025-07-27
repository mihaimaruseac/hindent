{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Type.Forall
  ( Forall
  , mkForall
  ) where

import HIndent.Ast.NodeComments hiding (fromEpAnn)
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Forall
  = Visible [WithComments TypeVariable] -- forall a b c ->
  | Invisible [WithComments TypeVariable] -- forall a b c.

instance CommentExtraction Forall where
  nodeComments _ = NodeComments [] [] []

instance Pretty Forall where
  pretty' (Visible vars) = do
    string "forall "
    spaced $ fmap pretty vars
    string " ->"
  pretty' (Invisible vars) = do
    string "forall "
    spaced $ fmap pretty vars
    dot

mkForall :: GHC.HsForAllTelescope GHC.GhcPs -> WithComments Forall
mkForall GHC.HsForAllVis {..} =
  fromEpAnn hsf_xvis
    $ Visible
    $ fmap (fmap mkTypeVariable . fromGenLocated) hsf_vis_bndrs
mkForall GHC.HsForAllInvis {..} =
  fromEpAnn hsf_xinvis
    $ Invisible
    $ fmap (fmap mkTypeVariable . fromGenLocated) hsf_invis_bndrs
