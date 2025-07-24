{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Type.UniversalQuantifier
  ( UniversalQuantifier
  , mkUniversalQuantifier
  ) where

import HIndent.Ast.NodeComments hiding (fromEpAnn)
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data UniversalQuantifier
  = Visible [WithComments TypeVariable] -- forall a b c ->
  | Invisible [WithComments TypeVariable] -- forall a b c.

instance CommentExtraction UniversalQuantifier where
  nodeComments _ = NodeComments [] [] []

instance Pretty UniversalQuantifier where
  pretty' (Visible vars) = do
    string "forall "
    spaced $ fmap pretty vars
    string " ->"
  pretty' (Invisible vars) = do
    string "forall "
    spaced $ fmap pretty vars
    dot

mkUniversalQuantifier ::
     GHC.HsForAllTelescope GHC.GhcPs -> WithComments UniversalQuantifier
mkUniversalQuantifier GHC.HsForAllVis {..} =
  fromEpAnn hsf_xvis
    $ Visible
    $ fmap (fmap mkTypeVariable . fromGenLocated) hsf_vis_bndrs
mkUniversalQuantifier GHC.HsForAllInvis {..} =
  fromEpAnn hsf_xinvis
    $ Invisible
    $ fmap (fmap mkTypeVariable . fromGenLocated) hsf_invis_bndrs
