{-# LANGUAGE CPP #-}

module HIndent.Ast.LocalBinds
  ( LocalBinds
  , mkLocalBinds
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
#endif
import HIndent.Ast.LocalBinds.ImplicitBindings
  ( ImplicitBindings
  , mkImplicitBindings
  )
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.WithComments (WithComments, fromEpAnn, fromGenLocated)
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import qualified HIndent.Pretty.SigBindFamily as SBF

data LocalBinds
  = Value
      { sigBindFamilies :: [WithComments SBF.SigBindFamily]
      }
  | ImplicitParameters
      { implicitBindings :: ImplicitBindings
      }

instance CommentExtraction LocalBinds where
  nodeComments Value {} = NodeComments [] [] []
  nodeComments ImplicitParameters {} = NodeComments [] [] []

instance Pretty LocalBinds where
  pretty' Value {sigBindFamilies = families} = lined $ fmap pretty families
  pretty' (ImplicitParameters {implicitBindings = binds}) = pretty binds

mkLocalBinds :: GHC.HsLocalBinds GHC.GhcPs -> Maybe (WithComments LocalBinds)
mkLocalBinds (GHC.HsValBinds ann binds) =
  Just $ fromEpAnn ann $ Value {sigBindFamilies = mkSigBindFamilies binds}
mkLocalBinds (GHC.HsIPBinds ann binds) =
  Just
    $ fromEpAnn ann
    $ ImplicitParameters {implicitBindings = mkImplicitBindings binds}
mkLocalBinds GHC.EmptyLocalBinds {} = Nothing

mkSigBindFamilies ::
     GHC.HsValBindsLR GHC.GhcPs GHC.GhcPs -> [WithComments SBF.SigBindFamily]
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkSigBindFamilies (GHC.ValBinds _ binds sigs) =
  fmap fromGenLocated $ SBF.mkSortedLSigBindFamilyList sigs binds [] [] []
#else
mkSigBindFamilies (GHC.ValBinds _ bindBag sigs) =
  fromGenLocated
    <$> SBF.mkSortedLSigBindFamilyList sigs (GHC.bagToList bindBag) [] [] []
#endif
mkSigBindFamilies GHC.XValBindsLR {} =
  error "`ghc-lib-parser` never generates this AST node."
