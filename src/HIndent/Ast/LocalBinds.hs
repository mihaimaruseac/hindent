{-# LANGUAGE CPP #-}

module HIndent.Ast.LocalBinds
  ( LocalBinds
  , mkLocalBinds
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
#endif
import qualified HIndent.Ast.GhcOrdered.BindGroupElements as BGE
import HIndent.Ast.LocalBinds.ImplicitBindings
  ( ImplicitBindings
  , mkImplicitBindings
  )
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.WithComments (WithComments, fromEpAnn)
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data LocalBinds
  = Value
      { sigBindFamilies :: BGE.BindGroupElements
      }
  | ImplicitParameters
      { implicitBindings :: ImplicitBindings
      }

instance CommentExtraction LocalBinds where
  nodeComments Value {} = NodeComments [] [] []
  nodeComments ImplicitParameters {} = NodeComments [] [] []

instance Pretty LocalBinds where
  pretty' Value {sigBindFamilies = families} =
    lined $ pretty <$> BGE.bindGroupElements families
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
     GHC.HsValBindsLR GHC.GhcPs GHC.GhcPs -> BGE.BindGroupElements
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkSigBindFamilies (GHC.ValBinds _ binds sigs) =
  BGE.mkSortedBindGroupElements sigs binds
#else
mkSigBindFamilies (GHC.ValBinds _ bindBag sigs) =
  BGE.mkSortedBindGroupElements sigs (GHC.bagToList bindBag)
#endif
mkSigBindFamilies GHC.XValBindsLR {} =
  error "`ghc-lib-parser` never generates this AST node."
