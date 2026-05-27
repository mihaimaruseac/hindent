{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.LocalBinds
  ( LocalBinds
  , mkLocalBinds
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
#endif
import HIndent.Ast.LocalBinds.Declaration.Collection
import HIndent.Ast.LocalBinds.ImplicitBindings
  ( ImplicitBindings
  , mkImplicitBindings
  )
import HIndent.Ast.WithComments (WithComments, fromEpAnn)
import {-# SOURCE #-} HIndent.Pretty (Pretty(..))
import HIndent.Pretty.NodeComments (CommentExtraction(..), emptyNodeComments)

data LocalBinds
  = Value
      { declarations :: LocalDeclarationCollection
      }
  | ImplicitParameters
      { implicitBindings :: ImplicitBindings
      }

instance CommentExtraction LocalBinds where
  nodeComments _ = emptyNodeComments

instance Pretty LocalBinds where
  pretty' Value {..} = pretty' declarations
  pretty' ImplicitParameters {..} = pretty' implicitBindings

mkLocalBinds :: GHC.HsLocalBinds GHC.GhcPs -> Maybe (WithComments LocalBinds)
mkLocalBinds (GHC.HsValBinds ann binds) =
  Just
    $ fromEpAnn ann
    $ Value {declarations = mkLocalDeclarationCollectionFromValBinds binds}
mkLocalBinds (GHC.HsIPBinds ann binds) =
  Just
    $ fromEpAnn ann
    $ ImplicitParameters {implicitBindings = mkImplicitBindings binds}
mkLocalBinds GHC.EmptyLocalBinds {} = Nothing

mkLocalDeclarationCollectionFromValBinds ::
     GHC.HsValBindsLR GHC.GhcPs GHC.GhcPs -> LocalDeclarationCollection
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkLocalDeclarationCollectionFromValBinds (GHC.ValBinds _ binds sigs) =
  mkLocalDeclarationCollection sigs binds
#else
mkLocalDeclarationCollectionFromValBinds (GHC.ValBinds _ bindBag sigs) =
  mkLocalDeclarationCollection sigs (GHC.bagToList bindBag)
#endif
mkLocalDeclarationCollectionFromValBinds GHC.XValBindsLR {} =
  error "`ghc-lib-parser` never generates this AST node."
