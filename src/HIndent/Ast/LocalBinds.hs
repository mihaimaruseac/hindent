{-# LANGUAGE CPP #-}

module HIndent.Ast.LocalBinds
  ( LocalBinds
  , mkLocalBinds
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
#endif
import HIndent.Ast.LocalBinds.Declarations
  ( LocalBindDeclarations
  , getBindGroupElements
  , mkLocalBindDeclarations
  )
import HIndent.Ast.LocalBinds.ImplicitBindings
  ( ImplicitBindings
  , mkImplicitBindings
  )
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.WithComments (WithComments, fromEpAnn, fromGenLocated)
import HIndent.GhcOrdered.BindGroupElement
  ( BindGroupElement
  , mkSortedBindGroupElements
  )
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data LocalBinds
  = Value
      { bindings :: LocalBindDeclarations
      }
  | ImplicitParameters
      { implicitBindings :: ImplicitBindings
      }

instance CommentExtraction LocalBinds where
  nodeComments Value {} = NodeComments [] [] []
  nodeComments ImplicitParameters {} = NodeComments [] [] []

instance CommentExtraction LocalBindDeclarations where
  nodeComments _ = NodeComments [] [] []

instance Pretty LocalBindDeclarations where
  pretty' = lined . fmap pretty . getBindGroupElements

instance Pretty LocalBinds where
  pretty' Value {bindings = decls} = pretty decls
  pretty' (ImplicitParameters {implicitBindings = binds}) = pretty binds

mkLocalBinds :: GHC.HsLocalBinds GHC.GhcPs -> Maybe (WithComments LocalBinds)
mkLocalBinds (GHC.HsValBinds ann binds) =
  Just
    $ fromEpAnn ann
    $ Value {bindings = mkLocalBindDeclarations $ mkLocalBindElements binds}
mkLocalBinds (GHC.HsIPBinds ann binds) =
  Just
    $ fromEpAnn ann
    $ ImplicitParameters {implicitBindings = mkImplicitBindings binds}
mkLocalBinds GHC.EmptyLocalBinds {} = Nothing

mkLocalBindElements ::
     GHC.HsValBindsLR GHC.GhcPs GHC.GhcPs -> [WithComments BindGroupElement]
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkLocalBindElements (GHC.ValBinds _ binds sigs) =
  fromGenLocated <$> mkSortedBindGroupElements sigs binds
#else
mkLocalBindElements (GHC.ValBinds _ bindBag sigs) =
  fromGenLocated <$> mkSortedBindGroupElements sigs (GHC.bagToList bindBag)
#endif
mkLocalBindElements GHC.XValBindsLR {} =
  error "`ghc-lib-parser` never generates this AST node."
