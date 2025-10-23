{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.LocalBinds
  ( LocalBinds(..)
  , ValueBindGroup(..)
  , ImplicitParameterBinds(..)
  , ImplicitParameterBind(..)
  , mkLocalBindsWithComments
  , mkLocalBindsFromLocated
  , hasLocalBinds
  , valueLocalBinds
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
import GHC.Types.SrcLoc (unLoc)
#endif
import {-# SOURCE #-} HIndent.Ast.Expression (Expression, mkExpression)
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.Type.ImplicitParameterName
import HIndent.Ast.WithComments
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import qualified HIndent.Pretty.SigBindFamily as SBF

data LocalBinds
  = ValueLocalBinds ValueBindGroup
  | ImplicitParameterLocalBinds ImplicitParameterBinds

newtype ValueBindGroup = ValueBindGroup
  { sigBindFamilies :: [WithComments SBF.SigBindFamily]
  }

newtype ImplicitParameterBinds = ImplicitParameterBinds
  { implicitBindings :: [WithComments ImplicitParameterBind]
  }

data ImplicitParameterBind = ImplicitParameterBind
  { name :: WithComments ImplicitParameterName
  , expression :: WithComments Expression
  }

instance CommentExtraction LocalBinds where
  nodeComments ValueLocalBinds {} = NodeComments [] [] []
  nodeComments ImplicitParameterLocalBinds {} = NodeComments [] [] []

instance CommentExtraction ValueBindGroup where
  nodeComments ValueBindGroup {} = NodeComments [] [] []

instance CommentExtraction ImplicitParameterBinds where
  nodeComments ImplicitParameterBinds {} = NodeComments [] [] []

instance CommentExtraction ImplicitParameterBind where
  nodeComments ImplicitParameterBind {} = NodeComments [] [] []

instance Pretty LocalBinds where
  pretty' (ValueLocalBinds binds) = pretty binds
  pretty' (ImplicitParameterLocalBinds binds) = pretty binds

instance Pretty ValueBindGroup where
  pretty' ValueBindGroup {..} = lined $ fmap pretty sigBindFamilies

instance Pretty ImplicitParameterBinds where
  pretty' ImplicitParameterBinds {..} = lined $ fmap pretty implicitBindings

instance Pretty ImplicitParameterBind where
  pretty' ImplicitParameterBind {..} =
    spaced [pretty name, string "=", pretty expression]

mkLocalBindsWithComments ::
     GHC.HsLocalBinds GHC.GhcPs -> WithComments LocalBinds
mkLocalBindsWithComments (GHC.HsValBinds ann binds) =
  fromEpAnn ann
    $ ValueLocalBinds ValueBindGroup {sigBindFamilies = mkSigBindFamilies binds}
mkLocalBindsWithComments (GHC.HsIPBinds ann binds) =
  fromEpAnn ann
    $ ImplicitParameterLocalBinds
    $ ImplicitParameterBinds {implicitBindings = mkImplicitBindings binds}
mkLocalBindsWithComments GHC.EmptyLocalBinds {} =
  mkWithComments
    $ ValueLocalBinds ValueBindGroup {sigBindFamilies = []}

mkLocalBindsFromLocated ::
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
     GHC.XRec GHC.GhcPs (GHC.HsLocalBinds GHC.GhcPs) -> WithComments LocalBinds
mkLocalBindsFromLocated = mkLocalBindsWithComments . unLoc
#else
     GHC.HsLocalBinds GHC.GhcPs -> WithComments LocalBinds
mkLocalBindsFromLocated = mkLocalBindsWithComments
#endif

hasLocalBinds :: LocalBinds -> Bool
hasLocalBinds (ValueLocalBinds ValueBindGroup {..}) =
  not $ null sigBindFamilies
hasLocalBinds (ImplicitParameterLocalBinds ImplicitParameterBinds {..}) =
  not $ null implicitBindings

valueLocalBinds :: LocalBinds -> Maybe ValueBindGroup
valueLocalBinds (ValueLocalBinds binds) = Just binds
valueLocalBinds _ = Nothing

mkSigBindFamilies ::
     GHC.HsValBindsLR GHC.GhcPs GHC.GhcPs -> [WithComments SBF.SigBindFamily]
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkSigBindFamilies (GHC.ValBinds _ binds sigs) =
  fmap fromGenLocated $ SBF.mkSortedLSigBindFamilyList sigs binds [] [] []
#else
mkSigBindFamilies (GHC.ValBinds _ bindBag sigs) =
  fromGenLocated
    <$>
      SBF.mkSortedLSigBindFamilyList sigs (GHC.bagToList bindBag) [] [] []
#endif
mkSigBindFamilies GHC.XValBindsLR {} =
  error "`ghc-lib-parser` never generates this AST node."

mkImplicitBindings ::
     GHC.HsIPBinds GHC.GhcPs -> [WithComments ImplicitParameterBind]
mkImplicitBindings (GHC.IPBinds _ binds) =
  fmap (fmap mkImplicitParameterBind . fromGenLocated) binds

mkImplicitParameterBind :: GHC.IPBind GHC.GhcPs -> ImplicitParameterBind
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkImplicitParameterBind (GHC.IPBind _ lhs rhs) =
  ImplicitParameterBind
    { name = mkImplicitParameterName <$> fromGenLocated lhs
    , expression = mkExpression <$> fromGenLocated rhs
    }
#else
mkImplicitParameterBind (GHC.IPBind _ (Left lhs) rhs) =
  ImplicitParameterBind
    { name = mkImplicitParameterName <$> fromGenLocated lhs
    , expression = mkExpression <$> fromGenLocated rhs
    }
mkImplicitParameterBind (GHC.IPBind _ (Right _) _) =
  error "`ghc-lib-parser` never generates this AST node."
#endif
