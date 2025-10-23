{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.LocalBinds
  ( LocalBinds(..)
  , ImplicitParameterBind(..)
  , mkLocalBindsWithComments
  , mkLocalBindsFromLocated
  , hasLocalBinds
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

data LocalBinds = LocalBinds
  { sigBindFamilies :: [WithComments SBF.SigBindFamily]
  , implicitBindings :: [WithComments ImplicitParameterBind]
  }

data ImplicitParameterBind = ImplicitParameterBind
  { name :: WithComments ImplicitParameterName
  , expression :: WithComments Expression
  }

instance CommentExtraction LocalBinds where
  nodeComments LocalBinds {} = NodeComments [] [] []

instance CommentExtraction ImplicitParameterBind where
  nodeComments ImplicitParameterBind {} = NodeComments [] [] []

instance Pretty LocalBinds where
  pretty' LocalBinds {..} =
    lined $ fmap pretty sigBindFamilies ++ fmap pretty implicitBindings

instance Pretty ImplicitParameterBind where
  pretty' ImplicitParameterBind {..} =
    spaced [pretty name, string "=", pretty expression]

mkLocalBindsWithComments ::
     GHC.HsLocalBinds GHC.GhcPs -> WithComments LocalBinds
mkLocalBindsWithComments (GHC.HsValBinds ann binds) =
  fromEpAnn ann
    $ LocalBinds
        { sigBindFamilies = mkSigBindFamilies binds
        , implicitBindings = []
        }
mkLocalBindsWithComments (GHC.HsIPBinds ann binds) =
  fromEpAnn ann
    $ LocalBinds
        { sigBindFamilies = []
        , implicitBindings = mkImplicitBindings binds
        }
mkLocalBindsWithComments GHC.EmptyLocalBinds {} =
  mkWithComments
    $ LocalBinds {sigBindFamilies = [], implicitBindings = []}

mkLocalBindsFromLocated ::
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
     GHC.XRec GHC.GhcPs (GHC.HsLocalBinds GHC.GhcPs) -> WithComments LocalBinds
mkLocalBindsFromLocated = mkLocalBindsWithComments . unLoc
#else
     GHC.HsLocalBinds GHC.GhcPs -> WithComments LocalBinds
mkLocalBindsFromLocated = mkLocalBindsWithComments
#endif

hasLocalBinds :: LocalBinds -> Bool
hasLocalBinds LocalBinds {..} =
  not (null sigBindFamilies && null implicitBindings)

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
