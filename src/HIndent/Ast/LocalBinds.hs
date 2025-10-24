{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.LocalBinds
  ( LocalBinds
  , mkLocalBinds
  , hasLocalBinds
  , valueSigBindFamilies
  , implicitParameterBindings
  , ValueBindGroup(..)
  , mkValueBindGroup
  , hasValueBindGroup
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
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
  = Value
      { sigBindFamilies :: Maybe [WithComments SBF.SigBindFamily]
      }
  | ImplicitParameters
      { implicitBindings :: [WithComments
                               ( WithComments ImplicitParameterName
                               , WithComments Expression)]
      }

instance CommentExtraction LocalBinds where
  nodeComments Value {} = NodeComments [] [] []
  nodeComments ImplicitParameters {} = NodeComments [] [] []

instance Pretty LocalBinds where
  pretty' Value {sigBindFamilies = Just families} = lined $ fmap pretty families
  pretty' Value {sigBindFamilies = Nothing} = pure ()
  pretty' ImplicitParameters {..} =
    lined
      $ fmap
          (\binding ->
             prettyWith binding $ \(name, expr) ->
               spaced [pretty name, string "=", pretty expr])
          implicitBindings

mkLocalBinds :: GHC.HsLocalBinds GHC.GhcPs -> WithComments LocalBinds
mkLocalBinds (GHC.HsValBinds ann binds) =
  fromEpAnn ann $ Value {sigBindFamilies = Just $ mkSigBindFamilies binds}
mkLocalBinds (GHC.HsIPBinds ann binds) =
  fromEpAnn ann
    $ ImplicitParameters {implicitBindings = mkImplicitBindings binds}
mkLocalBinds GHC.EmptyLocalBinds {} =
  mkWithComments $ Value {sigBindFamilies = Nothing}

hasLocalBinds :: LocalBinds -> Bool
hasLocalBinds Value {sigBindFamilies = Just families} = not $ null families
hasLocalBinds Value {sigBindFamilies = Nothing} = False
hasLocalBinds ImplicitParameters {..} = not $ null implicitBindings

valueSigBindFamilies :: LocalBinds -> Maybe [WithComments SBF.SigBindFamily]
valueSigBindFamilies Value {..} = sigBindFamilies
valueSigBindFamilies _ = Nothing

implicitParameterBindings ::
     LocalBinds
  -> [WithComments (WithComments ImplicitParameterName, WithComments Expression)]
implicitParameterBindings ImplicitParameters {..} = implicitBindings
implicitParameterBindings _ = []

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

mkImplicitBindings ::
     GHC.HsIPBinds GHC.GhcPs
  -> [WithComments (WithComments ImplicitParameterName, WithComments Expression)]
mkImplicitBindings (GHC.IPBinds _ binds) =
  fmap (fmap mkImplicitParameterBinding . fromGenLocated) binds

mkImplicitParameterBinding ::
     GHC.IPBind GHC.GhcPs
  -> (WithComments ImplicitParameterName, WithComments Expression)
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkImplicitParameterBinding (GHC.IPBind _ lhs rhs) =
  ( mkImplicitParameterName <$> fromGenLocated lhs
  , mkExpression <$> fromGenLocated rhs)
#else
mkImplicitParameterBinding (GHC.IPBind _ (Left lhs) rhs) =
  ( mkImplicitParameterName <$> fromGenLocated lhs
  , mkExpression <$> fromGenLocated rhs)
mkImplicitParameterBinding (GHC.IPBind _ (Right _) _) =
  error "`ghc-lib-parser` never generates this AST node."
#endif
newtype ValueBindGroup = ValueBindGroup
  { valueFamilies :: [WithComments SBF.SigBindFamily]
  }

instance CommentExtraction ValueBindGroup where
  nodeComments ValueBindGroup {} = NodeComments [] [] []

instance Pretty ValueBindGroup where
  pretty' ValueBindGroup {..} = lined $ fmap pretty valueFamilies

mkValueBindGroup ::
     GHC.HsLocalBinds GHC.GhcPs -> WithComments (Maybe ValueBindGroup)
mkValueBindGroup (GHC.HsValBinds ann binds) =
  Just . ValueBindGroup <$> fromEpAnn ann (mkSigBindFamilies binds)
mkValueBindGroup GHC.EmptyLocalBinds {} = mkWithComments Nothing
mkValueBindGroup GHC.HsIPBinds {} = mkWithComments Nothing

hasValueBindGroup :: ValueBindGroup -> Bool
hasValueBindGroup ValueBindGroup {..} = not $ null valueFamilies
