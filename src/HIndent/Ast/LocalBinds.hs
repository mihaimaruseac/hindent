{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.LocalBinds
  ( LocalBinds(..)
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

data LocalBinds
  = ValueLocalBinds
      { sigBindFamilies :: Maybe [WithComments SBF.SigBindFamily]
      }
  | ImplicitParameterLocalBinds
      { implicitBindings :: [WithComments
                               ( WithComments ImplicitParameterName
                               , WithComments Expression)]
      }

instance CommentExtraction LocalBinds where
  nodeComments ValueLocalBinds {} = NodeComments [] [] []
  nodeComments ImplicitParameterLocalBinds {} = NodeComments [] [] []

instance Pretty LocalBinds where
  pretty' ValueLocalBinds {sigBindFamilies = Just families} =
    lined $ fmap pretty families
  pretty' ValueLocalBinds {sigBindFamilies = Nothing} = pure ()
  pretty' ImplicitParameterLocalBinds {..} =
    lined
      $ fmap
          (\binding ->
             prettyWith binding $ \(name, expr) ->
               spaced [pretty name, string "=", pretty expr])
          implicitBindings

mkLocalBindsWithComments ::
     GHC.HsLocalBinds GHC.GhcPs -> WithComments LocalBinds
mkLocalBindsWithComments (GHC.HsValBinds ann binds) =
  fromEpAnn ann
    $ ValueLocalBinds {sigBindFamilies = Just $ mkSigBindFamilies binds}
mkLocalBindsWithComments (GHC.HsIPBinds ann binds) =
  fromEpAnn ann
    $ ImplicitParameterLocalBinds {implicitBindings = mkImplicitBindings binds}
mkLocalBindsWithComments GHC.EmptyLocalBinds {} =
  mkWithComments $ ValueLocalBinds {sigBindFamilies = Nothing}
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkLocalBindsFromLocated ::
     GHC.XRec GHC.GhcPs (GHC.HsLocalBinds GHC.GhcPs) -> WithComments LocalBinds
mkLocalBindsFromLocated = mkLocalBindsWithComments . unLoc
#else
mkLocalBindsFromLocated :: GHC.HsLocalBinds GHC.GhcPs -> WithComments LocalBinds
mkLocalBindsFromLocated = mkLocalBindsWithComments
#endif
hasLocalBinds :: LocalBinds -> Bool
hasLocalBinds ValueLocalBinds {sigBindFamilies = Just families} =
  not $ null families
hasLocalBinds ValueLocalBinds {sigBindFamilies = Nothing} = False
hasLocalBinds ImplicitParameterLocalBinds {..} = not $ null implicitBindings

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
