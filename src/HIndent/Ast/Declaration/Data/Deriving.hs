{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Deriving
  ( Deriving
  , mkDeriving
  ) where

import qualified GHC.Types.SrcLoc as GHC
import HIndent.Applicative
import HIndent.Ast.Declaration.Data.Deriving.Strategy
import HIndent.Ast.NodeComments
import HIndent.Ast.Type (Type, mkTypeFromHsSigType)
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Deriving = Deriving
  { strategy :: Maybe (WithComments DerivingStrategy)
  , classes :: WithComments [WithComments Type]
  }

instance CommentExtraction Deriving where
  nodeComments Deriving {} = NodeComments [] [] []

instance Pretty Deriving where
  pretty' Deriving {strategy = Just strategy, ..}
    | isViaStrategy (getNode strategy) = do
      spaced
        [ string "deriving"
        , prettyWith classes (hvTuple . fmap pretty)
        , pretty strategy
        ]
  pretty' Deriving {..} = do
    string "deriving "
    whenJust strategy $ \x -> pretty x >> space
    prettyWith classes (hvTuple . fmap pretty)

mkDeriving :: GHC.HsDerivingClause GHC.GhcPs -> Deriving
mkDeriving GHC.HsDerivingClause {..} = Deriving {..}
  where
    strategy =
      fmap (fmap mkDerivingStrategy . fromGenLocated) deriv_clause_strategy
    classes = fromGenLocated $ fmap (const rawClasses) deriv_clause_tys
    rawClasses =
      case GHC.unLoc deriv_clause_tys of
        GHC.DctSingle _ ty ->
          [flattenComments $ mkTypeFromHsSigType <$> fromGenLocated ty]
        GHC.DctMulti _ tys ->
          flattenComments . fmap mkTypeFromHsSigType . fromGenLocated <$> tys
