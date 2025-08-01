{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.StandAloneDeriving
  ( StandAloneDeriving
  , mkStandAloneDeriving
  ) where

import HIndent.Applicative
import HIndent.Ast.Declaration.Data.Deriving.Strategy
import HIndent.Ast.NodeComments
import HIndent.Ast.Type (Type, mkTypeFromHsSigType)
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data StandAloneDeriving = StandAloneDeriving
  { strategy :: Maybe (WithComments DerivingStrategy)
  , className :: WithComments Type
  }

instance CommentExtraction StandAloneDeriving where
  nodeComments StandAloneDeriving {} = NodeComments [] [] []

instance Pretty StandAloneDeriving where
  pretty' StandAloneDeriving {strategy = Just strategy, ..}
    | isViaStrategy (getNode strategy) =
      spaced
        [ string "deriving"
        , pretty strategy
        , string "instance"
        , pretty className
        ]
  pretty' StandAloneDeriving {..} = do
    string "deriving "
    whenJust strategy $ \x -> pretty x >> space
    string "instance "
    pretty className

mkStandAloneDeriving :: GHC.DerivDecl GHC.GhcPs -> StandAloneDeriving
mkStandAloneDeriving GHC.DerivDecl {deriv_type = GHC.HsWC {..}, ..} =
  StandAloneDeriving {..}
  where
    strategy = fmap (fmap mkDerivingStrategy . fromGenLocated) deriv_strategy
    className =
      flattenComments $ mkTypeFromHsSigType <$> fromGenLocated hswc_body
