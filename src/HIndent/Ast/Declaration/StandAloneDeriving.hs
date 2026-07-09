{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.StandAloneDeriving
  ( StandAloneDeriving
  , mkStandAloneDeriving
  ) where

import HIndent.Applicative
import HIndent.Ast.Declaration.Data.Deriving.Strategy
import HIndent.Ast.Type (Type, mkTypeFromHsSigType)
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data StandAloneDeriving = StandAloneDeriving
  { strategy :: Maybe (WithComments DerivingStrategy)
  , className :: WithComments Type
  }

instance Pretty StandAloneDeriving where
  pretty StandAloneDeriving {strategy = Just strategy, ..}
    | isViaStrategy (getNode strategy) =
      spaced
        [ string "deriving"
        , pretty strategy
        , string "instance"
        , pretty className
        ]
  pretty StandAloneDeriving {..} = do
    string "deriving "
    whenJust strategy $ \x -> pretty x >> space
    string "instance "
    pretty className

mkStandAloneDeriving :: GHC.DerivDecl GHC.GhcPs -> StandAloneDeriving
mkStandAloneDeriving GHC.DerivDecl {deriv_type = GHC.HsWC {..}, ..} =
  StandAloneDeriving {..}
  where
    strategy =
      fmap
        (fmap mkDerivingStrategy . mkWithCommentsFromGenLocated)
        deriv_strategy
    className =
      flattenComments
        $ mkTypeFromHsSigType <$> mkWithCommentsFromGenLocated hswc_body
