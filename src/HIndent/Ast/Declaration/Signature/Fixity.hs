{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Signature.Fixity
  ( Fixity
  , mkFixity
  ) where

import qualified Data.Text as Text
import qualified GHC.Types.Fixity as GHC
import HIndent.Ast.Declaration.Signature.Fixity.Associativity
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data Fixity = Fixity
  { level :: Int
  , associativity :: Associativity
  }

instance Pretty Fixity where
  pretty Fixity {..} =
    spaced [pretty associativity, string $ Text.pack $ show level]

mkFixity :: GHC.Fixity -> Fixity
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkFixity (GHC.Fixity level associativity) =
  Fixity level (mkAssociativity associativity)
#else
mkFixity (GHC.Fixity _ level associativity) =
  Fixity level (mkAssociativity associativity)
#endif
