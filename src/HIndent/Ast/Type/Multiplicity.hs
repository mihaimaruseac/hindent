{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module HIndent.Ast.Type.Multiplicity
  ( Multiplicity
  , mkMultiplicity
  , isUnrestricted
  ) where

import {-# SOURCE #-} HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data Multiplicity
  = MkUnrestricted
  | MkLinear
  | MkExplicit (WithComments Type)

instance Pretty Multiplicity where
  pretty MkUnrestricted = pure ()
  pretty MkLinear = string "%1"
  pretty (MkExplicit mult) = string "%" >> pretty mult

#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkMultiplicity :: GHC.HsMultAnn GHC.GhcPs -> Multiplicity
mkMultiplicity GHC.HsUnannotated {} = MkUnrestricted
mkMultiplicity GHC.HsLinearAnn {} = MkLinear
mkMultiplicity (GHC.HsExplicitMult _ mult) =
  MkExplicit (mkType <$> mkWithCommentsFromGenLocated mult)
#else
mkMultiplicity :: GHC.HsArrow GHC.GhcPs -> Multiplicity
mkMultiplicity (GHC.HsUnrestrictedArrow _) = MkUnrestricted
mkMultiplicity (GHC.HsLinearArrow _) = MkLinear

#if MIN_VERSION_ghc_lib_parser(9, 10, 0)
mkMultiplicity (GHC.HsExplicitMult _ mult) =
  MkExplicit (mkType <$> mkWithCommentsFromGenLocated mult)
#else
mkMultiplicity (GHC.HsExplicitMult _ mult _) =
  MkExplicit (mkType <$> mkWithCommentsFromGenLocated mult)
#endif
#endif

isUnrestricted :: Multiplicity -> Bool
isUnrestricted MkUnrestricted = True
isUnrestricted _ = False
