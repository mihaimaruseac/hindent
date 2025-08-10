{-# LANGUAGE CPP #-}

module HIndent.Ast.Type.Multiplicity
  ( Multiplicity
  , mkMultiplicity
  , isUnrestricted
  ) where

import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Multiplicity
  = MkUnrestricted
  | MkLinear
  | MkExplicit (WithComments Type)

instance CommentExtraction Multiplicity where
  nodeComments MkUnrestricted = NodeComments [] [] []
  nodeComments MkLinear = NodeComments [] [] []
  nodeComments (MkExplicit mult) = nodeComments mult

instance Pretty Multiplicity where
  pretty' MkUnrestricted = pure ()
  pretty' MkLinear = string "%1"
  pretty' (MkExplicit mult) = string "%" >> pretty mult

mkMultiplicity :: GHC.HsArrow GHC.GhcPs -> Multiplicity
mkMultiplicity (GHC.HsUnrestrictedArrow _) = MkUnrestricted
mkMultiplicity (GHC.HsLinearArrow _) = MkLinear
#if MIN_VERSION_ghc_lib_parser(9, 10, 0)
mkMultiplicity (GHC.HsExplicitMult _ mult) =
  MkExplicit (mkType <$> fromGenLocated mult)
#else
mkMultiplicity (GHC.HsExplicitMult _ mult _) =
  MkExplicit (mkType <$> fromGenLocated mult)
#endif
isUnrestricted :: Multiplicity -> Bool
isUnrestricted MkUnrestricted = True
isUnrestricted _ = False
