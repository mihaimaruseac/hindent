{-# LANGUAGE LambdaCase #-}

-- | A module defining 'SigBindFamily' and other related types and
-- functions.
module HIndent.Pretty.SigBindFamily
  ( SigBindFamily(..)
  , LSigBindFamily
  , mkSortedLSigBindFamilyList
  , mkLSigBindFamilyList
  , filterLSig
  , filterLBind
  ) where

import           Data.Function
import           Data.List
import           Data.Maybe
import           GHC.Hs
import           GHC.Types.SrcLoc

-- | A sum type containing one of those: 'Sig', 'HsBindLR', and
-- 'FamilyDecl'.
data SigBindFamily
  = Sig (Sig GhcPs)
  | Bind (HsBindLR GhcPs GhcPs)
  | TypeFamily (FamilyDecl GhcPs)
  | TyFamInst (TyFamInstDecl GhcPs)

-- | 'SigBindFamily' with the location information.
type LSigBindFamily = GenLocated SrcSpanAnnA SigBindFamily

-- | Creates a list of 'LSigBindFamily' from arguments. The list is sorted
-- by its elements' locations.
mkSortedLSigBindFamilyList ::
     [LSig GhcPs]
  -> [LHsBindLR GhcPs GhcPs]
  -> [LFamilyDecl GhcPs]
  -> [LTyFamInstDecl GhcPs]
  -> [LSigBindFamily]
mkSortedLSigBindFamilyList sigs binds fams =
  sortBy (compare `on` realSrcSpan . locA . getLoc) .
  mkLSigBindFamilyList sigs binds fams

-- | Creates a list of 'LSigBindFamily' from arguments.
mkLSigBindFamilyList ::
     [LSig GhcPs]
  -> [LHsBindLR GhcPs GhcPs]
  -> [LFamilyDecl GhcPs]
  -> [LTyFamInstDecl GhcPs]
  -> [LSigBindFamily]
mkLSigBindFamilyList sigs binds fams insts =
  fmap (fmap Sig) sigs ++
  fmap (fmap Bind) binds ++
  fmap (fmap TypeFamily) fams ++ fmap (fmap TyFamInst) insts

-- | Filters out 'Sig's and extract the wrapped values.
filterLSig :: [LSigBindFamily] -> [LSig GhcPs]
filterLSig =
  mapMaybe
    (\case
       (L l (Sig x)) -> Just $ L l x
       _             -> Nothing)

-- | Filters out 'Bind's and extract the wrapped values.
filterLBind :: [LSigBindFamily] -> [LHsBindLR GhcPs GhcPs]
filterLBind =
  mapMaybe
    (\case
       (L l (Bind x)) -> Just $ L l x
       _              -> Nothing)
