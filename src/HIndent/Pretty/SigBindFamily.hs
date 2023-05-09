{-# LANGUAGE LambdaCase #-}

-- | A module defining 'SigBindFamily' and other related types and
-- functions.
module HIndent.Pretty.SigBindFamily
  ( SigBindFamily(..)
  , LSigBindFamily
  , mkSortedLSigBindFamilyList
  , mkLSigBindFamilyList
  , destructLSigBindFamilyList
  , filterLSig
  , filterLBind
  ) where

import Data.Function
import Data.List
import Data.Maybe
import GHC.Hs
import GHC.Types.SrcLoc

-- | A sum type containing one of those: function signature, function
-- binding, type family, type family instance, and data family instance.
data SigBindFamily
  = Sig (Sig GhcPs)
  | Bind (HsBindLR GhcPs GhcPs)
  | TypeFamily (FamilyDecl GhcPs)
  | TyFamInst (TyFamInstDecl GhcPs)
  | DataFamInst (DataFamInstDecl GhcPs)

-- | 'SigBindFamily' with the location information.
type LSigBindFamily = GenLocated SrcSpanAnnA SigBindFamily

-- | Creates a list of 'LSigBindFamily' from arguments. The list is sorted
-- by its elements' locations.
mkSortedLSigBindFamilyList ::
     [LSig GhcPs]
  -> [LHsBindLR GhcPs GhcPs]
  -> [LFamilyDecl GhcPs]
  -> [LTyFamInstDecl GhcPs]
  -> [LDataFamInstDecl GhcPs]
  -> [LSigBindFamily]
mkSortedLSigBindFamilyList sigs binds fams datafams =
  sortBy (compare `on` realSrcSpan . locA . getLoc)
    . mkLSigBindFamilyList sigs binds fams datafams

-- | Creates a list of 'LSigBindFamily' from arguments.
mkLSigBindFamilyList ::
     [LSig GhcPs]
  -> [LHsBindLR GhcPs GhcPs]
  -> [LFamilyDecl GhcPs]
  -> [LTyFamInstDecl GhcPs]
  -> [LDataFamInstDecl GhcPs]
  -> [LSigBindFamily]
mkLSigBindFamilyList sigs binds fams insts datafams =
  fmap (fmap Sig) sigs
    ++ fmap (fmap Bind) binds
    ++ fmap (fmap TypeFamily) fams
    ++ fmap (fmap TyFamInst) insts
    ++ fmap (fmap DataFamInst) datafams

-- | Destructs a list of 'LSigBindFamily'
destructLSigBindFamilyList ::
     [LSigBindFamily]
  -> ( [LSig GhcPs]
     , [LHsBindLR GhcPs GhcPs]
     , [LFamilyDecl GhcPs]
     , [LTyFamInstDecl GhcPs]
     , [LDataFamInstDecl GhcPs])
destructLSigBindFamilyList =
  (,,,,)
    <$> filterLSig
    <*> filterLBind
    <*> filterLTypeFamily
    <*> filterLTyFamInst
    <*> filterLDataFamInst

-- | Filters out 'Sig's and extract the wrapped values.
filterLSig :: [LSigBindFamily] -> [LSig GhcPs]
filterLSig =
  mapMaybe
    (\case
       (L l (Sig x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'Bind's and extract the wrapped values.
filterLBind :: [LSigBindFamily] -> [LHsBindLR GhcPs GhcPs]
filterLBind =
  mapMaybe
    (\case
       (L l (Bind x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'TypeFamily's and extract the wrapped values.
filterLTypeFamily :: [LSigBindFamily] -> [LFamilyDecl GhcPs]
filterLTypeFamily =
  mapMaybe
    (\case
       (L l (TypeFamily x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'TyFamInst's and extract the wrapped values.
filterLTyFamInst :: [LSigBindFamily] -> [LTyFamInstDecl GhcPs]
filterLTyFamInst =
  mapMaybe
    (\case
       (L l (TyFamInst x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'DataFamInst's and extract the wrapped values.
filterLDataFamInst :: [LSigBindFamily] -> [LDataFamInstDecl GhcPs]
filterLDataFamInst =
  mapMaybe
    (\case
       (L l (DataFamInst x)) -> Just $ L l x
       _ -> Nothing)
