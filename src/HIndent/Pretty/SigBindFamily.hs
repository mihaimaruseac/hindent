{-# LANGUAGE LambdaCase #-}

-- | A module defining 'SigBindFamily' and related helpers for mixing
-- signatures, bindings, and family declarations/instances in source
-- order.
module HIndent.Pretty.SigBindFamily
  ( SigBindFamily(..)
  , LSigBindFamily
  , mkSortedLSigBindFamilyList
  , mkLSigBindFamilyList
  , destructLSigBindFamilyList
  , filterLSig
  , filterLBind
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import GHC.Hs
import GHC.Types.SrcLoc
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Pretty.NodeComments (CommentExtraction(..))

-- | A sum type containing one of those: function signature, binding,
-- family declaration (type or data), type family instance, type family
-- default, or data family instance.
data SigBindFamily
  = Sig (Sig GhcPs)
  | Bind (HsBindLR GhcPs GhcPs)
  | Family (FamilyDecl GhcPs)
  | TyFamInst (TyFamInstDecl GhcPs)
  | TyFamDeflt (TyFamDefltDecl GhcPs)
  | DataFamInst (DataFamInstDecl GhcPs)

-- | 'SigBindFamily' with location.
type LSigBindFamily = GenLocated SrcSpanAnnA SigBindFamily

instance CommentExtraction SigBindFamily where
  nodeComments _ = NodeComments [] [] []

-- | Creates a sorted list of 'LSigBindFamily' from separate buckets.
mkSortedLSigBindFamilyList ::
     [LSig GhcPs]
  -> [LHsBindLR GhcPs GhcPs]
  -> [LFamilyDecl GhcPs]
  -> [LTyFamInstDecl GhcPs]
  -> [LTyFamDefltDecl GhcPs]
  -> [LDataFamInstDecl GhcPs]
  -> [LSigBindFamily]
mkSortedLSigBindFamilyList sigs binds fams insts deflts datafams =
  sortBy (compare `on` realSrcSpan . locA . getLoc)
    $ mkLSigBindFamilyList sigs binds fams insts deflts datafams

-- | Creates a list of 'LSigBindFamily' from separate buckets.
mkLSigBindFamilyList ::
     [LSig GhcPs]
  -> [LHsBindLR GhcPs GhcPs]
  -> [LFamilyDecl GhcPs]
  -> [LTyFamInstDecl GhcPs]
  -> [LTyFamDefltDecl GhcPs]
  -> [LDataFamInstDecl GhcPs]
  -> [LSigBindFamily]
mkLSigBindFamilyList sigs binds fams insts deflts datafams =
  fmap (fmap Sig) sigs
    ++ fmap (fmap Bind) binds
    ++ fmap (fmap Family) fams
    ++ fmap (fmap TyFamInst) insts
    ++ fmap (fmap TyFamDeflt) deflts
    ++ fmap (fmap DataFamInst) datafams

-- | Split a list of 'LSigBindFamily' back into the original buckets.
destructLSigBindFamilyList ::
     [LSigBindFamily]
  -> ( [LSig GhcPs]
     , [LHsBindLR GhcPs GhcPs]
     , [LFamilyDecl GhcPs]
     , [LTyFamInstDecl GhcPs]
     , [LTyFamDefltDecl GhcPs]
     , [LDataFamInstDecl GhcPs])
destructLSigBindFamilyList =
  (,,,,,)
    <$> filterLSig
    <*> filterLBind
    <*> filterLFamily
    <*> filterLTyFamInst
    <*> filterLTyFamDeflt
    <*> filterLDataFamInst

-- | Filters out 'Sig's and extracts the wrapped values.
filterLSig :: [LSigBindFamily] -> [LSig GhcPs]
filterLSig =
  mapMaybe
    (\case
       (L l (Sig x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'Bind's and extracts the wrapped values.
filterLBind :: [LSigBindFamily] -> [LHsBindLR GhcPs GhcPs]
filterLBind =
  mapMaybe
    (\case
       (L l (Bind x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'Family's and extracts the wrapped values.
filterLFamily :: [LSigBindFamily] -> [LFamilyDecl GhcPs]
filterLFamily =
  mapMaybe
    (\case
       (L l (Family x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'TyFamInst's and extracts the wrapped values.
filterLTyFamInst :: [LSigBindFamily] -> [LTyFamInstDecl GhcPs]
filterLTyFamInst =
  mapMaybe
    (\case
       (L l (TyFamInst x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'TyFamDeflt's and extracts the wrapped values.
filterLTyFamDeflt :: [LSigBindFamily] -> [LTyFamDefltDecl GhcPs]
filterLTyFamDeflt =
  mapMaybe
    (\case
       (L l (TyFamDeflt x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'DataFamInst's and extracts the wrapped values.
filterLDataFamInst :: [LSigBindFamily] -> [LDataFamInstDecl GhcPs]
filterLDataFamInst =
  mapMaybe
    (\case
       (L l (DataFamInst x)) -> Just $ L l x
       _ -> Nothing)
