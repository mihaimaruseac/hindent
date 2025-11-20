module HIndent.GhcOrdered.ClassElement
  ( ClassElement
  , LClassElement
  , foldClassElement
  , mkSortedClassElements
  , destructClassElements
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import qualified GHC.Types.SrcLoc as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC

-- | Elements that may appear inside a class declaration.
data ClassElement
  = ClassElementSignature (GHC.Sig GHC.GhcPs)
  | ClassElementBind (GHC.HsBindLR GHC.GhcPs GHC.GhcPs)
  | ClassElementFamily (GHC.FamilyDecl GHC.GhcPs)
  | ClassElementTypeDefault (GHC.TyFamInstDecl GHC.GhcPs)

type LClassElement = GHC.LocatedA ClassElement

foldClassElement ::
     (GHC.Sig GHC.GhcPs -> r)
  -> (GHC.HsBindLR GHC.GhcPs GHC.GhcPs -> r)
  -> (GHC.FamilyDecl GHC.GhcPs -> r)
  -> (GHC.TyFamInstDecl GHC.GhcPs -> r)
  -> ClassElement
  -> r
foldClassElement onSig _ _ _ (ClassElementSignature sig) = onSig sig
foldClassElement _ onBind _ _ (ClassElementBind bind) = onBind bind
foldClassElement _ _ onFamily _ (ClassElementFamily fam) = onFamily fam
foldClassElement _ _ _ onTy (ClassElementTypeDefault inst) = onTy inst

mkSortedClassElements ::
     [GHC.LSig GHC.GhcPs]
  -> [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
  -> [GHC.LFamilyDecl GHC.GhcPs]
  -> [GHC.LTyFamInstDecl GHC.GhcPs]
  -> [LClassElement]
mkSortedClassElements sigs binds fams tyInsts =
  sortBy
    (compare `on` GHC.realSrcSpan . GHC.locA . GHC.getLoc)
    (fmap (fmap ClassElementSignature) sigs
       ++ fmap (fmap ClassElementBind) binds
       ++ fmap (fmap ClassElementFamily) fams
       ++ fmap (fmap ClassElementTypeDefault) tyInsts)

destructClassElements ::
     [LClassElement]
  -> ( [GHC.LSig GHC.GhcPs]
     , [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
     , [GHC.LFamilyDecl GHC.GhcPs]
     , [GHC.LTyFamInstDecl GHC.GhcPs])
destructClassElements xs =
  ( mapMaybe onlySig xs
  , mapMaybe onlyBind xs
  , mapMaybe onlyFamily xs
  , mapMaybe onlyTyInst xs)
  where
    onlySig (GHC.L l (ClassElementSignature sig)) = Just (GHC.L l sig)
    onlySig _ = Nothing
    onlyBind (GHC.L l (ClassElementBind bind)) = Just (GHC.L l bind)
    onlyBind _ = Nothing
    onlyFamily (GHC.L l (ClassElementFamily fam)) = Just (GHC.L l fam)
    onlyFamily _ = Nothing
    onlyTyInst (GHC.L l (ClassElementTypeDefault inst)) = Just (GHC.L l inst)
    onlyTyInst _ = Nothing
