module HIndent.GhcOrdered.InstanceMember
  ( InstanceMember
  , LInstanceMember
  , foldInstanceMember
  , mkSortedInstanceMembers
  ) where

import Data.Function (on)
import Data.List (sortBy)
import qualified GHC.Types.SrcLoc as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC

-- | Elements allowed inside a class instance declaration.
data InstanceMember
  = InstanceMemberSignature (GHC.Sig GHC.GhcPs)
  | InstanceMemberBind (GHC.HsBindLR GHC.GhcPs GHC.GhcPs)
  | InstanceMemberTypeFamily (GHC.TyFamInstDecl GHC.GhcPs)
  | InstanceMemberDataFamily (GHC.DataFamInstDecl GHC.GhcPs)

type LInstanceMember = GHC.LocatedA InstanceMember

foldInstanceMember ::
     (GHC.Sig GHC.GhcPs -> r)
  -> (GHC.HsBindLR GHC.GhcPs GHC.GhcPs -> r)
  -> (GHC.TyFamInstDecl GHC.GhcPs -> r)
  -> (GHC.DataFamInstDecl GHC.GhcPs -> r)
  -> InstanceMember
  -> r
foldInstanceMember onSig _ _ _ (InstanceMemberSignature sig) = onSig sig
foldInstanceMember _ onBind _ _ (InstanceMemberBind bind) = onBind bind
foldInstanceMember _ _ onTy _ (InstanceMemberTypeFamily inst) = onTy inst
foldInstanceMember _ _ _ onData (InstanceMemberDataFamily inst) = onData inst

mkSortedInstanceMembers ::
     [GHC.LSig GHC.GhcPs]
  -> [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
  -> [GHC.LTyFamInstDecl GHC.GhcPs]
  -> [GHC.LDataFamInstDecl GHC.GhcPs]
  -> [LInstanceMember]
mkSortedInstanceMembers sigs binds tyInsts dataInsts =
  sortBy
    (compare `on` GHC.realSrcSpan . GHC.locA . GHC.getLoc)
    (fmap (fmap InstanceMemberSignature) sigs
       ++ fmap (fmap InstanceMemberBind) binds
       ++ fmap (fmap InstanceMemberTypeFamily) tyInsts
       ++ fmap (fmap InstanceMemberDataFamily) dataInsts)
