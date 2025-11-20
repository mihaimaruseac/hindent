module HIndent.GhcOrdered.BindGroupElement
  ( BindGroupElement
  , LBindGroupElement
  , foldBindGroupElement
  , mkSortedBindGroupElements
  , destructBindGroupElements
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import qualified GHC.Types.SrcLoc as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC

-- | Elements that can appear together inside a binding group such as
-- @let@/@where@ blocks.
data BindGroupElement
  = BindGroupSignature (GHC.Sig GHC.GhcPs)
  | BindGroupBind (GHC.HsBindLR GHC.GhcPs GHC.GhcPs)

type LBindGroupElement = GHC.LocatedA BindGroupElement

foldBindGroupElement ::
     (GHC.Sig GHC.GhcPs -> r)
  -> (GHC.HsBindLR GHC.GhcPs GHC.GhcPs -> r)
  -> BindGroupElement
  -> r
foldBindGroupElement onSig _ (BindGroupSignature sig) = onSig sig
foldBindGroupElement _ onBind (BindGroupBind bind) = onBind bind

-- | Merge and sort signatures and binds by their source locations.
mkSortedBindGroupElements ::
     [GHC.LSig GHC.GhcPs]
  -> [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
  -> [LBindGroupElement]
mkSortedBindGroupElements sigs binds =
  sortBy
    (compare `on` GHC.realSrcSpan . GHC.locA . GHC.getLoc)
    (fmap (fmap BindGroupSignature) sigs ++ fmap (fmap BindGroupBind) binds)

-- | Split sorted elements back into their original buckets.
destructBindGroupElements ::
     [LBindGroupElement]
  -> ([GHC.LSig GHC.GhcPs], [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs])
destructBindGroupElements xs = (mapMaybe toSig xs, mapMaybe toBind xs)
  where
    toSig (GHC.L l (BindGroupSignature sig)) = Just (GHC.L l sig)
    toSig _ = Nothing
    toBind (GHC.L l (BindGroupBind bind)) = Just (GHC.L l bind)
    toBind _ = Nothing
