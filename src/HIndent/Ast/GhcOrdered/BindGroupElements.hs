module HIndent.Ast.GhcOrdered.BindGroupElements
  ( BindGroupElements
  , mkBindGroupElements
  , bindGroupElements
  , mkSortedBindGroupElements
  , destructBindGroupElements
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.GhcOrdered.BindGroupElement
  ( BindGroupElement
  , foldBindGroupElement
  , mkBindGroupBind
  , mkBindGroupSignature
  )
import HIndent.Ast.WithComments
  ( WithComments
  , addComments
  , fromGenLocated
  , getComments
  , getNode
  , mkWithComments
  )
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC

newtype BindGroupElements =
  BindGroupElements [WithComments BindGroupElement]

mkBindGroupElements :: [WithComments BindGroupElement] -> BindGroupElements
mkBindGroupElements = BindGroupElements

bindGroupElements :: BindGroupElements -> [WithComments BindGroupElement]
bindGroupElements (BindGroupElements xs) = xs

-- | Merge and sort signatures and binds by their source locations.
mkSortedBindGroupElements ::
     [GHC.LSig GHC.GhcPs]
  -> [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
  -> BindGroupElements
mkSortedBindGroupElements sigs binds =
  BindGroupElements
    $ fromGenLocated
        <$> sortBy
              (compare `on` GHC.realSrcSpan . GHC.locA . GHC.getLoc)
              (fmap (fmap mkBindGroupSignature) sigs
                 ++ fmap (fmap mkBindGroupBind) binds)

-- | Split sorted elements back into their original buckets.
destructBindGroupElements ::
     BindGroupElements
  -> ( [WithComments (GHC.Sig GHC.GhcPs)]
     , [WithComments (GHC.HsBindLR GHC.GhcPs GHC.GhcPs)])
destructBindGroupElements (BindGroupElements xs) =
  (mapMaybe toSig xs, mapMaybe toBind xs)
  where
    toSig x =
      foldBindGroupElement
        (Just . addComments (getComments x) . mkWithComments)
        (const Nothing)
        (getNode x)
    toBind x =
      foldBindGroupElement
        (const Nothing)
        (Just . addComments (getComments x) . mkWithComments)
        (getNode x)
