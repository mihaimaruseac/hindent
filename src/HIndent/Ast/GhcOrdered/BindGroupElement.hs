module HIndent.Ast.GhcOrdered.BindGroupElement
  ( BindGroupElement
  , BindGroupElements(..)
  , foldBindGroupElement
  , mkSortedBindGroupElements
  , destructBindGroupElements
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.NodeComments (NodeComments(..))
import {-# SOURCE #-} HIndent.Ast.WithComments
  ( WithComments
  , addComments
  , fromGenLocated
  , getComments
  , getNode
  , mkWithComments
  )
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments (CommentExtraction(..))

-- | Elements that can appear together inside a binding group such as
-- @let@/@where@ blocks.
data BindGroupElement
  = BindGroupSignature (GHC.Sig GHC.GhcPs)
  | BindGroupBind (GHC.HsBindLR GHC.GhcPs GHC.GhcPs)

newtype BindGroupElements = BindGroupElements
  { elements :: [WithComments BindGroupElement]
  }

instance CommentExtraction BindGroupElement where
  nodeComments _ = NodeComments [] [] []

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
  -> BindGroupElements
mkSortedBindGroupElements sigs binds =
  BindGroupElements
    $ fromGenLocated
        <$> sortBy
              (compare `on` GHC.realSrcSpan . GHC.locA . GHC.getLoc)
              (fmap (fmap BindGroupSignature) sigs
                 ++ fmap (fmap BindGroupBind) binds)

-- | Split sorted elements back into their original buckets.
destructBindGroupElements ::
     BindGroupElements
  -> ( [WithComments (GHC.Sig GHC.GhcPs)]
     , [WithComments (GHC.HsBindLR GHC.GhcPs GHC.GhcPs)])
destructBindGroupElements (BindGroupElements xs) =
  (mapMaybe toSig xs, mapMaybe toBind xs)
  where
    toSig x =
      case getNode x of
        BindGroupSignature sig ->
          Just $ addComments (getComments x) (mkWithComments sig)
        _ -> Nothing
    toBind x =
      case getNode x of
        BindGroupBind bind ->
          Just $ addComments (getComments x) (mkWithComments bind)
        _ -> Nothing
