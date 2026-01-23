module HIndent.Ast.GhcOrdered.BindGroupElement
  ( BindGroupElement
  , mkBindGroupSignature
  , mkBindGroupBind
  , foldBindGroupElement
  ) where

import HIndent.Ast.NodeComments (NodeComments(..))
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments (CommentExtraction(..))

-- | Elements that can appear together inside a binding group such as
-- @let@/@where@ blocks.
data BindGroupElement
  = BindGroupSignature (GHC.Sig GHC.GhcPs)
  | BindGroupBind (GHC.HsBindLR GHC.GhcPs GHC.GhcPs)

instance CommentExtraction BindGroupElement where
  nodeComments _ = NodeComments [] [] []

mkBindGroupSignature :: GHC.Sig GHC.GhcPs -> BindGroupElement
mkBindGroupSignature = BindGroupSignature

mkBindGroupBind :: GHC.HsBindLR GHC.GhcPs GHC.GhcPs -> BindGroupElement
mkBindGroupBind = BindGroupBind

foldBindGroupElement ::
     (GHC.Sig GHC.GhcPs -> r)
  -> (GHC.HsBindLR GHC.GhcPs GHC.GhcPs -> r)
  -> BindGroupElement
  -> r
foldBindGroupElement onSig _ (BindGroupSignature sig) = onSig sig
foldBindGroupElement _ onBind (BindGroupBind bind) = onBind bind
