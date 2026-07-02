module HIndent.Ast.Declaration.Instance.Class.Body
  ( ClassInstanceBody
  , hasClassInstanceBody
  , mkClassInstanceBody
  ) where

import Data.Function
import Data.List (sortBy)
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Declaration.Instance.Class.Member
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype ClassInstanceBody =
  ClassInstanceBody [WithComments ClassInstanceMember]

instance CommentExtraction ClassInstanceBody where
  nodeComments ClassInstanceBody {} = NodeComments [] [] []

instance Pretty ClassInstanceBody where
  pretty (ClassInstanceBody members) = lined $ fmap pretty members

mkClassInstanceBody ::
     [GHC.LSig GHC.GhcPs]
  -> [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
  -> [GHC.LTyFamInstDecl GHC.GhcPs]
  -> [GHC.LDataFamInstDecl GHC.GhcPs]
  -> ClassInstanceBody
mkClassInstanceBody sigs binds typeInstances dataInstances =
  ClassInstanceBody
    $ sortMembers
    $ fmap (fmap mkClassInstanceSignatureMember) sigs
        ++ fmap (fmap mkClassInstanceMethodMember) binds
        ++ fmap (fmap mkAssociatedTypeInstanceMember) typeInstances
        ++ fmap (fmap mkAssociatedDataInstanceMember) dataInstances

hasClassInstanceBody :: ClassInstanceBody -> Bool
hasClassInstanceBody (ClassInstanceBody members) = not $ null members

sortMembers ::
     [GHC.LocatedA ClassInstanceMember] -> [WithComments ClassInstanceMember]
sortMembers =
  fmap mkWithCommentsFromGenLocated
    . sortBy (compare `on` GHC.realSrcSpan . GHC.locA . GHC.getLoc)
