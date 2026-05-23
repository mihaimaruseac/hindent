module HIndent.Ast.Declaration.Class.Body
  ( ClassBody
  , mkClassBody
  , hasClassBody
  ) where

import Data.Function
import Data.List (sortBy)
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Declaration.Class.Member
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype ClassBody =
  ClassBody [WithComments ClassMember]

instance CommentExtraction ClassBody where
  nodeComments ClassBody {} = NodeComments [] [] []

instance Pretty ClassBody where
  pretty' (ClassBody members) = newlinePrefixed $ fmap pretty members

mkClassBody ::
     [GHC.LSig GHC.GhcPs]
  -> [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
  -> [GHC.LFamilyDecl GHC.GhcPs]
  -> [GHC.LTyFamDefltDecl GHC.GhcPs]
  -> ClassBody
mkClassBody sigs binds families defaults =
  ClassBody
    $ sortMembers
    $ fmap (fmap mkClassSignatureMember) sigs
        ++ fmap (fmap mkClassMethodMember) binds
        ++ fmap (fmap mkAssociatedFamilyMember) families
        ++ fmap (fmap mkAssociatedTypeDefaultMember) defaults

hasClassBody :: ClassBody -> Bool
hasClassBody (ClassBody members) = not $ null members

sortMembers :: [GHC.LocatedA ClassMember] -> [WithComments ClassMember]
sortMembers =
  fmap fromGenLocated
    . sortBy (compare `on` GHC.realSrcSpan . GHC.locA . GHC.getLoc)
