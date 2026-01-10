module HIndent.Ast.Declaration.Class.AssociatedThings
  ( ClassAssociatedThings
  , mkClassAssociatedThings
  , hasAssociatedThings
  ) where

import HIndent.Ast.Declaration.Class.AssociatedThing
  ( ClassAssociatedThing
  , mkClassAssociatedThing
  )
import HIndent.Ast.GhcOrdered.ClassElement (mkSortedClassElements)
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype ClassAssociatedThings =
  ClassAssociatedThings [WithComments ClassAssociatedThing]

instance CommentExtraction ClassAssociatedThings where
  nodeComments ClassAssociatedThings {} = NodeComments [] [] []

instance Pretty ClassAssociatedThings where
  pretty' (ClassAssociatedThings items) = newlinePrefixed $ fmap pretty items

mkClassAssociatedThings ::
     [GHC.LSig GHC.GhcPs]
  -> [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
  -> [GHC.LFamilyDecl GHC.GhcPs]
  -> [GHC.LTyFamInstDecl GHC.GhcPs]
  -> ClassAssociatedThings
mkClassAssociatedThings sigs binds fams tyInsts =
  ClassAssociatedThings
    $ fmap
        (fromGenLocated . fmap mkClassAssociatedThing)
        (mkSortedClassElements sigs binds fams tyInsts)

hasAssociatedThings :: ClassAssociatedThings -> Bool
hasAssociatedThings (ClassAssociatedThings items) = not (null items)
