module HIndent.Ast.Declaration.Class.AssociatedThings
  ( AssociatedThings
  , mkAssociatedThings
  , nullAssociatedThings
  ) where

import HIndent.Ast.Declaration.Class.AssociatedThing
  ( ClassAssociatedThing
  , mkClassAssociatedThings
  )
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments (CommentExtraction(..))

newtype AssociatedThings =
  AssociatedThings [WithComments ClassAssociatedThing]

instance CommentExtraction AssociatedThings where
  nodeComments AssociatedThings {} = NodeComments [] [] []

instance Pretty AssociatedThings where
  pretty' (AssociatedThings items) = newlinePrefixed $ fmap pretty items

mkAssociatedThings ::
     [GHC.LSig GHC.GhcPs]
  -> [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
  -> [GHC.LFamilyDecl GHC.GhcPs]
  -> [GHC.LTyFamInstDecl GHC.GhcPs]
  -> AssociatedThings
mkAssociatedThings sigs binds fams tyInsts =
  AssociatedThings (mkClassAssociatedThings sigs binds fams tyInsts)

nullAssociatedThings :: AssociatedThings -> Bool
nullAssociatedThings (AssociatedThings items) = null items
