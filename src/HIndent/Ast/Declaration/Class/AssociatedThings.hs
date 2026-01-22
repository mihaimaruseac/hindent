module HIndent.Ast.Declaration.Class.AssociatedThings
  ( ClassAssociatedThings
  , mkClassAssociatedThings
  , mkSortedClassAssociatedThings
  , destructClassAssociatedThings
  , hasAssociatedThings
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Declaration.Class.AssociatedThing
  ( ClassAssociatedThing
  , LClassAssociatedThing
  , foldClassAssociatedThing
  , mkClassAssociatedFamily
  , mkClassAssociatedMethod
  , mkClassAssociatedSig
  , mkClassAssociatedTypeDefault
  )
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
        fromGenLocated
        (mkSortedClassAssociatedThings sigs binds fams tyInsts)

mkSortedClassAssociatedThings ::
     [GHC.LSig GHC.GhcPs]
  -> [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
  -> [GHC.LFamilyDecl GHC.GhcPs]
  -> [GHC.LTyFamInstDecl GHC.GhcPs]
  -> [LClassAssociatedThing]
mkSortedClassAssociatedThings sigs binds fams tyInsts =
  sortBy
    (compare `on` GHC.realSrcSpan . GHC.locA . GHC.getLoc)
    (fmap (fmap mkClassAssociatedSig) sigs
       ++ fmap (fmap mkClassAssociatedMethod) binds
       ++ fmap (fmap mkClassAssociatedFamily) fams
       ++ fmap (fmap mkClassAssociatedTypeDefault) tyInsts)

destructClassAssociatedThings ::
     [LClassAssociatedThing]
  -> ( [GHC.LSig GHC.GhcPs]
     , [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
     , [GHC.LFamilyDecl GHC.GhcPs]
     , [GHC.LTyFamInstDecl GHC.GhcPs])
destructClassAssociatedThings xs =
  ( mapMaybe toSig xs
  , mapMaybe toBind xs
  , mapMaybe toFamily xs
  , mapMaybe toTyInst xs)
  where
    toSig (GHC.L l item) =
      foldClassAssociatedThing
        (Just . GHC.L l)
        (const Nothing)
        (const Nothing)
        (const Nothing)
        (const Nothing)
        item
    toBind (GHC.L l item) =
      foldClassAssociatedThing
        (const Nothing)
        (Just . GHC.L l)
        (const Nothing)
        (const Nothing)
        (const Nothing)
        item
    toFamily (GHC.L l item) =
      foldClassAssociatedThing
        (const Nothing)
        (const Nothing)
        (Just . GHC.L l)
        (Just . GHC.L l)
        (const Nothing)
        item
    toTyInst (GHC.L l item) =
      foldClassAssociatedThing
        (const Nothing)
        (const Nothing)
        (const Nothing)
        (const Nothing)
        (Just . GHC.L l)
        item

hasAssociatedThings :: ClassAssociatedThings -> Bool
hasAssociatedThings (ClassAssociatedThings items) = not (null items)
