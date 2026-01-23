{-# LANGUAGE CPP #-}

module HIndent.Ast.Declaration.Class.AssociatedThings
  ( ClassAssociatedThings
  , LClassAssociatedThing
  , mkClassAssociatedThings
  , mkSortedClassAssociatedThings
  , destructClassAssociatedThings
  , hasAssociatedThings
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Family.Data
import HIndent.Ast.Declaration.Family.Type
import HIndent.Ast.Declaration.Instance.Family.Type.Associated.Default
import HIndent.Ast.Declaration.Signature
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data ClassAssociatedThing
  = Sig (GHC.Sig GHC.GhcPs)
  | Method (GHC.HsBindLR GHC.GhcPs GHC.GhcPs)
  | TypeFam (GHC.FamilyDecl GHC.GhcPs)
  | DataFam (GHC.FamilyDecl GHC.GhcPs)
  | TypeDefault (GHC.TyFamInstDecl GHC.GhcPs)

type LClassAssociatedThing = GHC.LocatedA ClassAssociatedThing

newtype ClassAssociatedThings =
  ClassAssociatedThings [WithComments ClassAssociatedThing]

instance CommentExtraction ClassAssociatedThing where
  nodeComments Sig {} = NodeComments [] [] []
  nodeComments Method {} = NodeComments [] [] []
  nodeComments TypeFam {} = NodeComments [] [] []
  nodeComments DataFam {} = NodeComments [] [] []
  nodeComments TypeDefault {} = NodeComments [] [] []

instance Pretty ClassAssociatedThing where
  pretty' (Sig sig) = pretty $ mkSignature sig
  pretty' (Method bind) = pretty $ mkBind bind
  pretty' (TypeFam fam) = pretty $ typeFamilyFrom fam
  pretty' (DataFam fam) = pretty $ dataFamilyFrom fam
  pretty' (TypeDefault defn) = pretty $ mkAssociatedTypeDefault defn

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
    (fmap (fmap Sig) sigs
       ++ fmap (fmap Method) binds
       ++ fmap (fmap classAssociatedFamily) fams
       ++ fmap (fmap TypeDefault) tyInsts)
  where
    classAssociatedFamily decl =
      case GHC.fdInfo decl of
        GHC.DataFamily -> DataFam decl
        _ -> TypeFam decl

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

foldClassAssociatedThing ::
     (GHC.Sig GHC.GhcPs -> r)
  -> (GHC.HsBindLR GHC.GhcPs GHC.GhcPs -> r)
  -> (GHC.FamilyDecl GHC.GhcPs -> r)
  -> (GHC.FamilyDecl GHC.GhcPs -> r)
  -> (GHC.TyFamInstDecl GHC.GhcPs -> r)
  -> ClassAssociatedThing
  -> r
foldClassAssociatedThing onSig _ _ _ _ (Sig sig) = onSig sig
foldClassAssociatedThing _ onBind _ _ _ (Method bind) = onBind bind
foldClassAssociatedThing _ _ onType _ _ (TypeFam fam) = onType fam
foldClassAssociatedThing _ _ _ onData _ (DataFam fam) = onData fam
foldClassAssociatedThing _ _ _ _ onDefault (TypeDefault inst) = onDefault inst

typeFamilyFrom :: GHC.FamilyDecl GHC.GhcPs -> TypeFamily
typeFamilyFrom decl = fromMaybe (error "Unreachable") $ mkTypeFamily decl

dataFamilyFrom :: GHC.FamilyDecl GHC.GhcPs -> DataFamily
dataFamilyFrom decl = fromMaybe (error "Unreachable") $ mkDataFamily decl
