{-# LANGUAGE CPP #-}

module HIndent.Ast.Declaration.Class.AssociatedThing
  ( ClassAssociatedThing
  , ClassAssociatedThings(..)
  , mkClassAssociatedThings
  ) where

import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Family.Data
import HIndent.Ast.Declaration.Family.Type
import HIndent.Ast.Declaration.Instance.Family.Type.Associated.Default
import HIndent.Ast.Declaration.Signature
import HIndent.Ast.GhcOrdered.ClassElement
  ( foldClassElement
  , mkSortedClassElements
  )
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data ClassAssociatedThing
  = ClassAssociatedSignature Signature
  | ClassAssociatedMethod Bind
  | ClassAssociatedTypeFamily TypeFamily
  | ClassAssociatedDataFamily DataFamily
  | ClassAssociatedTypeDefault AssociatedTypeDefault

newtype ClassAssociatedThings =
  ClassAssociatedThings [WithComments ClassAssociatedThing]

instance CommentExtraction ClassAssociatedThing where
  nodeComments ClassAssociatedSignature {} = NodeComments [] [] []
  nodeComments ClassAssociatedMethod {} = NodeComments [] [] []
  nodeComments ClassAssociatedTypeFamily {} = NodeComments [] [] []
  nodeComments ClassAssociatedDataFamily {} = NodeComments [] [] []
  nodeComments ClassAssociatedTypeDefault {} = NodeComments [] [] []

instance Pretty ClassAssociatedThing where
  pretty' (ClassAssociatedSignature sig) = pretty sig
  pretty' (ClassAssociatedMethod bind) = pretty bind
  pretty' (ClassAssociatedTypeFamily fam) = pretty fam
  pretty' (ClassAssociatedDataFamily fam) = pretty fam
  pretty' (ClassAssociatedTypeDefault defn) = pretty defn

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
        (fromGenLocated . fmap convert)
        (mkSortedClassElements sigs binds fams tyInsts)
  where
    convert =
      foldClassElement
        (ClassAssociatedSignature . mkSignature)
        (ClassAssociatedMethod . mkBind)
        convertFamily
        (ClassAssociatedTypeDefault . mkAssociatedTypeDefault)
    convertFamily decl
      | Just fam <- mkTypeFamily decl = ClassAssociatedTypeFamily fam
      | Just fam <- mkDataFamily decl = ClassAssociatedDataFamily fam
      | otherwise = error "Unreachable"
