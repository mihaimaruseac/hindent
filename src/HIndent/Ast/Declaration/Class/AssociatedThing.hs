{-# LANGUAGE CPP #-}

module HIndent.Ast.Declaration.Class.AssociatedThing
  ( ClassAssociatedThing
  , mkClassAssociatedThing
  ) where

import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Family.Data
import HIndent.Ast.Declaration.Family.Type
import HIndent.Ast.Declaration.Instance.Family.Type.Associated.Default
import HIndent.Ast.Declaration.Signature
import HIndent.Ast.GhcOrdered.ClassElement (ClassElement, foldClassElement)
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.NodeComments

data ClassAssociatedThing
  = ClassAssociatedSignature Signature
  | ClassAssociatedMethod Bind
  | ClassAssociatedTypeFamily TypeFamily
  | ClassAssociatedDataFamily DataFamily
  | ClassAssociatedTypeDefault AssociatedTypeDefault

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

-- | Convert a GHC class element into our AST representation.
mkClassAssociatedThing :: ClassElement -> ClassAssociatedThing
mkClassAssociatedThing =
  foldClassElement
    (ClassAssociatedSignature . mkSignature)
    (ClassAssociatedMethod . mkBind)
    convertFamily
    (ClassAssociatedTypeDefault . mkAssociatedTypeDefault)
  where
    convertFamily decl
      | Just fam <- mkTypeFamily decl = ClassAssociatedTypeFamily fam
      | Just fam <- mkDataFamily decl = ClassAssociatedDataFamily fam
      | otherwise = error "Unreachable"
