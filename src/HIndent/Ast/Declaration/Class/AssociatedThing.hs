{-# LANGUAGE CPP #-}

module HIndent.Ast.Declaration.Class.AssociatedThing
  ( ClassAssociatedThing
  , classAssociatedSignature
  , classAssociatedMethod
  , classAssociatedTypeFamily
  , classAssociatedDataFamily
  , classAssociatedTypeDefault
  ) where

import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Family.Data
import HIndent.Ast.Declaration.Family.Type
import HIndent.Ast.Declaration.Instance.Family.Type.Associated.Default
import HIndent.Ast.Declaration.Signature
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.NodeComments

data ClassAssociatedThing
  = ClassAssociatedSignature Signature
  | ClassAssociatedMethod Bind
  | ClassAssociatedTypeFamily TypeFamily
  | ClassAssociatedDataFamily DataFamily
  | ClassAssociatedTypeDefault AssociatedTypeDefault

classAssociatedSignature :: Signature -> ClassAssociatedThing
classAssociatedSignature = ClassAssociatedSignature

classAssociatedMethod :: Bind -> ClassAssociatedThing
classAssociatedMethod = ClassAssociatedMethod

classAssociatedTypeFamily :: TypeFamily -> ClassAssociatedThing
classAssociatedTypeFamily = ClassAssociatedTypeFamily

classAssociatedDataFamily :: DataFamily -> ClassAssociatedThing
classAssociatedDataFamily = ClassAssociatedDataFamily

classAssociatedTypeDefault :: AssociatedTypeDefault -> ClassAssociatedThing
classAssociatedTypeDefault = ClassAssociatedTypeDefault

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
