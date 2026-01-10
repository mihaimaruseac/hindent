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
  = Sig Signature
  | Method Bind
  | TypeFam TypeFamily
  | DataFam DataFamily
  | TypeDefault AssociatedTypeDefault

instance CommentExtraction ClassAssociatedThing where
  nodeComments Sig {} = NodeComments [] [] []
  nodeComments Method {} = NodeComments [] [] []
  nodeComments TypeFam {} = NodeComments [] [] []
  nodeComments DataFam {} = NodeComments [] [] []
  nodeComments TypeDefault {} = NodeComments [] [] []

instance Pretty ClassAssociatedThing where
  pretty' (Sig sig) = pretty sig
  pretty' (Method bind) = pretty bind
  pretty' (TypeFam fam) = pretty fam
  pretty' (DataFam fam) = pretty fam
  pretty' (TypeDefault defn) = pretty defn

mkClassAssociatedThing :: ClassElement -> ClassAssociatedThing
mkClassAssociatedThing =
  foldClassElement
    (Sig . mkSignature)
    (Method . mkBind)
    convertFamily
    (TypeDefault . mkAssociatedTypeDefault)
  where
    convertFamily decl
      | Just fam <- mkTypeFamily decl = TypeFam fam
      | Just fam <- mkDataFamily decl = DataFam fam
      | otherwise = error "Unreachable"
