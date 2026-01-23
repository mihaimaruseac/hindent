module HIndent.Ast.Declaration.Instance.Class.Member
  ( ClassInstanceMember
  , mkClassInstanceMember
  ) where

import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Instance.Family.Type.Associated
import HIndent.Ast.Declaration.Signature
import HIndent.Ast.GhcOrdered.InstanceMember
  ( InstanceMember
  , foldInstanceMember
  )
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types (DataFamInstDecl'(..), DataFamInstDeclFor(..))

data ClassInstanceMember
  = Signature Signature
  | Method Bind
  | TypeFamily AssociatedType
  | DataFamily DataFamInstDecl'

instance CommentExtraction ClassInstanceMember where
  nodeComments Signature {} = NodeComments [] [] []
  nodeComments Method {} = NodeComments [] [] []
  nodeComments TypeFamily {} = NodeComments [] [] []
  nodeComments DataFamily {} = NodeComments [] [] []

instance Pretty ClassInstanceMember where
  pretty' (Signature sig) = pretty sig
  pretty' (Method bind) = pretty bind
  pretty' (TypeFamily assoc) = pretty assoc
  pretty' (DataFamily inst) = pretty inst

mkClassInstanceMember :: InstanceMember -> ClassInstanceMember
mkClassInstanceMember =
  foldInstanceMember
    (Signature . mkSignature)
    (Method . mkBind)
    (TypeFamily . mkAssociatedType)
    (DataFamily . DataFamInstDecl' DataFamInstDeclForInsideClassInst)
