{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module HIndent.Ast.Declaration.Instance.Class.Member
  ( ClassInstanceMember
  , mkClassInstanceMembers
  ) where

import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Instance.Family.Type.Associated
import HIndent.Ast.Declaration.Signature
import HIndent.Ast.GhcOrdered.InstanceMember
  ( foldInstanceMember
  , mkSortedInstanceMembers
  )
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
  ( DataFamInstDecl'(..)
  , pattern DataFamInstDeclInsideClassInst
  )

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

mkClassInstanceMembers ::
     [GHC.LSig GHC.GhcPs]
  -> [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
  -> [GHC.LTyFamInstDecl GHC.GhcPs]
  -> [GHC.LDataFamInstDecl GHC.GhcPs]
  -> [WithComments ClassInstanceMember]
mkClassInstanceMembers sigs binds tyInsts dataInsts =
  fmap
    (fromGenLocated . fmap convert)
    (mkSortedInstanceMembers sigs binds tyInsts dataInsts)
  where
    convert =
      foldInstanceMember
        (Signature . mkSignature)
        (Method . mkBind)
        (TypeFamily . mkAssociatedType)
        (DataFamily . DataFamInstDeclInsideClassInst)
