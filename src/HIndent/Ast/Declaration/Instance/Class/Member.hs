{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module HIndent.Ast.Declaration.Instance.Class.Member
  ( ClassInstanceMember
  , mkClassInstanceMembers
  ) where

import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Instance.Family.Type.Associated
import HIndent.Ast.Declaration.Signature
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.GhcOrdered.InstanceMember
  ( foldInstanceMember
  , mkSortedInstanceMembers
  )
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
  ( DataFamInstDecl'(..)
  , pattern DataFamInstDeclInsideClassInst
  )

data ClassInstanceMember
  = InstanceSignature Signature
  | InstanceMethod Bind
  | InstanceTypeFamily AssociatedType
  | InstanceDataFamily DataFamInstDecl'

instance CommentExtraction ClassInstanceMember where
  nodeComments InstanceSignature {} = NodeComments [] [] []
  nodeComments InstanceMethod {} = NodeComments [] [] []
  nodeComments InstanceTypeFamily {} = NodeComments [] [] []
  nodeComments InstanceDataFamily {} = NodeComments [] [] []

instance Pretty ClassInstanceMember where
  pretty' (InstanceSignature sig) = pretty sig
  pretty' (InstanceMethod bind) = pretty bind
  pretty' (InstanceTypeFamily assoc) = pretty assoc
  pretty' (InstanceDataFamily inst) = pretty inst

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
        (InstanceSignature . mkSignature)
        (InstanceMethod . mkBind)
        (InstanceTypeFamily . mkAssociatedType)
        (InstanceDataFamily . DataFamInstDeclInsideClassInst)
