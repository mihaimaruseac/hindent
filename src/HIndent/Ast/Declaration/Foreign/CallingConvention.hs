module HIndent.Ast.Declaration.Foreign.CallingConvention
  ( CallingConvention
  , mkCallingConvention
  ) where

import qualified GHC.Types.ForeignCall as GHC
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data CallingConvention
  = CCall
  | CApi
  | StdCall
  | Prim
  | JavaScript

instance CommentExtraction CallingConvention where
  nodeComments _ = NodeComments [] [] []

instance Pretty CallingConvention where
  pretty' CCall = string "ccall"
  pretty' CApi = string "capi"
  pretty' StdCall = string "stdcall"
  pretty' Prim = string "prim"
  pretty' JavaScript = string "javascript"

mkCallingConvention :: GHC.CCallConv -> CallingConvention
mkCallingConvention GHC.CCallConv = CCall
mkCallingConvention GHC.StdCallConv = StdCall
mkCallingConvention GHC.CApiConv = CApi
mkCallingConvention GHC.PrimCallConv = Prim
mkCallingConvention GHC.JavaScriptCallConv = JavaScript
