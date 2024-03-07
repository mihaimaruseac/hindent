module HIndent.Ast.Type
  ( Type
  , mkType
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments

newtype Type =
  Type (GHC.HsType GHC.GhcPs)

instance CommentExtraction Type where
  nodeComments (Type _) = NodeComments [] [] []

instance Pretty Type where
  pretty' (Type x) = pretty x

mkType :: GHC.HsType GHC.GhcPs -> Type
mkType x@GHC.HsForAllTy {}        = Type x
mkType x@GHC.HsQualTy {}          = Type x
mkType x@GHC.HsTyVar {}           = Type x
mkType x@GHC.HsAppTy {}           = Type x
mkType x@GHC.HsAppKindTy {}       = Type x
mkType x@GHC.HsFunTy {}           = Type x
mkType x@GHC.HsListTy {}          = Type x
mkType x@GHC.HsTupleTy {}         = Type x
mkType x@GHC.HsSumTy {}           = Type x
mkType x@GHC.HsOpTy {}            = Type x
mkType x@GHC.HsParTy {}           = Type x
mkType x@GHC.HsIParamTy {}        = Type x
mkType x@GHC.HsStarTy {}          = Type x
mkType x@GHC.HsKindSig {}         = Type x
mkType x@GHC.HsSpliceTy {}        = Type x
mkType x@GHC.HsDocTy {}           = Type x
mkType x@GHC.HsBangTy {}          = Type x
mkType x@GHC.HsRecTy {}           = Type x
mkType x@GHC.HsExplicitListTy {}  = Type x
mkType x@GHC.HsExplicitTupleTy {} = Type x
mkType x@GHC.HsTyLit {}           = Type x
mkType x@GHC.HsWildCardTy {}      = Type x
mkType x@GHC.XHsType {}           = Type x
