module HIndent.Ast.Declaration.Instance.Family.Type
  ( TypeFamilyInstance(..)
  , mkTypeFamilyInstance
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty.NodeComments

newtype TypeFamilyInstance =
  TypeFamilyInstance (GHC.InstDecl GHC.GhcPs)

instance CommentExtraction TypeFamilyInstance where
  nodeComments TypeFamilyInstance {} = NodeComments [] [] []

mkTypeFamilyInstance :: GHC.InstDecl GHC.GhcPs -> Maybe TypeFamilyInstance
mkTypeFamilyInstance x@GHC.TyFamInstD {} = Just $ TypeFamilyInstance x
mkTypeFamilyInstance _                   = Nothing
