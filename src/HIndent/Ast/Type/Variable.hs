module HIndent.Ast.Type.Variable
  ( TypeVariable(..)
  , mkTypeVariable
  ) where

import qualified GHC.Hs as GHC
import HIndent.Ast.NodeComments
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data TypeVariable = TypeVariable
  { name :: WithComments String
  , kind :: Maybe (WithComments Type)
  }

instance CommentExtraction TypeVariable where
  nodeComments TypeVariable {} = NodeComments [] [] []

mkTypeVariable :: GHC.HsTyVarBndr a GHC.GhcPs -> TypeVariable
mkTypeVariable (GHC.UserTyVar _ _ n) =
  TypeVariable {name = showOutputable <$> fromGenLocated n, kind = Nothing}
mkTypeVariable (GHC.KindedTyVar _ _ n k) =
  TypeVariable
    { name = showOutputable <$> fromGenLocated n
    , kind = Just $ mkType <$> fromGenLocated k
    }
