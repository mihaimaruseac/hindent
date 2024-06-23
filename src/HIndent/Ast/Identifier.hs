module HIndent.Ast.Identifier
  ( Identifier
  , mkIdentifier
  ) where

import qualified GHC.Types.Name as GHC
import qualified GHC.Types.Name.Reader as GHC
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments
import HIndent.Printer

newtype Identifier =
  Identifier (Printer ())

class MkIdentifier a where
  mkIdentifier :: a -> Identifier

instance CommentExtraction Identifier where
  nodeComments Identifier {} = NodeComments [] [] []

instance Pretty Identifier where
  pretty' (Identifier x) = x

instance MkIdentifier GHC.RdrName where
  mkIdentifier x = Identifier $ pretty x

instance MkIdentifier GHC.OccName where
  mkIdentifier x = Identifier $ pretty x
