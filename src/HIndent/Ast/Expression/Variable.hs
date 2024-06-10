{-# LANGUAGE FlexibleInstances #-}

module HIndent.Ast.Expression.Variable
  ( Variable
  , mkVariable
  ) where

import qualified GHC.Types.Name as GHC
import qualified GHC.Types.Name.Reader as GHC
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments
import HIndent.Printer

newtype Variable =
  Variable (Printer ())

class MkVariable a where
  mkVariable :: a -> Variable

instance CommentExtraction Variable where
  nodeComments Variable {} = NodeComments [] [] []

instance Pretty Variable where
  pretty' (Variable x) = x

instance MkVariable GHC.RdrName where
  mkVariable x = Variable $ pretty x

instance MkVariable GHC.OccName where
  mkVariable x = Variable $ pretty x
