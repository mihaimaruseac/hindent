{-# LANGUAGE CPP #-}

module HIndent.Ast.Context
  ( Context(..)
  , mkContext
  ) where

import           HIndent.Ast.NodeComments
import           HIndent.Ast.Type
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty.NodeComments

newtype Context =
  Context [WithComments Type]

instance CommentExtraction Context where
  nodeComments (Context _) = NodeComments [] [] []

mkContext :: GHC.HsContext GHC.GhcPs -> Context
mkContext = Context . fmap (fmap mkType . fromGenLocated)
