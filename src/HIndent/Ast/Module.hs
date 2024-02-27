{-# LANGUAGE CPP #-}

module HIndent.Ast.Module
  ( Module
  , mkModule
  ) where

import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types

newtype Module =
  Module GHC.HsModule'

instance CommentExtraction Module where
  nodeComments Module {} = NodeComments [] [] []

instance Pretty Module where
  pretty' (Module x) = pretty x

mkModule :: GHC.HsModule' -> WithComments Module
mkModule = mkWithComments . Module
