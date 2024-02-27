{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module
  ( Module
  , mkModule
  ) where

import HIndent.Ast.Module.Declaration
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.NodeComments

data Module = Module
  { moduleDeclaration :: ModuleDeclaration
  , module' :: GHC.HsModule'
  }

instance CommentExtraction Module where
  nodeComments Module {} = NodeComments [] [] []

instance Pretty Module where
  pretty' Module {..} = pretty' module'

mkModule :: GHC.HsModule' -> WithComments Module
mkModule m = fromEpAnn (GHC.getModuleAnn m) $ Module {..}
  where
    moduleDeclaration = mkModuleDeclaration m
    module' = m
