{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module.Declaration
  ( ModuleDeclaration
  , mkModuleDeclaration
  ) where

import HIndent.Ast.Module.Name
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Unit.Module.Warnings as GHC
import HIndent.Pretty.NodeComments

data ModuleDeclaration = ModuleDeclaration
  { name :: WithComments ModuleName
  , warning :: Maybe (GHC.LocatedP GHC.WarningTxt')
  , exports :: Maybe (GHC.LocatedL [GHC.LIE GHC.GhcPs])
  }

instance CommentExtraction ModuleDeclaration where
  nodeComments ModuleDeclaration {} = NodeComments [] [] []

mkModuleDeclaration :: GHC.HsModule' -> Maybe ModuleDeclaration
mkModuleDeclaration m =
  case GHC.hsmodName m of
    Nothing -> Nothing
    Just name' -> Just ModuleDeclaration {..}
      where name = mkModuleName <$> fromGenLocated name'
            warning = GHC.getDeprecMessage m
            exports = GHC.hsmodExports m
