{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module.Declaration
  ( ModuleDeclaration
  , mkModuleDeclaration
  ) where

import HIndent.Applicative
import HIndent.Ast.Module.Export.Collection
import HIndent.Ast.Module.Name
import HIndent.Ast.Module.Warning
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data ModuleDeclaration = ModuleDeclaration
  { name :: WithComments ModuleName
  , warning :: Maybe (WithComments ModuleWarning)
  , exports :: Maybe (WithComments ExportCollection)
  }

instance CommentExtraction ModuleDeclaration where
  nodeComments ModuleDeclaration {} = NodeComments [] [] []

instance Pretty ModuleDeclaration where
  pretty' ModuleDeclaration {..} = do
    pretty name
    whenJust warning $ \x -> do
      space
      pretty x
    whenJust exports $ \x -> do
      newline
      indentedBlock $ pretty x
    string " where"

mkModuleDeclaration :: GHC.HsModule' -> Maybe ModuleDeclaration
mkModuleDeclaration m =
  case GHC.hsmodName m of
    Nothing -> Nothing
    Just name' -> Just ModuleDeclaration {..}
      where name = mkModuleName <$> fromGenLocated name'
            warning = mkModuleWarning m
            exports = mkExportCollection m
