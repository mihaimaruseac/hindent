{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module.Declaration
  ( ModuleDeclaration
  , mkModuleDeclaration
  ) where

import HIndent.Ast.Module.Name
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC

newtype ModuleDeclaration = ModuleDeclaration
  { name :: WithComments ModuleName
  }

mkModuleDeclaration :: GHC.HsModule' -> Maybe ModuleDeclaration
mkModuleDeclaration GHC.HsModule {..} =
  case hsmodName of
    Nothing -> Nothing
    Just name' -> Just ModuleDeclaration {..}
      where name = mkModuleName <$> fromGenLocated name'
