{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module.Declaration
  ( ModuleDeclaration
  , mkModuleDeclaration
  ) where

import HIndent.Ast.Module.Name
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC

data ModuleDeclaration = ModuleDeclaration
  { name :: WithComments ModuleName
  , exports :: Maybe (GHC.LocatedL [GHC.LIE GHC.GhcPs])
  }

mkModuleDeclaration :: GHC.HsModule' -> Maybe ModuleDeclaration
mkModuleDeclaration GHC.HsModule {..} =
  case hsmodName of
    Nothing -> Nothing
    Just name' -> Just ModuleDeclaration {..}
      where name = mkModuleName <$> fromGenLocated name'
            exports = hsmodExports
