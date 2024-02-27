{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module
  ( Module
  , mkModule
  ) where

import Data.Maybe
import HIndent.Ast.Declaration.Collection
import HIndent.Ast.FileHeaderPragma.Collection
import HIndent.Ast.Import.Collection
import HIndent.Ast.Module.Declaration
import HIndent.Ast.NodeComments hiding (fromEpAnn)
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Module = Module
  { pragmas :: FileHeaderPragmaCollection
  , moduleDeclaration :: Maybe ModuleDeclaration
  , imports :: ImportCollection
  , declarations :: DeclarationCollection
  , module' :: GHC.HsModule'
  }

instance CommentExtraction Module where
  nodeComments Module {} = NodeComments [] [] []

instance Pretty Module where
  pretty' Module {..}
    | isEmpty = pure ()
    | otherwise = blanklined printers >> newline
    where
      isEmpty =
        not (hasPragmas pragmas)
          && isNothing moduleDeclaration
          && not (hasImports imports)
          && not (hasDeclarations declarations)
      printers = snd <$> filter fst pairs
      pairs =
        [ (hasPragmas pragmas, pretty pragmas)
        , (isJust moduleDeclaration, prettyModuleDecl moduleDeclaration)
        , (hasImports imports, pretty imports)
        , (hasDeclarations declarations, pretty declarations)
        ]
      prettyModuleDecl Nothing = error "The module declaration does not exist."
      prettyModuleDecl (Just decl) = pretty decl

mkModule :: GHC.HsModule' -> WithComments Module
mkModule m = fromEpAnn (GHC.getModuleAnn m) $ Module {..}
  where
    pragmas = mkFileHeaderPragmaCollection m
    moduleDeclaration = mkModuleDeclaration m
    imports = mkImportCollection m
    declarations = mkDeclarationCollection m
    module' = m
