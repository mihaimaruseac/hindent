{-# LANGUAGE CPP #-}
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
      printers =
        catMaybes
          [ toMaybe (hasPragmas pragmas) (pretty pragmas)
          , fmap pretty moduleDeclaration
          , toMaybe (hasImports imports) (pretty imports)
          , toMaybe (hasDeclarations declarations) (pretty declarations)
          ]
      toMaybe cond x =
        if cond
          then Just x
          else Nothing

mkModule :: GHC.HsModule' -> WithComments Module
mkModule m = fromEpAnn (GHC.getModuleAnn m) $ Module {..}
  where
    pragmas = mkFileHeaderPragmaCollection m
    moduleDeclaration = mkModuleDeclaration m
    imports = mkImportCollection m
    declarations = mkDeclarationCollection m
