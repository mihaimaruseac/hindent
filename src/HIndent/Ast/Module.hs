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
import HIndent.Pragma
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
  pretty' Module {module' = m@GHC.HsModule { hsmodName = Nothing
                                           , hsmodImports = []
                                           , hsmodDecls = []
                                           }}
    | not (pragmaExists m) = pure ()
  pretty' Module {module' = m, ..} = blanklined printers >> newline
    where
      printers = snd <$> filter fst pairs
      pairs =
        [ (hasPragmas pragmas, pretty pragmas)
        , (isJust moduleDeclaration, prettyModuleDecl moduleDeclaration)
        , (hasImports imports, pretty imports)
        , (declsExist m, pretty declarations)
        ]
      prettyModuleDecl Nothing = error "The module declaration does not exist."
      prettyModuleDecl (Just decl) = pretty decl
      declsExist = not . null . GHC.hsmodDecls

mkModule :: GHC.HsModule' -> WithComments Module
mkModule m = fromEpAnn (GHC.getModuleAnn m) $ Module {..}
  where
    pragmas = mkFileHeaderPragmaCollection m
    moduleDeclaration = mkModuleDeclaration m
    imports = mkImportCollection m
    declarations = mkDeclarationCollection m
    module' = m
