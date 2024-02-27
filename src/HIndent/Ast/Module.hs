{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module
  ( Module
  , mkModule
  ) where

import Data.Maybe
import qualified GHC.Types.SrcLoc as GHC
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
        , (declsExist m, prettyDecls)
        ]
      prettyModuleDecl Nothing = error "The module declaration does not exist."
      prettyModuleDecl (Just decl) = pretty decl
      prettyDecls =
        mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp)
          $ addDeclSeparator
          $ GHC.hsmodDecls m
      addDeclSeparator [] = []
      addDeclSeparator [x] = [(x, Nothing)]
      addDeclSeparator (x:xs) =
        (x, Just $ declSeparator $ GHC.unLoc x) : addDeclSeparator xs
      declSeparator (GHC.SigD _ GHC.TypeSig {}) = newline
      declSeparator (GHC.SigD _ GHC.InlineSig {}) = newline
      declSeparator (GHC.SigD _ GHC.PatSynSig {}) = newline
      declSeparator _ = blankline
      declsExist = not . null . GHC.hsmodDecls

mkModule :: GHC.HsModule' -> WithComments Module
mkModule m = fromEpAnn (GHC.getModuleAnn m) $ Module {..}
  where
    pragmas = mkFileHeaderPragmaCollection m
    moduleDeclaration = mkModuleDeclaration m
    imports = mkImportCollection m
    declarations = mkDeclarationCollection m
    module' = m
