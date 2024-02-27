{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module
  ( Module
  , mkModule
  ) where

import Control.Monad.RWS
import Data.Maybe
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.FileHeaderPragma.Collection
import HIndent.Ast.Module.Declaration
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.WithComments
import HIndent.Config
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pragma
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.Import
import HIndent.Pretty.NodeComments
import HIndent.Printer

data Module = Module
  { pragmas :: FileHeaderPragmaCollection
  , moduleDeclaration :: Maybe ModuleDeclaration
  , module' :: GHC.HsModule'
  }

instance CommentExtraction Module where
  nodeComments Module {} = NodeComments [] [] []
#if MIN_VERSION_ghc_lib_parser(9,6,1)
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
        , (importsExist m, prettyImports)
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
      prettyImports = importDecls >>= blanklined . fmap outputImportGroup
      outputImportGroup = lined . fmap pretty
      importDecls =
        gets (configSortImports . psConfig) >>= \case
          True -> pure $ extractImportsSorted m
          False -> pure $ extractImports m
#else
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
        , (importsExist m, prettyImports)
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
      prettyImports = importDecls >>= blanklined . fmap outputImportGroup
      outputImportGroup = lined . fmap pretty
      importDecls =
        gets (configSortImports . psConfig) >>= \case
          True -> pure $ extractImportsSorted m
          False -> pure $ extractImports m
#endif
mkModule :: GHC.HsModule' -> WithComments Module
mkModule m = fromEpAnn (GHC.getModuleAnn m) $ Module {..}
  where
    pragmas = mkFileHeaderPragmaCollection m
    moduleDeclaration = mkModuleDeclaration m
    module' = m
