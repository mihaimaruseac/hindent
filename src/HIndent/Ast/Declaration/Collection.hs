{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Collection
  ( DeclarationCollection
  , mkDeclarationCollection
  , hasDeclarations
  ) where

import           Data.Maybe
import qualified GHC.Hs                             as GHC
import           HIndent.Ast.NodeComments
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

newtype DeclarationCollection =
  DeclarationCollection [WithComments (GHC.HsDecl GHC.GhcPs)]

instance CommentExtraction DeclarationCollection where
  nodeComments DeclarationCollection {} = NodeComments [] [] []

instance Pretty DeclarationCollection where
  pretty' (DeclarationCollection decls) =
    mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp) $
    addDeclSeparator decls
    where
      addDeclSeparator [] = []
      addDeclSeparator [x] = [(x, Nothing)]
      addDeclSeparator (x:xs) =
        (x, Just $ declSeparator $ getNode x) : addDeclSeparator xs
      declSeparator (GHC.SigD _ GHC.TypeSig {})   = newline
      declSeparator (GHC.SigD _ GHC.InlineSig {}) = newline
      declSeparator (GHC.SigD _ GHC.PatSynSig {}) = newline
      declSeparator _                             = blankline

mkDeclarationCollection :: GHC.HsModule' -> DeclarationCollection
mkDeclarationCollection GHC.HsModule {..} =
  DeclarationCollection $ fromGenLocated <$> hsmodDecls

hasDeclarations :: DeclarationCollection -> Bool
hasDeclarations (DeclarationCollection xs) = not $ null xs
