{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module HIndent.Ast.Declaration.Collection
  ( DeclarationCollection
  , mkDeclarationCollection
  , hasDeclarations
  ) where

import Data.Maybe
import qualified GHC.Hs as GHC
import HIndent.Ast.Declaration
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype DeclarationCollection =
  DeclarationCollection [WithComments Declaration]

instance Pretty DeclarationCollection where
  pretty (DeclarationCollection decls) =
    mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp)
      $ addDeclSeparator decls
    where
      addDeclSeparator [] = []
      addDeclSeparator [x] = [(x, Nothing)]
      addDeclSeparator (x:y:xs) =
        (x, Just $ declSeparator (getNode x) (getNode y))
          : addDeclSeparator (y : xs)
      declSeparator (isSignature -> True) _ = newline
      declSeparator _ (isInlinePragma -> True) = newline
      declSeparator _ _ = blankline

mkDeclarationCollection :: GHC.HsModule' -> DeclarationCollection
mkDeclarationCollection GHC.HsModule {..} =
  DeclarationCollection
    $ fmap mkDeclaration . mkWithCommentsFromGenLocated <$> hsmodDecls

hasDeclarations :: DeclarationCollection -> Bool
hasDeclarations (DeclarationCollection xs) = not $ null xs
