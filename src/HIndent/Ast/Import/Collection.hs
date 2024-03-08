{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import.Collection
  ( ImportCollection
  , mkImportCollection
  , hasImports
  ) where

import Control.Monad.RWS
import Data.Function
import Data.List
import qualified GHC.Hs as GHC
import GHC.Stack
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Import
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import HIndent.Config
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Printer

newtype ImportCollection =
  ImportCollection [[WithComments Import]]

instance CommentExtraction ImportCollection where
  nodeComments ImportCollection {} = NodeComments [] [] []

instance Pretty ImportCollection where
  pretty' (ImportCollection xs) =
    importDecls >>= blanklined . fmap outputImportGroup
    where
      outputImportGroup = lined . fmap pretty
      importDecls =
        gets (configSortImports . psConfig) >>= \case
          True -> pure $ fmap sortByName xs
          False -> pure xs

mkImportCollection :: GHC.HsModule' -> ImportCollection
mkImportCollection GHC.HsModule {..} =
  ImportCollection
    $ fmap
        (fmap (fmap mkImport . fromGenLocated))
        (extractImports' hsmodImports)

hasImports :: ImportCollection -> Bool
hasImports (ImportCollection xs) = not $ null xs

-- | Extracts import declarations from the given module. Adjacent import
-- declarations are grouped as a single list.
extractImports' :: [GHC.LImportDecl GHC.GhcPs] -> [[GHC.LImportDecl GHC.GhcPs]]
extractImports' = groupImports . sortImportsByLocation

-- | Combines adjacent import declarations into a single list.
groupImports :: [GHC.LImportDecl GHC.GhcPs] -> [[GHC.LImportDecl GHC.GhcPs]]
groupImports = groupImports' []
  where
    groupImports' ::
         [[GHC.LImportDecl GHC.GhcPs]]
      -> [GHC.LImportDecl GHC.GhcPs]
      -> [[GHC.LImportDecl GHC.GhcPs]]
    groupImports' xs [] = xs
    groupImports' [] (x:xs) = groupImports' [[x]] xs
    groupImports' [[]] (x:xs) = groupImports' [[x]] xs
    groupImports' ([]:x:xs) (y:ys) = groupImports' ([y] : x : xs) ys
    groupImports' ((z:zs):xs) (y:ys)
      | z `isAdjacentTo` y = groupImports' ((y : z : zs) : xs) ys
      | otherwise = groupImports' ([y] : (z : zs) : xs) ys
    a `isAdjacentTo` b =
      GHC.srcSpanEndLine (sp a) + 1 == GHC.srcSpanStartLine (sp b)
        || GHC.srcSpanEndLine (sp b) + 1 == GHC.srcSpanStartLine (sp a)
    sp x =
      case GHC.locA $ GHC.getLoc x of
        GHC.RealSrcSpan x' _ -> x'
        _ -> error "Src span unavailable."

-- | This function sorts imports by their start line numbers.
sortImportsByLocation ::
     [GHC.LImportDecl GHC.GhcPs] -> [GHC.LImportDecl GHC.GhcPs]
sortImportsByLocation = sortBy (flip compare `on` lineIdx)
  where
    lineIdx = startLine . GHC.locA . GHC.getLoc

-- | This function returns the start line of the given 'SrcSpan'. If it is
-- not available, it raises an error.
startLine :: HasCallStack => GHC.SrcSpan -> Int
startLine (GHC.RealSrcSpan x _) = GHC.srcSpanStartLine x
startLine (GHC.UnhelpfulSpan _) = error "The src span is unavailable."
