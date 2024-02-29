{-# LANGUAGE CPP #-}

module HIndent.Ast.FileHeaderPragma.Collection
  ( FileHeaderPragmaCollection
  , mkFileHeaderPragmaCollection
  , hasPragmas
  ) where

import           Data.Maybe
import           Generics.SYB
import           HIndent.Ast.FileHeaderPragma
import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

newtype FileHeaderPragmaCollection =
  FileHeaderPragmaCollection [FileHeaderPragma]

instance CommentExtraction FileHeaderPragmaCollection where
  nodeComments _ = NodeComments [] [] []

instance Pretty FileHeaderPragmaCollection where
  pretty' (FileHeaderPragmaCollection xs) = lined $ fmap pretty xs

mkFileHeaderPragmaCollection :: GHC.HsModule' -> FileHeaderPragmaCollection
mkFileHeaderPragmaCollection =
  FileHeaderPragmaCollection .
  mapMaybe mkFileHeaderPragma . collectBlockComments

hasPragmas :: FileHeaderPragmaCollection -> Bool
hasPragmas (FileHeaderPragmaCollection xs) = not $ null xs

collectBlockComments :: GHC.HsModule' -> [GHC.EpaCommentTok]
collectBlockComments = listify isBlockComment . GHC.getModuleAnn

-- | Checks if the given comment is a block one.
isBlockComment :: GHC.EpaCommentTok -> Bool
isBlockComment GHC.EpaBlockComment {} = True
isBlockComment _                      = False
