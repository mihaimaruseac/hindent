module HIndent.Ast.LocalBinds.Declaration.Collection
  ( LocalDeclarationCollection
  , hasLocalDeclarations
  , mkLocalDeclarationCollection
  ) where

import Data.Function
import Data.List (sortBy)
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.LocalBinds.Declaration
import HIndent.Ast.WithComments (WithComments, mkWithCommentsFromGenLocated)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments (CommentExtraction(..), emptyNodeComments)

newtype LocalDeclarationCollection =
  LocalDeclarationCollection [WithComments LocalDeclaration]

instance CommentExtraction LocalDeclarationCollection where
  nodeComments _ = emptyNodeComments

instance Pretty LocalDeclarationCollection where
  pretty' (LocalDeclarationCollection declarations) =
    lined $ fmap pretty declarations

mkLocalDeclarationCollection ::
     [GHC.LSig GHC.GhcPs]
  -> [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
  -> LocalDeclarationCollection
mkLocalDeclarationCollection sigs binds =
  LocalDeclarationCollection
    $ fmap mkWithCommentsFromGenLocated
    $ sortBy (compare `on` GHC.realSrcSpan . GHC.locA . GHC.getLoc)
    $ fmap (fmap mkLocalSignatureDeclaration) sigs
        ++ fmap (fmap mkLocalBindingDeclaration) binds

hasLocalDeclarations :: LocalDeclarationCollection -> Bool
hasLocalDeclarations (LocalDeclarationCollection declarations) =
  not $ null declarations
