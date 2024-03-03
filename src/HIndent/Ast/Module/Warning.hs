module HIndent.Ast.Module.Warning
  ( ModuleWarning
  , mkModuleWarning
  ) where

import           HIndent.Ast.NodeComments
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs                   as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Unit.Module.Warnings as GHC
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types

newtype ModuleWarning =
  ModuleWarning GHC.WarningTxt'

instance CommentExtraction ModuleWarning where
  nodeComments _ = NodeComments [] [] []

instance Pretty ModuleWarning where
  pretty' (ModuleWarning x) = pretty' $ ModuleDeprecatedPragma x

mkModuleWarning :: GHC.HsModule' -> Maybe (WithComments ModuleWarning)
mkModuleWarning =
  fmap (fromGenLocated . fmap ModuleWarning) . GHC.getDeprecMessage
