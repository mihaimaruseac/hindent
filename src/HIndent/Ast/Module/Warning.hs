module HIndent.Ast.Module.Warning
  ( ModuleWarning
  , mkModuleWarning
  ) where

import qualified GHC.Hs                                               as GHC
import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs                   as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Unit.Module.Warnings as GHC
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types

newtype ModuleWarning =
  ModuleWarning (GHC.LocatedP GHC.WarningTxt')

instance CommentExtraction ModuleWarning where
  nodeComments _ = NodeComments [] [] []

instance Pretty ModuleWarning where
  pretty' (ModuleWarning x) = pretty' $ fmap ModuleDeprecatedPragma x

mkModuleWarning :: GHC.HsModule' -> Maybe ModuleWarning
mkModuleWarning = fmap ModuleWarning . GHC.getDeprecMessage
