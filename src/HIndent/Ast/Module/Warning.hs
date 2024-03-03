{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module.Warning
  ( ModuleWarning
  , mkModuleWarning
  ) where

import           HIndent.Ast.Module.Warning.Kind
import           HIndent.Ast.NodeComments
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs                   as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Unit.Module.Warnings as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data ModuleWarning = ModuleWarning
  { messages :: [String]
  , kind     :: Kind
  }

instance CommentExtraction ModuleWarning where
  nodeComments _ = NodeComments [] [] []

instance Pretty ModuleWarning where
  pretty' ModuleWarning {..} = do
    string "{-# "
    pretty kind
    space
    prettyMsgs
    string " #-}"
    where
      prettyMsgs =
        case messages of
          [x] -> string x
          xs  -> hList $ fmap string xs

mkModuleWarning :: GHC.HsModule' -> Maybe (WithComments ModuleWarning)
mkModuleWarning =
  fmap (fromGenLocated . fmap fromWarningTxt) . GHC.getDeprecMessage
  where
    fromWarningTxt (GHC.WarningTxt _ s) = ModuleWarning {..}
      where
        messages = fmap showOutputable s
        kind = Warning
    fromWarningTxt (GHC.DeprecatedTxt _ s) = ModuleWarning {..}
      where
        messages = fmap showOutputable s
        kind = Deprecated
