{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.Expression.QualifiedDo
  ( QualifiedDo
  , mkQualifiedDo
  ) where

import HIndent.Ast.Expression.DoOrMdo (DoOrMdo, mkDoOrMdo)
import HIndent.Ast.Module.Name (ModuleName, mkModuleName)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data QualifiedDo =
  QualifiedDo (Maybe ModuleName) DoOrMdo

instance CommentExtraction QualifiedDo where
  nodeComments = const emptyNodeComments

instance Pretty QualifiedDo where
  pretty' (QualifiedDo (Just moduleName) doOrMdo) = do
    pretty moduleName
    string "."
    pretty doOrMdo
  pretty' (QualifiedDo Nothing doOrMdo) = pretty doOrMdo

mkQualifiedDo :: GHC.HsDoFlavour -> QualifiedDo
mkQualifiedDo stmtContext@(GHC.DoExpr moduleName) =
  QualifiedDo (fmap mkModuleName moduleName) $ mkDoOrMdo stmtContext
mkQualifiedDo stmtContext@(GHC.MDoExpr moduleName) =
  QualifiedDo (fmap mkModuleName moduleName) $ mkDoOrMdo stmtContext
mkQualifiedDo _ = error "`mkQualifiedDo` only supports `DoExpr` and `MDoExpr`."
