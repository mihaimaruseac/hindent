{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Operator.Infix
  ( InfixOperator
  , mkInfixOperator
  ) where

import Data.Maybe
import qualified GHC.Types.Name as GHC
import qualified GHC.Types.Name.Reader as GHC
import qualified GHC.Unit.Module as GHC
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data InfixOperator = InfixOperator
  { name :: GHC.OccName
  , moduleName :: Maybe GHC.ModuleName
  , backtick :: Bool
  }

instance CommentExtraction InfixOperator where
  nodeComments InfixOperator {} = NodeComments [] [] []

instance Pretty InfixOperator where
  pretty' InfixOperator {..} =
    wrap $ hDotSep $ catMaybes [pretty <$> moduleName, Just $ pretty name]
    where
      wrap =
        if backtick
          then backticks
          else id

mkInfixOperator :: GHC.RdrName -> InfixOperator
mkInfixOperator (GHC.Unqual name) =
  InfixOperator name Nothing (backticksNeeded name)
mkInfixOperator (GHC.Qual modName name) =
  InfixOperator name (Just modName) (backticksNeeded name)
mkInfixOperator (GHC.Orig {}) =
  error "This AST node should not appear in the parser output."
mkInfixOperator (GHC.Exact name) =
  InfixOperator (GHC.occName name) Nothing (backticksNeeded $ GHC.occName name)

backticksNeeded :: GHC.OccName -> Bool
backticksNeeded = not . GHC.isSymOcc
