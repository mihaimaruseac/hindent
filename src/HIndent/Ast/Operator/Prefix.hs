{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Operator.Prefix
  ( PrefixOperator
  , mkPrefixOperator
  ) where

import Data.Maybe
import qualified GHC.Types.Name as GHC
import qualified GHC.Types.Name.Reader as GHC
import qualified GHC.Unit.Module as GHC
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data PrefixOperator = PrefixOperator
  { name :: String
  , moduleName :: Maybe GHC.ModuleName
  , parentheses :: Bool
  }

instance CommentExtraction PrefixOperator where
  nodeComments PrefixOperator {} = NodeComments [] [] []

instance Pretty PrefixOperator where
  pretty' PrefixOperator {..} =
    wrap $ hDotSep $ catMaybes [pretty <$> moduleName, Just $ string name]
    where
      wrap =
        if parentheses
          then parens
          else id

mkPrefixOperator :: GHC.RdrName -> PrefixOperator
mkPrefixOperator (GHC.Unqual name) =
  PrefixOperator (showOutputable name) Nothing (parensNeeded name)
mkPrefixOperator (GHC.Qual modName name) =
  PrefixOperator (showOutputable name) (Just modName) (parensNeeded name)
mkPrefixOperator (GHC.Orig {}) =
  error "This AST node should not appear in the parser output."
mkPrefixOperator (GHC.Exact name) =
  PrefixOperator (showOutputable name) Nothing (parensNeeded $ GHC.occName name)

parensNeeded :: GHC.OccName -> Bool
parensNeeded = GHC.isSymOcc
