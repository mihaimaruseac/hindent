{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Name.Prefix
  ( PrefixName
  , mkPrefixName
  ) where

import Data.Maybe
import qualified GHC.Types.Name as GHC
import qualified GHC.Types.Name.Reader as GHC
import qualified GHC.Unit.Module as GHC
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data PrefixName = PrefixName
  { name :: String
  , moduleName :: Maybe GHC.ModuleName
  , parentheses :: Bool
  }

instance CommentExtraction PrefixName where
  nodeComments PrefixName {} = NodeComments [] [] []

instance Pretty PrefixName where
  pretty' PrefixName {..} =
    wrap $ hDotSep $ catMaybes [pretty <$> moduleName, Just $ string name]
    where
      wrap =
        if parentheses
          then parens
          else id

mkPrefixName :: GHC.RdrName -> PrefixName
mkPrefixName (GHC.Unqual name) =
  PrefixName (showOutputable name) Nothing (parensNeeded name)
mkPrefixName (GHC.Qual modName name) =
  PrefixName (showOutputable name) (Just modName) (parensNeeded name)
mkPrefixName (GHC.Orig {}) =
  error "This AST node should not appear in the parser output."
mkPrefixName (GHC.Exact name) =
  PrefixName (showOutputable name) Nothing (parensNeeded $ GHC.occName name)

parensNeeded :: GHC.OccName -> Bool
parensNeeded = GHC.isSymOcc
