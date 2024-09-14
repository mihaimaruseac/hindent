{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family.Type.Injectivity
  ( Injectivity
  , mkInjectivity
  ) where

import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Injectivity = Injectivity
  { from :: WithComments PrefixName
  , to :: [WithComments PrefixName]
  }

instance CommentExtraction Injectivity where
  nodeComments Injectivity {} = NodeComments [] [] []

instance Pretty Injectivity where
  pretty' Injectivity {..} = spaced $ pretty from : string "->" : fmap pretty to

mkInjectivity :: GHC.InjectivityAnn GHC.GhcPs -> Injectivity
mkInjectivity (GHC.InjectivityAnn _ f t) = Injectivity {..}
  where
    from = fromGenLocated $ fmap mkPrefixName f
    to = fmap (fromGenLocated . fmap mkPrefixName) t
