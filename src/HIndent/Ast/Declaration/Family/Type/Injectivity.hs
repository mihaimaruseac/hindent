{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family.Type.Injectivity
  ( Injectivity
  , mkInjectivity
  ) where

import                          HIndent.Ast.NodeComments
import                qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-}           HIndent.Pretty
import                          HIndent.Pretty.Combinators
import                          HIndent.Pretty.NodeComments

data Injectivity = Injectivity
  { from :: GHC.LIdP GHC.GhcPs
  , to   :: [GHC.LIdP GHC.GhcPs]
  }

instance CommentExtraction Injectivity where
  nodeComments Injectivity {} = NodeComments [] [] []

instance Pretty Injectivity where
  pretty' Injectivity {..} = spaced $ pretty from : string "->" : fmap pretty to

mkInjectivity :: GHC.InjectivityAnn GHC.GhcPs -> Injectivity
mkInjectivity (GHC.InjectivityAnn _ from to) = Injectivity {..}
