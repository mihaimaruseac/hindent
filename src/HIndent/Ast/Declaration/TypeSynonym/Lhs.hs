{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.TypeSynonym.Lhs
  ( TypeSynonymLhs
  , mkTypeSynonymLhs
  ) where

import qualified GHC.Types.Fixity as GHC
import HIndent.Ast.Name.Infix
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data TypeSynonymLhs
  = Prefix
      { pName :: WithComments PrefixName -- Using `name` in both `Prefix` and `Infix` causes a type conflict.
      , typeVariables :: [WithComments TypeVariable]
      }
  | Infix
      { left :: WithComments TypeVariable
      , iName :: WithComments InfixName
      , right :: WithComments TypeVariable
      }

instance CommentExtraction TypeSynonymLhs where
  nodeComments Prefix {} = NodeComments [] [] []
  nodeComments Infix {} = NodeComments [] [] []

instance Pretty TypeSynonymLhs where
  pretty' Prefix {..} = spaced $ pretty pName : fmap pretty typeVariables
  pretty' Infix {..} = spaced [pretty left, pretty iName, pretty right]

mkTypeSynonymLhs :: GHC.TyClDecl GHC.GhcPs -> TypeSynonymLhs
mkTypeSynonymLhs GHC.SynDecl {tcdFixity = GHC.Prefix, ..} = Prefix {..}
  where
    pName = fromGenLocated $ fmap mkPrefixName tcdLName
    typeVariables =
      fmap mkTypeVariable . fromGenLocated <$> GHC.hsq_explicit tcdTyVars
mkTypeSynonymLhs GHC.SynDecl {tcdFixity = GHC.Infix, ..} =
  case GHC.hsq_explicit tcdTyVars of
    [l, r] -> Infix {..}
      where
        left = mkTypeVariable <$> fromGenLocated l
        iName = fromGenLocated $ fmap mkInfixName tcdLName
        right = mkTypeVariable <$> fromGenLocated r
    _ -> error "Unexpected number of type variables for infix type synonym."
mkTypeSynonymLhs _ = error "Not a type synonym."
