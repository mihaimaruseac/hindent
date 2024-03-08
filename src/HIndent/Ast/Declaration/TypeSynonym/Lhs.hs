{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.TypeSynonym.Lhs
  ( TypeSynonymLhs
  , mkTypeSynonymLhs
  ) where

import qualified GHC.Types.Fixity as GHC
import HIndent.Ast.NodeComments
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types

data TypeSynonymLhs
  = Prefix
      { name :: GHC.LIdP GHC.GhcPs
      , typeVariables :: [WithComments TypeVariable]
      }
  | Infix
      { left :: WithComments TypeVariable
      , name :: GHC.LIdP GHC.GhcPs
      , right :: WithComments TypeVariable
      }

instance CommentExtraction TypeSynonymLhs where
  nodeComments Prefix {} = NodeComments [] [] []
  nodeComments Infix {} = NodeComments [] [] []

instance Pretty TypeSynonymLhs where
  pretty' Prefix {..} = spaced $ pretty name : fmap pretty typeVariables
  pretty' Infix {..} =
    spaced [pretty left, pretty $ fmap InfixOp name, pretty right]

mkTypeSynonymLhs :: GHC.TyClDecl GHC.GhcPs -> TypeSynonymLhs
mkTypeSynonymLhs GHC.SynDecl {tcdFixity = GHC.Prefix, ..} =
  Prefix
    { name = tcdLName
    , typeVariables =
        fmap mkTypeVariable . fromGenLocated <$> GHC.hsq_explicit tcdTyVars
    }
mkTypeSynonymLhs GHC.SynDecl {tcdFixity = GHC.Infix, ..} =
  case GHC.hsq_explicit tcdTyVars of
    [l, r] ->
      Infix
        { left = mkTypeVariable <$> fromGenLocated l
        , name = tcdLName
        , right = mkTypeVariable <$> fromGenLocated r
        }
    _ -> error "Unexpected number of type variables for infix type synonym."
mkTypeSynonymLhs _ = error "Not a type synonym."
