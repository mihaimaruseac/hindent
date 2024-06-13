{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Class.NameAndTypeVariables
  ( NameAndTypeVariables
  , mkNameAndTypeVariables
  ) where

import qualified GHC.Types.Fixity as GHC
import HIndent.Ast.NodeComments
import HIndent.Ast.Operator.Infix
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data NameAndTypeVariables
  = Prefix
      { name :: GHC.LIdP GHC.GhcPs
      , typeVariables :: [WithComments TypeVariable]
      }
  | Infix
      { left :: WithComments TypeVariable
      , name :: GHC.LIdP GHC.GhcPs
      , right :: WithComments TypeVariable
      , remains :: [WithComments TypeVariable]
      }

instance CommentExtraction NameAndTypeVariables where
  nodeComments Prefix {} = NodeComments [] [] []
  nodeComments Infix {} = NodeComments [] [] []

instance Pretty NameAndTypeVariables where
  pretty' Prefix {..} = spaced $ pretty name : fmap pretty typeVariables
  pretty' Infix {..} = do
    parens
      $ spaced [pretty left, pretty $ fmap mkInfixOperator name, pretty right]
    spacePrefixed $ fmap pretty remains

mkNameAndTypeVariables :: GHC.TyClDecl GHC.GhcPs -> Maybe NameAndTypeVariables
mkNameAndTypeVariables GHC.ClassDecl {tcdFixity = GHC.Prefix, ..} =
  Just Prefix {..}
  where
    name = tcdLName
    typeVariables =
      fmap mkTypeVariable . fromGenLocated <$> GHC.hsq_explicit tcdTyVars
mkNameAndTypeVariables GHC.ClassDecl { tcdFixity = GHC.Infix
                                     , tcdTyVars = GHC.HsQTvs {hsq_explicit = h:t:xs}
                                     , ..
                                     } = Just Infix {..}
  where
    left = mkTypeVariable <$> fromGenLocated h
    name = tcdLName
    right = mkTypeVariable <$> fromGenLocated t
    remains = fmap (fmap mkTypeVariable . fromGenLocated) xs
mkNameAndTypeVariables _ = Nothing
