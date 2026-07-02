{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Class.NameAndTypeVariables
  ( NameAndTypeVariables
  , mkNameAndTypeVariables
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

data NameAndTypeVariables
  = Prefix
      { pName :: WithComments PrefixName -- Using `name` in both `Prefix` and `Infix` causes a type conflict.
      , typeVariables :: [WithComments TypeVariable]
      }
  | Infix
      { left :: WithComments TypeVariable
      , iName :: WithComments InfixName
      , right :: WithComments TypeVariable
      , remains :: [WithComments TypeVariable]
      }

instance CommentExtraction NameAndTypeVariables where
  nodeComments Prefix {} = NodeComments [] [] []
  nodeComments Infix {} = NodeComments [] [] []

instance Pretty NameAndTypeVariables where
  pretty Prefix {..} = spaced $ pretty pName : fmap pretty typeVariables
  pretty Infix {..} = do
    parens $ spaced [pretty left, pretty iName, pretty right]
    spacePrefixed $ fmap pretty remains

mkNameAndTypeVariables :: GHC.TyClDecl GHC.GhcPs -> Maybe NameAndTypeVariables
mkNameAndTypeVariables GHC.ClassDecl {tcdFixity = GHC.Prefix, ..} =
  Just Prefix {..}
  where
    pName = mkWithCommentsFromGenLocated $ fmap mkPrefixName tcdLName
    typeVariables =
      fmap mkTypeVariable . mkWithCommentsFromGenLocated
        <$> GHC.hsq_explicit tcdTyVars
mkNameAndTypeVariables GHC.ClassDecl { tcdFixity = GHC.Infix
                                     , tcdTyVars = GHC.HsQTvs {hsq_explicit = h:t:xs}
                                     , ..
                                     } = Just Infix {..}
  where
    left = mkTypeVariable <$> mkWithCommentsFromGenLocated h
    iName = mkWithCommentsFromGenLocated $ fmap mkInfixName tcdLName
    right = mkTypeVariable <$> mkWithCommentsFromGenLocated t
    remains = fmap (fmap mkTypeVariable . mkWithCommentsFromGenLocated) xs
mkNameAndTypeVariables _ = Nothing
