{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Family.Data.Associated
  ( AssociatedDataFamilyInstance
  , mkAssociatedDataFamilyInstance
  ) where

import HIndent.Ast.Declaration.Data.Body
import HIndent.Ast.Declaration.Data.NewOrData
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.Type.Argument.Collection
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data AssociatedDataFamilyInstance = AssociatedDataFamilyInstance
  { newOrData :: NewOrData
  , name :: WithComments PrefixName
  , types :: TypeArgumentCollection
  , body :: DataBody
  }

instance CommentExtraction AssociatedDataFamilyInstance where
  nodeComments AssociatedDataFamilyInstance {} = NodeComments [] [] []

instance Pretty AssociatedDataFamilyInstance where
  pretty AssociatedDataFamilyInstance {..} = do
    lhs
    pretty body
    where
      lhs =
        spaced
          $ [pretty newOrData, pretty name]
              <> [pretty types | hasTypeArguments types]

mkAssociatedDataFamilyInstance ::
     GHC.DataFamInstDecl GHC.GhcPs -> AssociatedDataFamilyInstance
mkAssociatedDataFamilyInstance GHC.DataFamInstDecl {GHC.dfid_eqn = GHC.FamEqn {..}} =
  AssociatedDataFamilyInstance
    { newOrData = mkNewOrData feqn_rhs
    , name = mkWithCommentsFromGenLocated $ fmap mkPrefixName feqn_tycon
    , types = mkTypeArgumentCollection feqn_pats
    , body = mkDataBody feqn_rhs
    }
