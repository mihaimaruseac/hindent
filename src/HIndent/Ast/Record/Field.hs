{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Record.Field
  ( ExprField
  , PatField
  , mkExprField
  , mkPatField
  ) where

import HIndent.Applicative (whenJust)
import {-# SOURCE #-} HIndent.Ast.Expression (Expression, mkExpression)
import HIndent.Ast.Name.RecordField (FieldName, mkFieldNameFromFieldOcc)
import HIndent.Ast.NodeComments (NodeComments(..))
import {-# SOURCE #-} HIndent.Ast.Pattern (Pattern, mkPattern)
import HIndent.Ast.WithComments (WithComments, fromGenLocated)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
  ( (<-|>)
  , indentedBlock
  , newline
  , space
  , string
  )
import HIndent.Pretty.NodeComments (CommentExtraction(..))

type ExprField = Field Expression

type PatField = Field Pattern

data Field rhs = Field
  { name :: WithComments FieldName
  , value :: Maybe (WithComments rhs)
  }

instance CommentExtraction (Field rhs) where
  nodeComments Field {} = NodeComments [] [] []

instance Pretty ExprField where
  pretty' Field {..} = do
    pretty name
    whenJust value $ \val -> do
      string " ="
      (space >> pretty val) <-|> (newline >> indentedBlock (pretty val))

instance Pretty PatField where
  pretty' Field {..} = do
    pretty name
    whenJust value $ \val -> do
      string " = "
      pretty val
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkExprField ::
     GHC.HsFieldBind (GHC.LFieldOcc GHC.GhcPs) (GHC.LHsExpr GHC.GhcPs)
  -> ExprField
mkExprField GHC.HsFieldBind {..} =
  Field
    { name = mkFieldNameFromFieldOcc <$> fromGenLocated hfbLHS
    , value =
        if hfbPun
          then Nothing
          else Just (mkExpression <$> fromGenLocated hfbRHS)
    }
#else
mkExprField ::
     GHC.HsRecField' (GHC.FieldOcc GHC.GhcPs) (GHC.LHsExpr GHC.GhcPs)
  -> ExprField
mkExprField GHC.HsRecField {..} =
  Field
    { name = mkFieldNameFromFieldOcc <$> fromGenLocated hsRecFieldLbl
    , value =
        if hsRecPun
          then Nothing
          else Just (mkExpression <$> fromGenLocated hsRecFieldArg)
    }
#endif

#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkPatField ::
     GHC.HsFieldBind (GHC.LFieldOcc GHC.GhcPs) (GHC.LPat GHC.GhcPs) -> PatField
mkPatField GHC.HsFieldBind {..} =
  Field
    { name = mkFieldNameFromFieldOcc <$> fromGenLocated hfbLHS
    , value =
        if hfbPun
          then Nothing
          else Just (mkPattern <$> fromGenLocated hfbRHS)
    }
#else
mkPatField ::
     GHC.HsRecField' (GHC.FieldOcc GHC.GhcPs) (GHC.LPat GHC.GhcPs) -> PatField
mkPatField GHC.HsRecField {..} =
  Field
    { name = mkFieldNameFromFieldOcc <$> fromGenLocated hsRecFieldLbl
    , value =
        if hsRecPun
          then Nothing
          else Just (mkPattern <$> fromGenLocated hsRecFieldArg)
    }
#endif
