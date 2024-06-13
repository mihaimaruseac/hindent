{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module HIndent.Ast.Expression.Application.Infix
  ( InfixApplication
  , mkInfixApplication
  ) where

import Control.Monad
import Data.Maybe
import qualified GHC.Types.Fixity as GHC
import qualified GHC.Types.SrcLoc as GHC
import {-# SOURCE #-} HIndent.Ast.Expression
import HIndent.Ast.NodeComments
import HIndent.Fixity
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
import qualified Language.Haskell.GhclibParserEx.GHC.Hs.Expr as GHC

data InfixApplication = InfixApplication
  { lhs :: GHC.LHsExpr GHC.GhcPs
  , op :: GHC.LHsExpr GHC.GhcPs
  , rhs :: GHC.LHsExpr GHC.GhcPs
  }

instance CommentExtraction InfixApplication where
  nodeComments InfixApplication {} = NodeComments [] [] []

instance Pretty InfixApplication where
  pretty' InfixApplication {..} = horizontal <-|> vertical
    where
      horizontal =
        spaced
          [ pretty $ fmap mkExpression lhs
          , pretty (InfixExpr op)
          , pretty $ fmap mkExpression rhs
          ]
      vertical =
        case findFixity op of
          GHC.Fixity _ _ GHC.InfixL -> leftAssoc
          GHC.Fixity _ _ GHC.InfixR -> rightAssoc
          GHC.Fixity _ _ GHC.InfixN -> noAssoc
      leftAssoc = prettyOps allOperantsAndOperatorsLeftAssoc
      rightAssoc = prettyOps allOperantsAndOperatorsRightAssoc
      noAssoc
        | GHC.L _ (GHC.OpApp _ _ o _) <- lhs
        , isSameAssoc o = leftAssoc
        | otherwise = rightAssoc
      prettyOps [l, o, GHC.L _ (GHC.HsDo _ (GHC.DoExpr m) xs)] = do
        spaced
          [ pretty $ fmap mkExpression l
          , pretty $ InfixExpr o
          , pretty $ QualifiedDo m Do
          ]
        newline
        indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
      prettyOps [l, o, GHC.L _ (GHC.HsDo _ (GHC.MDoExpr m) xs)] = do
        spaced
          [ pretty $ fmap mkExpression l
          , pretty $ InfixExpr o
          , pretty $ QualifiedDo m Mdo
          ]
        newline
        indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
      prettyOps [l, o, r@(GHC.L _ GHC.HsLam {})] = do
        spaced
          [ pretty $ fmap mkExpression l
          , pretty $ InfixExpr o
          , pretty $ fmap mkExpression r
          ]
      prettyOps [l, o, r@(GHC.L _ GHC.HsLamCase {})] = do
        spaced
          [ pretty $ fmap mkExpression l
          , pretty $ InfixExpr o
          , pretty $ fmap mkExpression r
          ]
      prettyOps (l:xs) = do
        pretty $ fmap mkExpression l
        newline
        indentedBlock $ f xs
        where
          f (o:r:rems) = do
            (pretty (InfixExpr o) >> space) |=> pretty (fmap mkExpression r)
            unless (null rems) $ do
              newline
              f rems
          f _ =
            error
              "The number of the sum of operants and operators should be odd."
      prettyOps _ = error "Too short list."
      findFixity o =
        fromMaybe GHC.defaultFixity $ lookup (GHC.varToStr o) fixities
      allOperantsAndOperatorsLeftAssoc = reverse $ rhs : op : collect lhs
        where
          collect :: GHC.LHsExpr GHC.GhcPs -> [GHC.LHsExpr GHC.GhcPs]
          collect (GHC.L _ (GHC.OpApp _ l o r))
            | isSameAssoc o = r : o : collect l
          collect x = [x]
      allOperantsAndOperatorsRightAssoc = lhs : op : collect rhs
        where
          collect :: GHC.LHsExpr GHC.GhcPs -> [GHC.LHsExpr GHC.GhcPs]
          collect (GHC.L _ (GHC.OpApp _ l o r))
            | isSameAssoc o = l : o : collect r
          collect x = [x]
      isSameAssoc (findFixity -> GHC.Fixity _ lv d) = lv == level && d == dir
      GHC.Fixity _ level dir = findFixity op

mkInfixApplication ::
     GHC.LHsExpr GHC.GhcPs
  -> GHC.LHsExpr GHC.GhcPs
  -> GHC.LHsExpr GHC.GhcPs
  -> InfixApplication
mkInfixApplication = InfixApplication
