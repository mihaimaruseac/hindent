{-# LANGUAGE CPP #-}

module HIndent.Ast.Expression.Splice
  ( Splice
  , mkSplice
  , mkTypedSplice
  ) where

import qualified GHC.Data.FastString as GHC
import {-# SOURCE #-} HIndent.Ast.Expression (Expression, mkExpression)
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments (WithComments, fromGenLocated)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
import qualified GHC.Types.SrcLoc as GHC
#endif
data Splice
  = Typed (WithComments Expression)
  | UntypedDollar (WithComments Expression)
  | UntypedBare (WithComments Expression)
  | QuasiQuote PrefixName GHC.FastString

instance CommentExtraction Splice where
  nodeComments (Typed expr) = nodeComments expr
  nodeComments (UntypedDollar expr) = nodeComments expr
  nodeComments (UntypedBare expr) = nodeComments expr
  nodeComments QuasiQuote {} = NodeComments [] [] []

instance Pretty Splice where
  pretty' (Typed x) = string "$$" >> pretty x
  pretty' (UntypedDollar x) = string "$" >> pretty x
  pretty' (UntypedBare x) = pretty x
  pretty' (QuasiQuote l r) =
    brackets $ do
      pretty l
      wrapWithBars
        $ indentedWithFixedLevel 0
        $ sequence_
        $ printers [] ""
        $ GHC.unpackFS r
    where
      printers ps s [] = reverse (string (reverse s) : ps)
      printers ps s ('\n':xs) =
        printers (newline : string (reverse s) : ps) "" xs
      printers ps s (x:xs) = printers ps (x : s) xs
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkSplice :: GHC.HsUntypedSplice GHC.GhcPs -> Splice
mkSplice (GHC.HsUntypedSpliceExpr anns x)
  | hasDollarToken anns = UntypedDollar $ mkExpression <$> fromGenLocated x
  | otherwise = UntypedBare $ mkExpression <$> fromGenLocated x
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkSplice (GHC.HsQuasiQuote _ l (GHC.L _ r)) =
  QuasiQuote (mkPrefixName (GHC.unLoc l)) r
#else
mkSplice (GHC.HsQuasiQuote _ l (GHC.L _ r)) = QuasiQuote (mkPrefixName l) r
#endif
#else
mkSplice :: GHC.HsSplice GHC.GhcPs -> Splice
mkSplice (GHC.HsTypedSplice _ _ _ body) =
  Typed $ mkExpression <$> fromGenLocated body
mkSplice (GHC.HsUntypedSplice _ GHC.DollarSplice _ body) =
  UntypedDollar $ mkExpression <$> fromGenLocated body
mkSplice (GHC.HsUntypedSplice _ GHC.BareSplice _ body) =
  UntypedBare $ mkExpression <$> fromGenLocated body
mkSplice (GHC.HsQuasiQuote _ _ l _ r) = QuasiQuote (mkPrefixName l) r
mkSplice GHC.HsSpliced {} = error "This AST node should never appear."
#endif

#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
hasDollarToken :: GHC.XUntypedSpliceExpr GHC.GhcPs -> Bool
hasDollarToken (GHC.EpTok _) = True
hasDollarToken GHC.NoEpTok = False
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
hasDollarToken :: GHC.XUntypedSpliceExpr GHC.GhcPs -> Bool
hasDollarToken anns = any isDollarAnn anns
  where
    isDollarAnn (GHC.AddEpAnn GHC.AnnDollar _) = True
    isDollarAnn _ = False
#else
hasDollarToken :: GHC.XUntypedSpliceExpr GHC.GhcPs -> Bool
hasDollarToken (GHC.EpAnn _ anns _) = any isDollarAnn anns
  where
    isDollarAnn (GHC.AddEpAnn GHC.AnnDollar _) = True
    isDollarAnn _ = False
hasDollarToken GHC.EpAnnNotUsed = False
#endif
mkTypedSplice :: GHC.LHsExpr GHC.GhcPs -> Splice
mkTypedSplice = Typed . fmap mkExpression . fromGenLocated
