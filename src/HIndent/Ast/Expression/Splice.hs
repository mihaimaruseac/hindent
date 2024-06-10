{-# LANGUAGE CPP #-}

module HIndent.Ast.Expression.Splice
  ( Splice
  , mkSplice
  ) where

import qualified GHC.Data.FastString as GHC
import qualified GHC.Types.Name.Reader as GHC
import {-# SOURCE #-} HIndent.Ast.Expression
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
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
  | QuasiQuote GHC.RdrName GHC.FastString

instance CommentExtraction Splice where
  nodeComments Typed {} = NodeComments [] [] []
  nodeComments UntypedDollar {} = NodeComments [] [] []
  nodeComments UntypedBare {} = NodeComments [] [] []
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
mkSplice (GHC.HsUntypedSpliceExpr _ x) =
  UntypedDollar $ fmap mkExpression $ fromGenLocated x
mkSplice (GHC.HsQuasiQuote _ l (GHC.L _ r)) = QuasiQuote l r
#else
mkSplice :: GHC.HsSplice GHC.GhcPs -> Splice
mkSplice (GHC.HsTypedSplice _ _ _ body) =
  Typed $ fmap mkExpression $ fromGenLocated body
mkSplice (GHC.HsUntypedSplice _ GHC.DollarSplice _ body) =
  UntypedDollar $ fmap mkExpression $ fromGenLocated body
mkSplice (GHC.HsUntypedSplice _ GHC.BareSplice _ body) =
  UntypedBare $ fmap mkExpression $ fromGenLocated body
mkSplice (GHC.HsQuasiQuote _ _ l _ r) = QuasiQuote l r
mkSplice GHC.HsSpliced {} = error "This AST node should never appear."
#endif
