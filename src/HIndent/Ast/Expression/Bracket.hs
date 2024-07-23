{-# LANGUAGE CPP #-}

module HIndent.Ast.Expression.Bracket
  ( Bracket
  , mkBracket
  ) where

import HIndent.Ast.Declaration
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Bracket
  = TypedExpression (GHC.LHsExpr GHC.GhcPs)
  | UntypedExpression (GHC.LHsExpr GHC.GhcPs)
  | Pattern (GHC.LPat GHC.GhcPs)
  | Declaration [WithComments Declaration]
  | Type (GHC.LHsType GHC.GhcPs)
  | Variable Bool (WithComments PrefixName)

instance CommentExtraction Bracket where
  nodeComments TypedExpression {} = NodeComments [] [] []
  nodeComments UntypedExpression {} = NodeComments [] [] []
  nodeComments Pattern {} = NodeComments [] [] []
  nodeComments Declaration {} = NodeComments [] [] []
  nodeComments Type {} = NodeComments [] [] []
  nodeComments Variable {} = NodeComments [] [] []

instance Pretty Bracket where
  pretty' (TypedExpression x) = typedBrackets $ pretty x
  pretty' (UntypedExpression x) = brackets $ wrapWithBars $ pretty x
  pretty' (Pattern x) = brackets $ string "p" >> wrapWithBars (pretty x)
  pretty' (Declaration decls) =
    brackets $ string "d| " |=> lined (fmap pretty decls) >> string " |"
  pretty' (Type x) = brackets $ string "t" >> wrapWithBars (pretty x)
  pretty' (Variable True var) = string "'" >> pretty var
  pretty' (Variable False var) = string "''" >> pretty var
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkBracket :: GHC.HsQuote GHC.GhcPs -> Bracket
#else
mkBracket :: GHC.HsBracket GHC.GhcPs -> Bracket
#endif
mkBracket (GHC.ExpBr _ x) = UntypedExpression x
mkBracket (GHC.PatBr _ x) = Pattern x
mkBracket (GHC.DecBrL _ x) =
  Declaration $ fmap (fmap mkDeclaration . fromGenLocated) x
mkBracket (GHC.TypBr _ x) = Type x
mkBracket (GHC.VarBr _ b x) = Variable b $ fromGenLocated $ fmap mkPrefixName x
mkBracket (GHC.DecBrG {}) = error "This AST node should never appear."
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkBracket (GHC.TExpBr _ x) = TypedExpression x
#endif
