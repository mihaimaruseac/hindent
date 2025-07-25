{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Pattern
  ( Pattern
  , PatInsidePatDecl
  , mkPattern
  , mkPatInsidePatDecl
  ) where

import Control.Monad
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified GHC.Types.Basic as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Expression.Splice
import HIndent.Ast.Name.Infix hiding (unlessSpecialOp)
import qualified HIndent.Ast.Name.Infix as InfixName
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments hiding (fromEpAnn)
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments (CommentExtraction(..))
import HIndent.Pretty.Types (RecConPat(..))

data Pattern
  = WildCard
  | Variable (WithComments PrefixName)
  | Lazy (WithComments Pattern)
  | As
      { name :: WithComments PrefixName
      , pat :: WithComments Pattern
      }
  | Parenthesized (WithComments Pattern)
  | Bang (WithComments Pattern)
  | List [WithComments Pattern]
  | Tuple
      { boxity :: GHC.Boxity
      , patterns :: [WithComments Pattern]
      }
  | Sum
      { pat :: WithComments Pattern
      , position :: Int
      , arity :: Int
      }
  | PrefixConstructor
      { name :: WithComments PrefixName
      , patterns :: [WithComments Pattern]
      }
  | InfixConstructor
      { left :: WithComments Pattern
      , operator :: WithComments InfixName
      , right :: WithComments Pattern
      }
  | RecordConstructor
      { name :: WithComments PrefixName
      , fields :: GHC.HsRecFields GHC.GhcPs (GHC.LPat GHC.GhcPs)
      }
  | View
      { expression :: GHC.LHsExpr GHC.GhcPs
      , pat :: WithComments Pattern
      }
  | Splice (WithComments Splice)
  | Literal (GHC.HsLit GHC.GhcPs)
  | Overloaded (GHC.HsOverLit GHC.GhcPs)
  | NPlusK
      { n :: WithComments PrefixName
      , k :: GHC.HsOverLit GHC.GhcPs
      }
  | Signature
      { pat :: WithComments Pattern
      , sig :: WithComments Type
      }
  | Or (NonEmpty (WithComments Pattern))

instance CommentExtraction Pattern where
  nodeComments _ = NodeComments [] [] []

instance Pretty Pattern where
  pretty' WildCard = string "_"
  pretty' (Variable name) = pretty name
  pretty' (Lazy pat) = string "~" >> pretty pat
  pretty' As {..} = pretty name >> string "@" >> pretty pat
  pretty' (Parenthesized pat) = parens $ pretty pat
  pretty' (Bang pat) = string "!" >> pretty pat
  pretty' (List pats) = hList $ pretty <$> pats
  pretty' Tuple {boxity = GHC.Boxed, ..} = hTuple $ pretty <$> patterns
  pretty' Tuple {boxity = GHC.Unboxed, ..} = hUnboxedTuple $ pretty <$> patterns
  pretty' Sum {..} = do
    string "(#"
    forM_ [1 .. arity] $ \idx -> do
      if idx == position
        then string " " >> pretty pat >> string " "
        else string " "
      when (idx < arity) $ string "|"
    string "#)"
  pretty' PrefixConstructor {..} = do
    pretty name
    spacePrefixed $ pretty <$> patterns
  pretty' InfixConstructor {..} = do
    pretty left
    InfixName.unlessSpecialOp (getNode operator) space
    pretty operator
    InfixName.unlessSpecialOp (getNode operator) space
    pretty right
  pretty' RecordConstructor {..} =
    (pretty name >> space) |=> pretty (RecConPat fields)
  pretty' View {..} = spaced [pretty expression, string "->", pretty pat]
  pretty' (Splice splice) = pretty splice
  pretty' (Literal lit) = pretty lit
  pretty' (Overloaded lit) = pretty lit
  pretty' NPlusK {..} = pretty n >> string "+" >> pretty k
  pretty' Signature {..} = spaced [pretty pat, string "::", pretty sig]
  pretty' (Or pats) = inter (string "; ") $ pretty <$> NE.toList pats

mkPattern :: GHC.Pat GHC.GhcPs -> Pattern
mkPattern GHC.WildPat {} = WildCard
mkPattern (GHC.VarPat _ x) = Variable $ mkPrefixName <$> fromGenLocated x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkPattern GHC.EmbTyPat {} = notGeneratedByParser
mkPattern GHC.InvisPat {} = notGeneratedByParser
#endif
mkPattern (GHC.LazyPat _ x) = Lazy $ mkPattern <$> fromGenLocated x
#if MIN_VERSION_ghc_lib_parser(9, 6, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkPattern (GHC.AsPat _ a _ b) =
  As
    { name = mkPrefixName <$> fromGenLocated a
    , pat = mkPattern <$> fromGenLocated b
    }
#else
mkPattern (GHC.AsPat _ a b) =
  As
    { name = mkPrefixName <$> fromGenLocated a
    , pat = mkPattern <$> fromGenLocated b
    }
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkPattern (GHC.ParPat _ _ inner _) =
  Parenthesized $ mkPattern <$> fromGenLocated inner
#else
mkPattern (GHC.ParPat _ inner) =
  Parenthesized $ mkPattern <$> fromGenLocated inner
#endif
mkPattern (GHC.BangPat _ x) = Bang $ mkPattern <$> fromGenLocated x
mkPattern (GHC.ListPat _ xs) = List $ fmap (fmap mkPattern . fromGenLocated) xs
mkPattern (GHC.TuplePat _ pats boxity) =
  Tuple
    {boxity = boxity, patterns = fmap (fmap mkPattern . fromGenLocated) pats}
mkPattern (GHC.SumPat _ x position numElem) =
  Sum
    {pat = mkPattern <$> fromGenLocated x, position = position, arity = numElem}
mkPattern GHC.ConPat {..} =
  case pat_args of
    GHC.PrefixCon _ as ->
      PrefixConstructor
        { name = mkPrefixName <$> fromGenLocated pat_con
        , patterns = fmap (fmap mkPattern . fromGenLocated) as
        }
    GHC.RecCon rec ->
      RecordConstructor
        {name = mkPrefixName <$> fromGenLocated pat_con, fields = rec}
    GHC.InfixCon a b ->
      InfixConstructor
        { left = mkPattern <$> fromGenLocated a
        , operator = mkInfixName <$> fromGenLocated pat_con
        , right = mkPattern <$> fromGenLocated b
        }
mkPattern (GHC.ViewPat _ l r) =
  View {expression = l, pat = mkPattern <$> fromGenLocated r}
mkPattern (GHC.SplicePat _ x) = Splice $ mkWithComments $ mkSplice x
mkPattern (GHC.LitPat _ x) = Literal x
mkPattern (GHC.NPat _ x _ _) = Overloaded $ GHC.unLoc x
mkPattern (GHC.NPlusKPat _ n k _ _ _) =
  NPlusK {n = mkPrefixName <$> fromGenLocated n, k = GHC.unLoc k}
mkPattern (GHC.SigPat _ l r) =
  Signature
    { pat = mkPattern <$> fromGenLocated l
    , sig =
        flattenComments
          $ fmap mkType
              <$> fromEpAnn (GHC.hsps_ext r) (fromGenLocated $ GHC.hsps_body r)
    }
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkPattern (GHC.OrPat _ pats) = Or $ fmap (fmap mkPattern . fromGenLocated) pats
#endif
newtype PatInsidePatDecl =
  PatInsidePatDecl Pattern

instance CommentExtraction PatInsidePatDecl where
  nodeComments (PatInsidePatDecl p) = nodeComments p

instance Pretty PatInsidePatDecl where
  pretty' (PatInsidePatDecl InfixConstructor {..}) =
    spaced [pretty left, pretty operator, pretty right]
  pretty' (PatInsidePatDecl p) = pretty p

mkPatInsidePatDecl :: GHC.Pat GHC.GhcPs -> PatInsidePatDecl
mkPatInsidePatDecl = PatInsidePatDecl . mkPattern
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
notGeneratedByParser :: a
notGeneratedByParser = error "This AST node is not generated by the parser."
#endif
