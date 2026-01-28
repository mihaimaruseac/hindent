{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Signature
  ( Signature
  , mkSignature
  ) where

import qualified GHC.Types.Basic as GHC
import HIndent.Applicative
import HIndent.Ast.Declaration.Signature.BooleanFormula
import HIndent.Ast.Declaration.Signature.Fixity
import HIndent.Ast.Declaration.Signature.Inline.Phase
import HIndent.Ast.Declaration.Signature.Inline.Spec
import {-# SOURCE #-} HIndent.Ast.Expression (Expression)
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
import {-# SOURCE #-} HIndent.Ast.Expression (mkExpression)
#endif
import HIndent.Ast.Name.Infix
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.Type (DeclSigType, Type, mkDeclSigType, mkTypeFromHsSigType)
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

-- We want to use the same name for `parameters` and `signature`, but GHC
-- doesn't allow it.
data Signature
  = Type
      { names :: [WithComments PrefixName]
      , parameters :: WithComments DeclSigType
      }
  | Pattern
      { names :: [WithComments PrefixName]
      , signature :: WithComments Type
      }
  | DefaultClassMethod
      { names :: [WithComments PrefixName]
      , methodSig :: WithComments DeclSigType
      }
  | ClassMethod
      { names :: [WithComments PrefixName]
      , methodSig :: WithComments DeclSigType
      }
  | Fixity
      { opNames :: [WithComments InfixName] -- Using `names` causes a type conflict.
      , fixity :: Fixity
      }
  | Inline
      { name :: WithComments PrefixName
      , spec :: InlineSpec
      , phase :: Maybe InlinePhase
      }
  | Specialise
      { name :: WithComments PrefixName
      , sigs :: [WithComments Type]
      , parentheses :: Bool
      }
  | SpecialiseExpr
      { expression :: WithComments Expression
      }
  | SpecialiseInstance (WithComments Type)
  | Minimal (WithComments BooleanFormula)
  | Scc (WithComments PrefixName)
  | Complete (WithComments [WithComments PrefixName])

instance CommentExtraction Signature where
  nodeComments Type {} = NodeComments [] [] []
  nodeComments Pattern {} = NodeComments [] [] []
  nodeComments DefaultClassMethod {} = NodeComments [] [] []
  nodeComments ClassMethod {} = NodeComments [] [] []
  nodeComments Fixity {} = NodeComments [] [] []
  nodeComments Inline {} = NodeComments [] [] []
  nodeComments Specialise {} = NodeComments [] [] []
  nodeComments SpecialiseExpr {} = NodeComments [] [] []
  nodeComments SpecialiseInstance {} = NodeComments [] [] []
  nodeComments Minimal {} = NodeComments [] [] []
  nodeComments Scc {} = NodeComments [] [] []
  nodeComments Complete {} = NodeComments [] [] []

instance Pretty Signature where
  pretty' Type {..} = do
    printFunName
    string " ::"
    horizontal <-|> vertical
    where
      horizontal = do
        space
        pretty parameters
      vertical = do
        headLen <- printerLength printFunName
        indentSpaces <- getIndentSpaces
        if headLen < indentSpaces
          then space |=> pretty parameters
          else do
            newline
            indentedBlock $ indentedWithSpace 3 $ pretty parameters
      printFunName = hCommaSep $ fmap pretty names
  pretty' Pattern {..} =
    spaced
      [ string "pattern"
      , hCommaSep $ fmap pretty names
      , string "::"
      , pretty signature
      ]
  pretty' DefaultClassMethod {..} = do
    string "default "
    hCommaSep $ fmap pretty names
    string " ::"
    hor <-|> ver
    where
      hor = space >> pretty methodSig
      ver = do
        newline
        indentedBlock $ indentedWithSpace 3 $ pretty methodSig
  pretty' ClassMethod {..} = do
    hCommaSep $ fmap pretty names
    string " ::"
    hor <-|> ver
    where
      hor = space >> pretty methodSig
      ver = do
        newline
        indentedBlock $ indentedWithSpace 3 $ pretty methodSig
  pretty' Fixity {..} = spaced [pretty fixity, hCommaSep $ fmap pretty opNames]
  pretty' Inline {..} = do
    string "{-# "
    pretty spec
    whenJust phase $ \x -> space >> pretty x
    space
    pretty name
    string " #-}"
  pretty' Specialise {..} =
    spaced [string "{-# SPECIALISE", specialiseBody, string "#-}"]
    where
      sigDoc = hCommaSep $ fmap pretty sigs
      specialiseBody =
        if parentheses
          then parens $ spaced [pretty name, string "::", sigDoc]
          else spaced [pretty name, string "::", sigDoc]
  pretty' SpecialiseExpr {..} =
    spaced [string "{-# SPECIALISE", pretty expression, string "#-}"]
  pretty' (SpecialiseInstance sig) =
    spaced [string "{-# SPECIALISE instance", pretty sig, string "#-}"]
  pretty' (Minimal xs) =
    string "{-# MINIMAL " |=> do
      pretty xs
      string " #-}"
  pretty' (Scc name) = spaced [string "{-# SCC", pretty name, string "#-}"]
  pretty' (Complete names) =
    spaced
      [ string "{-# COMPLETE"
      , prettyWith names (hCommaSep . fmap pretty)
      , string "#-}"
      ]

mkSignature :: GHC.Sig GHC.GhcPs -> Signature
mkSignature (GHC.TypeSig _ ns GHC.HsWC { hswc_ext = GHC.NoExtField
                                       , GHC.hswc_body = params
                                       }) = Type {..}
  where
    names = fmap (fromGenLocated . fmap mkPrefixName) ns
    parameters = flattenComments $ mkDeclSigType <$> fromGenLocated params
mkSignature (GHC.PatSynSig _ ns s) = Pattern {..}
  where
    names = fmap (fromGenLocated . fmap mkPrefixName) ns
    signature = flattenComments $ mkTypeFromHsSigType <$> fromGenLocated s
mkSignature (GHC.ClassOpSig _ True ns s) = DefaultClassMethod {..}
  where
    names = fmap (fromGenLocated . fmap mkPrefixName) ns
    methodSig = flattenComments $ mkDeclSigType <$> fromGenLocated s
mkSignature (GHC.ClassOpSig _ False ns s) = ClassMethod {..}
  where
    names = fmap (fromGenLocated . fmap mkPrefixName) ns
    methodSig = flattenComments $ mkDeclSigType <$> fromGenLocated s
mkSignature (GHC.FixSig _ (GHC.FixitySig _ ops fy)) = Fixity {..}
  where
    fixity = mkFixity fy
    opNames = fmap (fromGenLocated . fmap mkInfixName) ops
mkSignature (GHC.InlineSig _ n GHC.InlinePragma {..}) = Inline {..}
  where
    name = fromGenLocated $ fmap mkPrefixName n
    spec = mkInlineSpec inl_inline
    phase = mkInlinePhase inl_act
mkSignature (GHC.SpecSig _ n s _) = Specialise {..}
  where
    name = fromGenLocated $ fmap mkPrefixName n
    sigs = flattenComments . fmap mkTypeFromHsSigType . fromGenLocated <$> s
    parentheses = False
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkSignature (GHC.SpecSigE _ _ expr _) = SpecialiseExpr {..}
  where
    expression = mkExpression <$> fromGenLocated expr
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkSignature (GHC.SCCFunSig _ n _) = Scc name
  where
    name = fromGenLocated $ fmap mkPrefixName n
mkSignature (GHC.CompleteMatchSig _ ns _) = Complete names
  where
    names = mkWithComments $ fmap (fromGenLocated . fmap mkPrefixName) ns
#elif MIN_VERSION_ghc_lib_parser(9, 6, 0)
mkSignature (GHC.SCCFunSig _ n _) = Scc name
  where
    name = fromGenLocated $ fmap mkPrefixName n
mkSignature (GHC.CompleteMatchSig _ ns _) = Complete names
  where
    names = fromGenLocated $ fmap (fmap (fromGenLocated . fmap mkPrefixName)) ns
#elif MIN_VERSION_ghc_lib_parser(9, 4, 0)
mkSignature (GHC.SCCFunSig _ _ name _) =
  Scc $ fromGenLocated $ fmap mkPrefixName name
mkSignature (GHC.CompleteMatchSig _ _ names _) =
  Complete
    $ fromGenLocated
    $ fmap (fmap (fromGenLocated . fmap mkPrefixName)) names
#else
mkSignature (GHC.SCCFunSig _ _ name _) =
  Scc $ fromGenLocated $ fmap mkPrefixName name
mkSignature (GHC.CompleteMatchSig _ _ names _) =
  Complete
    $ fromGenLocated
    $ fmap (fmap (fromGenLocated . fmap mkPrefixName)) names
#endif
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
mkSignature (GHC.SpecInstSig _ sig) =
  SpecialiseInstance
    $ flattenComments
    $ mkTypeFromHsSigType <$> fromGenLocated sig
mkSignature (GHC.MinimalSig _ xs) =
  Minimal $ mkBooleanFormula <$> fromGenLocated xs
#else
mkSignature (GHC.SpecInstSig _ _ sig) = SpecialiseInstance sig
mkSignature (GHC.MinimalSig _ _ xs) =
  Minimal $ mkBooleanFormula <$> fromGenLocated xs
mkSignature GHC.IdSig {} =
  error "`ghc-lib-parser` never generates this AST node."
#endif
