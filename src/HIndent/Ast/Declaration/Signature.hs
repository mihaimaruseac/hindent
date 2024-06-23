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
import HIndent.Ast.Name.Infix
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types

-- We want to use the same name for `parameters` and `signature`, but GHC
-- doesn't allow it.
data Signature
  = Type
      { names :: [GHC.LIdP GHC.GhcPs]
      , parameters :: GHC.LHsSigWcType GHC.GhcPs
      }
  | Pattern
      { names :: [GHC.LIdP GHC.GhcPs]
      , signature :: GHC.LHsSigType GHC.GhcPs
      }
  | DefaultClassMethod
      { names :: [GHC.LIdP GHC.GhcPs]
      , signature :: GHC.LHsSigType GHC.GhcPs
      }
  | ClassMethod
      { names :: [GHC.LIdP GHC.GhcPs]
      , signature :: GHC.LHsSigType GHC.GhcPs
      }
  | Fixity
      { names :: [GHC.LIdP GHC.GhcPs]
      , fixity :: Fixity
      }
  | Inline
      { name :: GHC.LIdP GHC.GhcPs
      , spec :: InlineSpec
      , phase :: Maybe InlinePhase
      }
  | Specialise
      { name :: GHC.LIdP GHC.GhcPs
      , sigs :: [GHC.LHsSigType GHC.GhcPs]
      }
  | SpecialiseInstance (GHC.LHsSigType GHC.GhcPs)
  | Minimal (WithComments BooleanFormula)
  | Scc (GHC.LIdP GHC.GhcPs)
  | Complete (GHC.XRec GHC.GhcPs [GHC.LIdP GHC.GhcPs])

instance CommentExtraction Signature where
  nodeComments Type {} = NodeComments [] [] []
  nodeComments Pattern {} = NodeComments [] [] []
  nodeComments DefaultClassMethod {} = NodeComments [] [] []
  nodeComments ClassMethod {} = NodeComments [] [] []
  nodeComments Fixity {} = NodeComments [] [] []
  nodeComments Inline {} = NodeComments [] [] []
  nodeComments Specialise {} = NodeComments [] [] []
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
        pretty $ HsSigTypeInsideDeclSig <$> GHC.hswc_body parameters
      vertical = do
        headLen <- printerLength printFunName
        indentSpaces <- getIndentSpaces
        if headLen < indentSpaces
          then space
                 |=> pretty
                       (HsSigTypeInsideDeclSig <$> GHC.hswc_body parameters)
          else do
            newline
            indentedBlock
              $ indentedWithSpace 3
              $ pretty
              $ HsSigTypeInsideDeclSig <$> GHC.hswc_body parameters
      printFunName = hCommaSep $ fmap pretty names
  pretty' Pattern {..} =
    spaced
      [ string "pattern"
      , hCommaSep $ fmap pretty names
      , string "::"
      , pretty signature
      ]
  pretty' DefaultClassMethod {..} =
    spaced
      [ string "default"
      , hCommaSep $ fmap pretty names
      , string "::"
      , printCommentsAnd signature pretty
      ]
  pretty' ClassMethod {..} = do
    hCommaSep $ fmap pretty names
    string " ::"
    hor <-|> ver
    where
      hor =
        space >> printCommentsAnd signature (pretty . HsSigTypeInsideDeclSig)
      ver = do
        newline
        indentedBlock
          $ indentedWithSpace 3
          $ printCommentsAnd signature (pretty . HsSigTypeInsideDeclSig)
  pretty' Fixity {..} =
    spaced [pretty fixity, hCommaSep $ fmap (pretty . fmap mkInfixName) names]
  pretty' Inline {..} = do
    string "{-# "
    pretty spec
    whenJust phase $ \x -> space >> pretty x
    space
    pretty name
    string " #-}"
  pretty' Specialise {..} =
    spaced
      [ string "{-# SPECIALISE"
      , pretty name
      , string "::"
      , hCommaSep $ fmap pretty sigs
      , string "#-}"
      ]
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
      , printCommentsAnd names (hCommaSep . fmap pretty)
      , string "#-}"
      ]

mkSignature :: GHC.Sig GHC.GhcPs -> Signature
mkSignature (GHC.TypeSig _ names parameters) = Type {..}
mkSignature (GHC.PatSynSig _ names signature) = Pattern {..}
mkSignature (GHC.ClassOpSig _ True names signature) = DefaultClassMethod {..}
mkSignature (GHC.ClassOpSig _ False names signature) = ClassMethod {..}
mkSignature (GHC.FixSig _ (GHC.FixitySig _ names fixity)) =
  Fixity {fixity = mkFixity fixity, ..}
mkSignature (GHC.InlineSig _ name GHC.InlinePragma {..}) = Inline {..}
  where
    spec = mkInlineSpec inl_inline
    phase = mkInlinePhase inl_act
mkSignature (GHC.SpecSig _ name sigs _) = Specialise {..}
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
mkSignature (GHC.SpecInstSig _ sig) = SpecialiseInstance sig
mkSignature (GHC.MinimalSig _ xs) =
  Minimal $ mkBooleanFormula <$> fromGenLocated xs
mkSignature (GHC.SCCFunSig _ name _) = Scc name
mkSignature (GHC.CompleteMatchSig _ names _) = Complete names
#else
mkSignature (GHC.SpecInstSig _ _ sig) = SpecialiseInstance sig
mkSignature (GHC.MinimalSig _ _ xs) =
  Minimal $ mkBooleanFormula <$> fromGenLocated xs
mkSignature (GHC.SCCFunSig _ _ name _) = Scc name
mkSignature (GHC.CompleteMatchSig _ _ names _) = Complete names
mkSignature GHC.IdSig {} =
  error "`ghc-lib-parser` never generates this AST node."
#endif
