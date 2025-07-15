{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module HIndent.Ast.Type
  ( Type
  , VerticalFuncType
  , DeclSigType
  , InstDeclType
  , mkType
  , mkVerticalFuncType
  , mkDeclSigType
  , mkInstDeclType
  ) where

import Control.Monad.RWS (gets)
import HIndent.Ast.Declaration.Data.Record.Field
import HIndent.Ast.Expression.Splice
import HIndent.Ast.Name.Infix
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.Type.Bang
import HIndent.Ast.Type.Literal
import HIndent.Ast.WithComments
import HIndent.Config
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
import HIndent.Printer

data Type
  = UniversalType
      (WithComments (GHC.HsForAllTelescope GHC.GhcPs))
      (WithComments Type)
  | ConstrainedType (WithComments Context) (WithComments Type)
  | Variable GHC.PromotionFlag (WithComments PrefixName)
  | Application (WithComments Type) (WithComments Type)
  | KindApplication (WithComments Type) (WithComments Type)
  | Function (WithComments Type) (WithComments Type)
  | List (WithComments Type)
  | Tuple GHC.HsTupleSort [WithComments Type]
  | Sum [WithComments Type]
  | InfixType (WithComments Type) (WithComments InfixName) (WithComments Type)
  | Parenthesized (WithComments Type)
  | ImplicitParameter GHC.HsIPName (WithComments Type)
  | Star
  | KindSig (WithComments Type) (WithComments Type)
  | Splice Splice
  | StrictType Bang (WithComments Type)
  | RecordType [WithComments RecordField]
  | PromotedList [WithComments Type]
  | PromotedTuple [WithComments Type]
  | Literal Literal
  | Wildcard

instance CommentExtraction Type where
  nodeComments (UniversalType {}) = NodeComments [] [] []
  nodeComments (ConstrainedType {}) = NodeComments [] [] []
  nodeComments (Variable {}) = NodeComments [] [] []
  nodeComments (Application {}) = NodeComments [] [] []
  nodeComments (KindApplication {}) = NodeComments [] [] []
  nodeComments (Function {}) = NodeComments [] [] []
  nodeComments (List (getNode -> List {})) = NodeComments [] [] []
  nodeComments (List x) = nodeComments x
  nodeComments (Tuple _ xs) = mconcat $ nodeComments <$> xs
  nodeComments (Sum xs) = mconcat $ nodeComments <$> xs
  nodeComments (InfixType {}) = NodeComments [] [] []
  nodeComments (Parenthesized (getNode -> Parenthesized {})) =
    NodeComments [] [] []
  nodeComments (Parenthesized x) = nodeComments x
  nodeComments (ImplicitParameter {}) = NodeComments [] [] []
  nodeComments Star = NodeComments [] [] []
  nodeComments (KindSig {}) = NodeComments [] [] []
  nodeComments (Splice {}) = NodeComments [] [] []
  nodeComments (StrictType {}) = NodeComments [] [] []
  nodeComments (RecordType xs) = mconcat $ nodeComments <$> xs
  nodeComments (PromotedList xs) = mconcat $ nodeComments <$> xs
  nodeComments (PromotedTuple xs) = mconcat $ nodeComments <$> xs
  nodeComments (Literal {}) = NodeComments [] [] []
  nodeComments Wildcard = NodeComments [] [] []

instance Pretty Type where
  pretty' (UniversalType tele body) =
    (pretty (getNode tele) >> space) |=> pretty body
  pretty' (ConstrainedType ctxt body) = hor <-|> ver
    where
      hor = spaced [pretty ctxt, string "=>", pretty body]
      ver = do
        pretty ctxt
        lined [string " =>", indentedBlock $ pretty body]
  pretty' (Variable GHC.NotPromoted x) = pretty x
  pretty' (Variable GHC.IsPromoted x) = string "'" >> pretty x
  pretty' (Application l r) = hor <-|> ver
    where
      hor = spaced [pretty l, pretty r]
      ver = verticalApp l r
      verticalApp left right = do
        case getNode left of
          Application l' r' ->
            verticalApp l' r' >> newline >> indentedBlock (pretty right)
          _ -> pretty left >> newline >> indentedBlock (pretty right)
  pretty' (KindApplication l r) = pretty l >> string " @" >> pretty r
  pretty' (Function a b) = (pretty a >> string " -> ") |=> pretty b
  pretty' (List xs) = brackets $ pretty xs
  pretty' (Tuple GHC.HsUnboxedTuple []) = string "(# #)"
  pretty' (Tuple GHC.HsBoxedOrConstraintTuple []) = string "()"
  pretty' (Tuple GHC.HsUnboxedTuple xs) = hvUnboxedTuple' $ fmap pretty xs
  pretty' (Tuple GHC.HsBoxedOrConstraintTuple xs) = hvTuple' $ fmap pretty xs
  pretty' (Sum xs) = hvUnboxedSum' $ fmap pretty xs
  pretty' (InfixType l op r) = do
    lineBreak <- gets (configLineBreaks . psConfig)
    if getInfixName (getNode op) `elem` lineBreak
      then do
        pretty l
        newline
        pretty op
        space
        pretty r
      else spaced [pretty l, pretty op, pretty r]
  pretty' (Parenthesized inside) = parens $ pretty inside
  pretty' (ImplicitParameter x ty) =
    spaced [string "?" >> pretty x, string "::", pretty ty]
  pretty' Star = string "*"
  pretty' (KindSig t k) = spaced [pretty t, string "::", pretty k]
  pretty' (Splice sp) = pretty sp
  pretty' (StrictType pack x) = pretty pack >> pretty x
  pretty' (RecordType xs) = hvFields $ fmap pretty xs
  pretty' (PromotedList []) = string "'[]"
  pretty' (PromotedList xs) = hvPromotedList $ fmap pretty xs
  pretty' (PromotedTuple xs) = hPromotedTuple $ fmap pretty xs
  pretty' (Literal x) = pretty x
  pretty' Wildcard = string "_"

mkType :: GHC.HsType GHC.GhcPs -> Type
mkType (GHC.HsForAllTy _ tele body) =
  UniversalType (mkWithComments tele) (mkType <$> fromGenLocated body)
mkType GHC.HsQualTy {..} =
  ConstrainedType
    (mkWithComments $ Context hst_ctxt)
    (mkType <$> fromGenLocated hst_body)
mkType (GHC.HsTyVar _ prom x) =
  Variable prom (mkPrefixName <$> fromGenLocated x)
mkType (GHC.HsAppTy _ l r) =
  Application (mkType <$> fromGenLocated l) (mkType <$> fromGenLocated r)
#if MIN_VERSION_ghc_lib_parser(9, 8, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkType (GHC.HsAppKindTy _ l _ r) =
  KindApplication (mkType <$> fromGenLocated l) (mkType <$> fromGenLocated r)
#else
mkType (GHC.HsAppKindTy _ l r) =
  KindApplication (mkType <$> fromGenLocated l) (mkType <$> fromGenLocated r)
#endif
mkType (GHC.HsFunTy _ _ a b) =
  Function (mkType <$> fromGenLocated a) (mkType <$> fromGenLocated b)
mkType (GHC.HsListTy _ xs) = List (mkType <$> fromGenLocated xs)
mkType (GHC.HsTupleTy _ sort xs) =
  Tuple sort (fmap (fmap mkType . fromGenLocated) xs)
mkType (GHC.HsSumTy _ xs) = Sum (fmap (fmap mkType . fromGenLocated) xs)
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkType (GHC.HsOpTy _ _ l op r) =
  InfixType
    (mkType <$> fromGenLocated l)
    (mkInfixName <$> fromGenLocated op)
    (mkType <$> fromGenLocated r)
#else
mkType (GHC.HsOpTy _ l op r) =
  InfixType
    (mkType <$> fromGenLocated l)
    (mkInfixName <$> fromGenLocated op)
    (mkType <$> fromGenLocated r)
#endif
mkType (GHC.HsParTy _ inside) = Parenthesized (mkType <$> fromGenLocated inside)
mkType (GHC.HsIParamTy _ x ty) =
  ImplicitParameter (getNode $ fromGenLocated x) (mkType <$> fromGenLocated ty)
mkType GHC.HsStarTy {} = Star
mkType (GHC.HsKindSig _ t k) =
  KindSig (mkType <$> fromGenLocated t) (mkType <$> fromGenLocated k)
mkType (GHC.HsSpliceTy _ sp) = Splice (mkSplice sp)
mkType GHC.HsDocTy {} = error "HsDocTy not supported"
mkType (GHC.HsBangTy _ pack x) =
  StrictType (mkBang pack) (mkType <$> fromGenLocated x)
mkType (GHC.HsRecTy _ xs) =
  RecordType (fmap (fromGenLocated . fmap mkRecordField) xs)
mkType (GHC.HsExplicitListTy _ _ xs) =
  PromotedList (fmap (fmap mkType . fromGenLocated) xs)
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkType (GHC.HsExplicitTupleTy _ _ xs) =
  PromotedTuple (fmap (fmap mkType . fromGenLocated) xs)
#else
mkType (GHC.HsExplicitTupleTy _ xs) =
  PromotedTuple (fmap (fmap mkType . fromGenLocated) xs)
#endif
mkType (GHC.HsTyLit _ x) = Literal (mkLiteral x)
mkType GHC.HsWildCardTy {} = Wildcard
mkType GHC.XHsType {} = error "XHsType not generated by parser"

newtype VerticalFuncType =
  VerticalFuncType Type

instance CommentExtraction VerticalFuncType where
  nodeComments (VerticalFuncType t) = nodeComments t

instance Pretty VerticalFuncType where
  pretty' (VerticalFuncType (Function a b)) = do
    pretty $ fmap mkVerticalFuncType a
    newline
    prefixed "-> " $ pretty $ fmap mkVerticalFuncType b
  pretty' (VerticalFuncType t) = pretty t

mkVerticalFuncType :: Type -> VerticalFuncType
mkVerticalFuncType = VerticalFuncType

newtype DeclSigType =
  DeclSigType Type

instance CommentExtraction DeclSigType where
  nodeComments (DeclSigType t) = nodeComments t

instance Pretty DeclSigType where
  pretty' (DeclSigType (ConstrainedType ctxt body)) = hor <-|> ver
    where
      hor = spaced [pretty ctxt, string "=>", pretty body]
      ver = do
        pretty ctxt
        newline
        prefixed "=> " $ pretty $ fmap mkVerticalFuncType body
  pretty' (DeclSigType (Function a b)) = hor <-|> ver
    where
      hor = spaced [pretty a, string "->", pretty b]
      ver = do
        pretty $ fmap mkVerticalFuncType a
        newline
        prefixed "-> " $ pretty $ fmap mkVerticalFuncType b
  pretty' (DeclSigType t) = pretty t

mkDeclSigType :: Type -> DeclSigType
mkDeclSigType = DeclSigType

newtype InstDeclType =
  InstDeclType Type

instance CommentExtraction InstDeclType where
  nodeComments (InstDeclType t) = nodeComments t

instance Pretty InstDeclType where
  pretty' (InstDeclType (ConstrainedType ctxt body)) = hor <-|> ver
    where
      hor = spaced [pretty ctxt, string "=>", pretty body]
      ver = do
        pretty ctxt >> string " =>"
        newline
        indentedWithFixedLevel 9 $ pretty body
  pretty' (InstDeclType t) = pretty t

mkInstDeclType :: Type -> InstDeclType
mkInstDeclType = InstDeclType
