{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

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
      { telescope :: WithComments (GHC.HsForAllTelescope GHC.GhcPs)
      , body :: WithComments Type
      }
  | ConstrainedType
      { context :: WithComments Context
      , body :: WithComments Type
      }
  | Variable
      { promotionFlag :: GHC.PromotionFlag
      , name :: WithComments PrefixName
      }
  | Application
      { function :: WithComments Type
      , argument :: WithComments Type
      }
  | KindApplication
      { base :: WithComments Type
      , kind :: WithComments Type
      }
  | Function
      { from :: WithComments Type
      , to :: WithComments Type
      }
  | List
      { elementType :: WithComments Type
      }
  | Tuple
      { tupleSort :: GHC.HsTupleSort
      , elements :: [WithComments Type]
      }
  | Sum
      { elements :: [WithComments Type]
      }
  | InfixType
      { left :: WithComments Type
      , operator :: WithComments InfixName
      , right :: WithComments Type
      }
  | Parenthesized
      { inner :: WithComments Type
      }
  | ImplicitParameter
      { ipName :: GHC.HsIPName
      , paramType :: WithComments Type
      }
  | Star
  | KindSig
      { annotated :: WithComments Type
      , kind :: WithComments Type
      }
  | Splice
      { splice :: Splice
      }
  | StrictType
      { bang :: Bang
      , baseType :: WithComments Type
      }
  | RecordType
      { fields :: [WithComments RecordField]
      }
  | PromotedList
      { elements :: [WithComments Type]
      }
  | PromotedTuple
      { elements :: [WithComments Type]
      }
  | Literal
      { literal :: Literal
      }
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
  pretty' UniversalType {..} =
    (pretty (getNode telescope) >> space) |=> pretty body
  pretty' ConstrainedType {..} = hor <-|> ver
    where
      hor = spaced [pretty context, string "=>", pretty body]
      ver = do
        pretty context
        lined [string " =>", indentedBlock $ pretty body]
  pretty' Variable {promotionFlag = GHC.NotPromoted, ..} = pretty name
  pretty' Variable {promotionFlag = GHC.IsPromoted, ..} =
    string "'" >> pretty name
  pretty' Application {..} = hor <-|> ver
    where
      hor = spaced [pretty function, pretty argument]
      ver = verticalApp function argument
      verticalApp left right = do
        case getNode left of
          Application {function = l', argument = r'} ->
            verticalApp l' r' >> newline >> indentedBlock (pretty right)
          _ -> pretty left >> newline >> indentedBlock (pretty right)
  pretty' KindApplication {..} = pretty base >> string " @" >> pretty kind
  pretty' Function {..} = (pretty from >> string " -> ") |=> pretty to
  pretty' List {..} = brackets $ pretty elementType
  pretty' Tuple {tupleSort = GHC.HsUnboxedTuple, elements = []} = string "(# #)"
  pretty' Tuple {tupleSort = GHC.HsBoxedOrConstraintTuple, elements = []} =
    string "()"
  pretty' Tuple {tupleSort = GHC.HsUnboxedTuple, ..} =
    hvUnboxedTuple' $ fmap pretty elements
  pretty' Tuple {tupleSort = GHC.HsBoxedOrConstraintTuple, ..} =
    hvTuple' $ fmap pretty elements
  pretty' Sum {..} = hvUnboxedSum' $ fmap pretty elements
  pretty' InfixType {..} = do
    lineBreak <- gets (configLineBreaks . psConfig)
    if getInfixName (getNode operator) `elem` lineBreak
      then do
        pretty left
        newline
        pretty operator
        space
        pretty right
      else spaced [pretty left, pretty operator, pretty right]
  pretty' Parenthesized {..} = parens $ pretty inner
  pretty' ImplicitParameter {..} =
    spaced [string "?" >> pretty ipName, string "::", pretty paramType]
  pretty' Star = string "*"
  pretty' KindSig {..} = spaced [pretty annotated, string "::", pretty kind]
  pretty' Splice {..} = pretty splice
  pretty' StrictType {..} = pretty bang >> pretty baseType
  pretty' RecordType {..} = hvFields $ fmap pretty fields
  pretty' PromotedList {elements = []} = string "'[]"
  pretty' PromotedList {..} = hvPromotedList $ fmap pretty elements
  pretty' PromotedTuple {..} = hPromotedTuple $ fmap pretty elements
  pretty' Literal {..} = pretty literal
  pretty' Wildcard = string "_"

mkType :: GHC.HsType GHC.GhcPs -> Type
mkType (GHC.HsForAllTy _ tele body) =
  UniversalType
    {telescope = mkWithComments tele, body = mkType <$> fromGenLocated body}
mkType GHC.HsQualTy {..} =
  ConstrainedType
    { context = mkWithComments $ Context hst_ctxt
    , body = mkType <$> fromGenLocated hst_body
    }
mkType (GHC.HsTyVar _ prom x) =
  Variable {promotionFlag = prom, name = mkPrefixName <$> fromGenLocated x}
mkType (GHC.HsAppTy _ l r) =
  Application
    { function = mkType <$> fromGenLocated l
    , argument = mkType <$> fromGenLocated r
    }
#if MIN_VERSION_ghc_lib_parser(9, 8, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkType (GHC.HsAppKindTy _ l _ r) =
  KindApplication
    {base = mkType <$> fromGenLocated l, kind = mkType <$> fromGenLocated r}
#else
mkType (GHC.HsAppKindTy _ l r) =
  KindApplication
    {base = mkType <$> fromGenLocated l, kind = mkType <$> fromGenLocated r}
#endif
mkType (GHC.HsFunTy _ _ a b) =
  Function
    {from = mkType <$> fromGenLocated a, to = mkType <$> fromGenLocated b}
mkType (GHC.HsListTy _ xs) = List {elementType = mkType <$> fromGenLocated xs}
mkType (GHC.HsTupleTy _ sort xs) =
  Tuple {tupleSort = sort, elements = fmap (fmap mkType . fromGenLocated) xs}
mkType (GHC.HsSumTy _ xs) =
  Sum {elements = fmap (fmap mkType . fromGenLocated) xs}
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkType (GHC.HsOpTy _ _ l op r) =
  InfixType
    { left = mkType <$> fromGenLocated l
    , operator = mkInfixName <$> fromGenLocated op
    , right = mkType <$> fromGenLocated r
    }
#else
mkType (GHC.HsOpTy _ l op r) =
  InfixType
    { left = mkType <$> fromGenLocated l
    , operator = mkInfixName <$> fromGenLocated op
    , right = mkType <$> fromGenLocated r
    }
#endif
mkType (GHC.HsParTy _ inside) =
  Parenthesized {inner = mkType <$> fromGenLocated inside}
mkType (GHC.HsIParamTy _ x ty) =
  ImplicitParameter
    { ipName = getNode $ fromGenLocated x
    , paramType = mkType <$> fromGenLocated ty
    }
mkType GHC.HsStarTy {} = Star
mkType (GHC.HsKindSig _ t k) =
  KindSig
    { annotated = mkType <$> fromGenLocated t
    , kind = mkType <$> fromGenLocated k
    }
mkType (GHC.HsSpliceTy _ sp) = Splice {splice = mkSplice sp}
mkType GHC.HsDocTy {} = error "HsDocTy not supported"
mkType (GHC.HsBangTy _ pack x) =
  StrictType {bang = mkBang pack, baseType = mkType <$> fromGenLocated x}
mkType (GHC.HsRecTy _ xs) =
  RecordType {fields = fmap (fromGenLocated . fmap mkRecordField) xs}
mkType (GHC.HsExplicitListTy _ _ xs) =
  PromotedList {elements = fmap (fmap mkType . fromGenLocated) xs}
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkType (GHC.HsExplicitTupleTy _ _ xs) =
  PromotedTuple {elements = fmap (fmap mkType . fromGenLocated) xs}
#else
mkType (GHC.HsExplicitTupleTy _ xs) =
  PromotedTuple {elements = fmap (fmap mkType . fromGenLocated) xs}
#endif
mkType (GHC.HsTyLit _ x) = Literal {literal = mkLiteral x}
mkType GHC.HsWildCardTy {} = Wildcard
mkType GHC.XHsType {} = error "XHsType not generated by parser"

newtype VerticalFuncType =
  VerticalFuncType Type

instance CommentExtraction VerticalFuncType where
  nodeComments (VerticalFuncType t) = nodeComments t

instance Pretty VerticalFuncType where
  pretty' (VerticalFuncType Function {..}) = do
    pretty $ fmap mkVerticalFuncType from
    newline
    prefixed "-> " $ pretty $ fmap mkVerticalFuncType to
  pretty' (VerticalFuncType t) = pretty t

mkVerticalFuncType :: Type -> VerticalFuncType
mkVerticalFuncType = VerticalFuncType

newtype DeclSigType =
  DeclSigType Type

instance CommentExtraction DeclSigType where
  nodeComments (DeclSigType t) = nodeComments t

instance Pretty DeclSigType where
  pretty' (DeclSigType ConstrainedType {..}) = hor <-|> ver
    where
      hor = spaced [pretty context, string "=>", pretty body]
      ver = do
        pretty context
        newline
        prefixed "=> " $ pretty $ fmap mkVerticalFuncType body
  pretty' (DeclSigType Function {..}) = hor <-|> ver
    where
      hor = spaced [pretty from, string "->", pretty to]
      ver = do
        pretty $ fmap mkVerticalFuncType from
        newline
        prefixed "-> " $ pretty $ fmap mkVerticalFuncType to
  pretty' (DeclSigType t) = pretty t

mkDeclSigType :: Type -> DeclSigType
mkDeclSigType = DeclSigType

newtype InstDeclType =
  InstDeclType Type

instance CommentExtraction InstDeclType where
  nodeComments (InstDeclType t) = nodeComments t

instance Pretty InstDeclType where
  pretty' (InstDeclType ConstrainedType {..}) = hor <-|> ver
    where
      hor = spaced [pretty context, string "=>", pretty body]
      ver = do
        pretty context >> string " =>"
        newline
        indentedWithFixedLevel 9 $ pretty body
  pretty' (InstDeclType t) = pretty t

mkInstDeclType :: Type -> InstDeclType
mkInstDeclType = InstDeclType
