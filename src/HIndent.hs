{-# LANGUAGE OverloadedStrings #-}

-- | Haskell indenter.

module HIndent where

import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import           Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.Pretty as P
import           Language.Haskell.Exts.Syntax

class Pretty a where
  pretty :: a -> Builder

pretty' :: P.Pretty a => a -> Builder
pretty' = T.fromText . T.pack . P.prettyPrint

instance Pretty Activation where
  pretty = pretty'

instance Pretty Alt where
  pretty x =
    case x of
      Alt _ _ _ _ -> undefined

instance Pretty Annotation where
  pretty = pretty'

instance Pretty Assoc where
  pretty = pretty'

instance Pretty Asst where
  pretty x =
    case x of
      ClassA _ _   -> undefined
      InfixA _ _ _ -> undefined
      IParam _ _   -> undefined
      EqualP _ _   -> undefined

instance Pretty BangType where
  pretty x =
    case x of
      BangedTy _   -> undefined
      UnBangedTy _ -> undefined
      UnpackedTy _ -> undefined

instance Pretty Binds where
  pretty x =
    case x of
      BDecls _  -> undefined
      IPBinds _ -> undefined

instance Pretty Bracket where
  pretty x =
    case x of
      ExpBracket _  -> undefined
      PatBracket _  -> undefined
      TypeBracket _ -> undefined
      DeclBracket _ -> undefined

instance Pretty CName where
  pretty = pretty'

instance Pretty CallConv where
  pretty = pretty'

instance Pretty ClassDecl where
  pretty x =
    undefined

instance Pretty ConDecl where
  pretty x =
    case x of
      ConDecl _ _        -> undefined
      InfixConDecl _ _ _ -> undefined
      RecDecl _ _        -> undefined

instance Pretty DataOrNew where
  pretty = pretty'

instance Pretty Decl where
  pretty x =
    case x of
      TypeDecl _ _ _ _          -> undefined
      TypeFamDecl _ _ _ _       -> undefined
      DataDecl _ _ _ _ _ _ _    -> undefined
      GDataDecl _ _ _ _ _ _ _ _ -> undefined
      DataFamDecl _ _ _ _ _     -> undefined
      TypeInsDecl _ _ _         -> undefined
      DataInsDecl _ _ _ _ _     -> undefined
      GDataInsDecl _ _ _ _ _ _  -> undefined
      ClassDecl _ _ _ _ _ _     -> undefined
      InstDecl _ _ _ _ _        -> undefined
      DerivDecl _ _ _ _         -> undefined
      InfixDecl _ _ _ _         -> undefined
      DefaultDecl _ _           -> undefined
      SpliceDecl _ _            -> undefined
      TypeSig _ _ _             -> undefined
      FunBind _                 -> undefined
      PatBind _ _ _ _ _         -> undefined
      ForImp _ _ _ _ _ _        -> undefined
      ForExp _ _ _ _ _          -> undefined
      RulePragmaDecl _ _        -> undefined
      DeprPragmaDecl _ _        -> undefined
      WarnPragmaDecl _ _        -> undefined
      InlineSig _ _ _ _         -> undefined
      InlineConlikeSig _ _ _    -> undefined
      SpecSig _ _ _ _           -> undefined
      SpecInlineSig _ _ _ _ _   -> undefined
      InstSig _ _ _ _           -> undefined
      AnnPragma _ _             -> undefined

instance Pretty Exp where
  pretty x =
    case x of
      Var _                -> undefined
      IPVar _              -> undefined
      Con _                -> undefined
      Lit _                -> undefined
      InfixApp _ _ _       -> undefined
      App _ _              -> undefined
      NegApp _             -> undefined
      Lambda _ _ _         -> undefined
      Let _ _              -> undefined
      If _ _ _             -> undefined
      MultiIf _            -> undefined
      Case _ _             -> undefined
      Do _                 -> undefined
      MDo _                -> undefined
      Tuple _ _            -> undefined
      TupleSection _ _     -> undefined
      List _               -> undefined
      Paren _              -> undefined
      LeftSection _ _      -> undefined
      RightSection _ _     -> undefined
      RecConstr _ _        -> undefined
      RecUpdate _ _        -> undefined
      EnumFrom _           -> undefined
      EnumFromTo _ _       -> undefined
      EnumFromThen _ _     -> undefined
      EnumFromThenTo _ _ _ -> undefined
      ListComp _ _         -> undefined
      ParComp _ _          -> undefined
      ExpTypeSig _ _ _     -> undefined
      VarQuote _           -> undefined
      TypQuote _           -> undefined
      BracketExp _         -> undefined
      SpliceExp _          -> undefined
      QuasiQuote _ _       -> undefined
      XTag _ _ _ _ _       -> undefined
      XETag _ _ _ _        -> undefined
      XPcdata _            -> undefined
      XExpTag _            -> undefined
      XChildTag _ _        -> undefined
      CorePragma _ _       -> undefined
      SCCPragma _ _        -> undefined
      GenPragma _ _ _ _    -> undefined
      Proc _ _ _           -> undefined
      LeftArrApp _ _       -> undefined
      RightArrApp _ _      -> undefined
      LeftArrHighApp _ _   -> undefined
      RightArrHighApp _ _  -> undefined
      LCase _              -> undefined

instance Pretty ExportSpec where
  pretty = pretty'

instance Pretty FieldUpdate where
  pretty x =
    case x of
      FieldUpdate _ _ -> undefined
      FieldPun _      -> undefined
      FieldWildcard   -> undefined

instance Pretty FunDep where
  pretty = pretty'

instance Pretty GadtDecl where
  pretty x =
    case x of
      GadtDecl _ _ _ -> undefined

instance Pretty GuardedAlt where
  pretty x =
    case x of
      GuardedAlt _ _ _ -> undefined

instance Pretty GuardedAlts where
  pretty x =
    case x of
      UnGuardedAlt _ -> undefined
      GuardedAlts _  -> undefined

instance Pretty GuardedRhs where
  pretty x =
    case x of
      GuardedRhs _ _ _ -> undefined

instance Pretty IPBind where
  pretty x =
    case x of
      IPBind _ _ _ -> undefined

instance Pretty IPName where
  pretty = pretty'

instance Pretty IfAlt where
  pretty x =
    case x of
      IfAlt _ _ -> undefined

instance Pretty ImportSpec where
  pretty = pretty'

instance Pretty ImportDecl where
  pretty = pretty'

instance Pretty InstDecl where
  pretty x =
    undefined

instance Pretty Kind where
  pretty = pretty'

instance Pretty Literal where
  pretty = pretty'

instance Pretty Match where
  pretty x =
    case x of
      Match _ _ _ _ _ _ -> undefined

instance Pretty Module where
  pretty x =
    case x of
      Module _ _ _ _ _ _ _ -> undefined

instance Pretty ModulePragma where
  pretty = pretty'

instance Pretty Name where
  pretty = pretty'

instance Pretty Op where
  pretty = pretty'

instance Pretty PXAttr where
  pretty = pretty'

instance Pretty Pat where
  pretty x =
    case x of
      PVar _           -> undefined
      PLit _           -> undefined
      PNeg _           -> undefined
      PNPlusK _ _      -> undefined
      PInfixApp _ _ _  -> undefined
      PApp _ _         -> undefined
      PTuple _ _       -> undefined
      PList _          -> undefined
      PParen _         -> undefined
      PRec _ _         -> undefined
      PAsPat _ _       -> undefined
      PWildCard        -> undefined
      PIrrPat _        -> undefined
      PatTypeSig _ _ _ -> undefined
      PViewPat _ _     -> undefined
      PRPat _          -> undefined
      PXTag _ _ _ _ _  -> undefined
      PXETag _ _ _ _   -> undefined
      PXPcdata _       -> undefined
      PXPatTag _       -> undefined
      PXRPats _        -> undefined
      PQuasiQuote _ _  -> undefined
      PBangPat _       -> undefined

instance Pretty PatField where
  pretty x =
    case x of
      PFieldPat _ _  -> undefined
      PFieldPun _    -> undefined
      PFieldWildcard -> undefined

instance Pretty Promoted where
  pretty = pretty'

instance Pretty QName where
  pretty = pretty'

instance Pretty QOp where
  pretty = pretty'

instance Pretty QualConDecl where
  pretty x =
    case x of
      QualConDecl _ _ _ _ -> undefined

instance Pretty QualStmt where
  pretty x =
    case x of
      QualStmt _       -> undefined
      ThenTrans _      -> undefined
      ThenBy _ _       -> undefined
      GroupBy _        -> undefined
      GroupUsing _     -> undefined
      GroupByUsing _ _ -> undefined

instance Pretty RPat where
  pretty = pretty'

instance Pretty RPatOp where
  pretty = pretty'

instance Pretty Rhs where
  pretty x =
    case x of
      UnGuardedRhs _ -> undefined
      GuardedRhss _  -> undefined

instance Pretty Rule where
  pretty =
    case undefined of
      Rule _ _ _ _ _ -> undefined

instance Pretty RuleVar where
  pretty x =
    case x of
      RuleVar _        -> undefined
      TypedRuleVar _ _ -> undefined

instance Pretty Safety where
  pretty = pretty'

instance Pretty SpecialCon where
  pretty = pretty'

instance Pretty Splice where
  pretty x =
    case undefined of
      IdSplice _    -> undefined
      ParenSplice _ -> undefined

instance Pretty Stmt where
  pretty x =
    case x of
      Generator _ _ _ -> undefined
      Qualifier _     -> undefined
      LetStmt _       -> undefined
      RecStmt _       -> undefined

instance Pretty TyVarBind where
  pretty = pretty'

instance Pretty Type where
  pretty x =
    case x of
      TyForall _ _ _ -> undefined
      TyFun _ _      -> undefined
      TyTuple _ _    -> undefined
      TyList _       -> undefined
      TyApp _ _      -> undefined
      TyVar _        -> undefined
      TyCon _        -> undefined
      TyParen _      -> undefined
      TyInfix _ _ _  -> undefined
      TyKind _ _     -> undefined
      TyPromoted _   -> undefined

instance Pretty WarningText where
  pretty x =
    case x of
      DeprText _ -> undefined
      WarnText _ -> undefined

instance Pretty XAttr where
  pretty = pretty'

instance Pretty XName where
  pretty = pretty'

instance Pretty Tool where
  pretty x =
    case x of
      GHC           -> "GHC"
      HUGS          -> "HUGS"
      NHC98         -> "NHC98"
      YHC           -> "YHC"
      HADDOCK       -> "HADDOCK"
      UnknownTool x -> T.fromText (T.pack x)
