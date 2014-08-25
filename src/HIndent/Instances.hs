{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Instances for pretty printing.

module HIndent.Instances where

import           HIndent.Types
import           HIndent.Combinators

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import           Language.Haskell.Exts.Syntax

import           Control.Monad.State
import           Data.Monoid
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.IO as TIO
import           Language.Haskell.Exts.Parser

instance Pretty Stmt where
  pretty x =
    case x of
      Generator _ _ _ -> undefined
      Qualifier _ -> undefined
      LetStmt _ -> undefined
      RecStmt _ -> undefined

instance Pretty QualStmt where
  pretty x =
    case x of
      QualStmt _ -> undefined
      ThenTrans _ -> undefined
      ThenBy _ _ -> undefined
      GroupBy _ -> undefined
      GroupUsing _ -> undefined
      GroupByUsing _ _ -> undefined

instance Pretty Pat where
  pretty x =
    case x of
      PLit l ->
        pretty l
      PNeg l ->
        depend (write "-")
               (pretty l)
      PNPlusK n k ->
        depend (do pretty n
                   write "-")
               (int k)
      PInfixApp a op b ->
        depend (do pretty a
                   space)
               (depend (do pretty op
                           space)
                       (pretty b))
      PApp f args ->
        depend (do pretty f
                   space)
               (spaced (map pretty args))
      PTuple boxed pats ->
        depend (write (case boxed of
                         Boxed -> "(#"
                         Unboxed -> "("))
               (do commas (map pretty pats)
                   write (case boxed of
                            Boxed -> "#)"
                            Unboxed -> ")"))
      PList ps -> brackets (commas (map pretty ps))
      PParen e ->
        parens (pretty e)
      PRec qname fields -> undefined
      PAsPat _ _ -> undefined
      PWildCard -> undefined
      PIrrPat _ -> undefined
      PatTypeSig _ _ _ -> undefined
      PViewPat _ _ -> undefined
      PQuasiQuote _ _ -> undefined
      PBangPat _ -> undefined
      PRPat{} -> pretty' x
      PXTag{} -> pretty' x
      PXETag{} -> pretty' x
      PXPcdata{} -> pretty' x
      PXPatTag{} -> pretty' x
      PXRPats{} -> pretty' x
      PVar{} -> pretty' x

instance Pretty Type where
  pretty x =
    case x of
      TyForall _ _ _ -> undefined
      TyFun _ _ -> undefined
      TyTuple _ _ -> undefined
      TyList _ -> undefined
      TyApp _ _ -> undefined
      TyVar _ -> undefined
      TyCon _ -> undefined
      TyParen _ -> undefined
      TyInfix _ _ _ -> undefined
      TyKind _ _ -> undefined
      TyPromoted _ -> undefined

instance Pretty Exp where
  pretty x =
    case x of
      InfixApp a op b ->
        do pretty a
           space
           pretty op
           newline
           pretty b
      App op a ->
        case flatten op [a] of
          (f,args) ->
            depend (do pretty f
                       space)
                   (lined (map pretty args))
      NegApp e ->
        depend (write "-")
               (pretty e)
      Lambda _ ps e ->
        depend (write "\\")
               (do spaced (map pretty ps)
                   write " -> "
                   newline
                   indented 1 (pretty e))
      Let _ e ->
        do write "let"
           newline
           depend (write "in ")
                  (pretty e)
      If p t e ->
        do depend (write "if ")
                  (do pretty p
                      newline
                      depend (write "then ")
                             (pretty t)
                      newline
                      depend (write "else ")
                             (pretty e))
      Paren e ->
        parens (pretty e)
      MultiIf _ -> undefined
      Case e alts ->
        do write "case "
           indented 5 (pretty e)
           write " of"
           newline
           indented 2 (mapM_ pretty alts)
      Do _ -> undefined
      MDo _ -> undefined
      Tuple _ _ -> undefined
      TupleSection _ _ -> undefined
      List _ -> undefined
      LeftSection _ _ -> undefined
      RightSection _ _ -> undefined
      RecConstr _ _ -> undefined
      RecUpdate _ _ -> undefined
      EnumFrom _ -> undefined
      EnumFromTo _ _ -> undefined
      EnumFromThen _ _ -> undefined
      EnumFromThenTo _ _ _ -> undefined
      ListComp _ _ -> undefined
      ParComp _ _ -> undefined
      ExpTypeSig _ _ _ -> undefined
      VarQuote _ -> undefined
      TypQuote _ -> undefined
      BracketExp _ -> undefined
      SpliceExp _ -> undefined
      QuasiQuote _ _ -> undefined
      CorePragma _ _ -> undefined
      SCCPragma _ _ -> undefined
      GenPragma _ _ _ _ -> undefined
      Proc _ _ _ -> undefined
      LeftArrApp _ _ -> undefined
      RightArrApp _ _ -> undefined
      LeftArrHighApp _ _ -> undefined
      RightArrHighApp _ _ -> undefined
      LCase _ -> undefined
      XTag{} -> pretty' x
      XETag{} -> pretty' x
      XPcdata{} -> pretty' x
      XExpTag{} -> pretty' x
      XChildTag{} -> pretty' x
      Var{} -> pretty' x
      IPVar{} -> pretty' x
      Con{} -> pretty' x
      Lit{} -> pretty' x
    where flatten :: Exp -> [Exp] -> (Exp,[Exp])
          flatten (App f a) b = flatten f (a : b)
          flatten f as = (f,as)

instance Pretty Decl where
  pretty x =
    case x of
      TypeDecl _ _ _ _ -> undefined
      TypeFamDecl _ _ _ _ -> undefined
      DataDecl _ _ _ _ _ _ _ -> undefined
      GDataDecl _ _ _ _ _ _ _ _ -> undefined
      DataFamDecl _ _ _ _ _ -> undefined
      TypeInsDecl _ _ _ -> undefined
      DataInsDecl _ _ _ _ _ -> undefined
      GDataInsDecl _ _ _ _ _ _ -> undefined
      ClassDecl _ _ _ _ _ _ -> undefined
      InstDecl _ _ _ _ _ -> undefined
      DerivDecl _ _ _ _ -> undefined
      InfixDecl _ _ _ _ -> undefined
      DefaultDecl _ _ -> undefined
      SpliceDecl _ _ -> undefined
      TypeSig _ _ _ -> undefined
      FunBind _ -> undefined
      PatBind _ _ _ _ _ -> undefined
      ForImp _ _ _ _ _ _ -> undefined
      ForExp _ _ _ _ _ -> undefined
      RulePragmaDecl _ _ -> undefined
      DeprPragmaDecl _ _ -> undefined
      WarnPragmaDecl _ _ -> undefined
      InlineSig _ _ _ _ -> undefined
      InlineConlikeSig _ _ _ -> undefined
      SpecSig _ _ _ _ -> undefined
      SpecInlineSig _ _ _ _ _ -> undefined
      InstSig _ _ _ _ -> undefined
      AnnPragma _ _ -> undefined

instance Pretty Alt where
  pretty x =
    case x of
      Alt _ _ _ _ ->
        undefined

instance Pretty Asst where
  pretty x =
    case x of
      ClassA _ _ -> undefined
      InfixA _ _ _ -> undefined
      IParam _ _ -> undefined
      EqualP _ _ -> undefined

instance Pretty BangType where
  pretty x =
    case x of
      BangedTy _ -> undefined
      UnBangedTy _ -> undefined
      UnpackedTy _ -> undefined

instance Pretty Binds where
  pretty x =
    case x of
      BDecls _ -> undefined
      IPBinds _ -> undefined

instance Pretty Bracket where
  pretty x =
    case x of
      ExpBracket _ -> undefined
      PatBracket _ -> undefined
      TypeBracket _ -> undefined
      DeclBracket _ -> undefined

instance Pretty ClassDecl where
  pretty _ =
    undefined

instance Pretty ConDecl where
  pretty x =
    case x of
      ConDecl _ _ -> undefined
      InfixConDecl _ _ _ -> undefined
      RecDecl _ _ -> undefined

instance Pretty FieldUpdate where
  pretty x =
    case x of
      FieldUpdate _ _ -> undefined
      FieldPun _ -> undefined
      FieldWildcard -> undefined

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
      GuardedAlts _ -> undefined

instance Pretty GuardedRhs where
  pretty x =
    case x of
      GuardedRhs _ _ _ -> undefined

instance Pretty IPBind where
  pretty x =
    case x of
      IPBind _ _ _ -> undefined

instance Pretty IfAlt where
  pretty x =
    case x of
      IfAlt _ _ -> undefined

instance Pretty InstDecl where
  pretty _ =
    undefined

instance Pretty Match where
  pretty x =
    case x of
      Match _ _ _ _ _ _ -> undefined

instance Pretty Module where
  pretty x =
    case x of
      Module _ _ _ _ _ _ _ -> undefined

instance Pretty PatField where
  pretty x =
    case x of
      PFieldPat _ _ -> undefined
      PFieldPun _ -> undefined
      PFieldWildcard -> undefined

instance Pretty QualConDecl where
  pretty x =
    case x of
      QualConDecl _ _ _ _ -> undefined

instance Pretty Rhs where
  pretty x =
    case x of
      UnGuardedRhs _ -> undefined
      GuardedRhss _ -> undefined

instance Pretty Rule where
  pretty =
    case undefined of
      Rule _ _ _ _ _ -> undefined

instance Pretty RuleVar where
  pretty x =
    case x of
      RuleVar _ -> undefined
      TypedRuleVar _ _ -> undefined

instance Pretty Splice where
  pretty _ =
    case undefined of
      IdSplice _ -> undefined
      ParenSplice _ -> undefined

instance Pretty WarningText where
  pretty x =
    case x of
      DeprText _ -> undefined
      WarnText _ -> undefined

instance Pretty Tool where
  pretty x =
    case x of
      GHC -> write "GHC"
      HUGS -> write "HUGS"
      NHC98 -> write "NHC98"
      YHC -> write "YHC"
      HADDOCK -> write "HADDOCK"
      UnknownTool t -> write (T.fromText (T.pack t))

instance Pretty Activation where
  pretty = pretty'

instance Pretty Annotation where
  pretty = pretty'

instance Pretty Assoc where
  pretty = pretty'

instance Pretty CName where
  pretty = pretty'

instance Pretty CallConv where
  pretty = pretty'

instance Pretty DataOrNew where
  pretty = pretty'

instance Pretty ExportSpec where
  pretty = pretty'

instance Pretty FunDep where
  pretty = pretty'

instance Pretty IPName where
  pretty = pretty'

instance Pretty ImportSpec where
  pretty = pretty'

instance Pretty ImportDecl where
  pretty = pretty'

instance Pretty Kind where
  pretty = pretty'

instance Pretty Literal where
  pretty = pretty'

instance Pretty ModulePragma where
  pretty = pretty'

instance Pretty Name where
  pretty = pretty'

instance Pretty Op where
  pretty = pretty'

instance Pretty PXAttr where
  pretty = pretty'

instance Pretty Promoted where
  pretty = pretty'

instance Pretty QName where
  pretty = pretty'

instance Pretty QOp where
  pretty = pretty'

instance Pretty RPat where
  pretty = pretty'

instance Pretty RPatOp where
  pretty = pretty'

instance Pretty Safety where
  pretty = pretty'

instance Pretty SpecialCon where
  pretty = pretty'

instance Pretty TyVarBind where
  pretty = pretty'

instance Pretty XAttr where
  pretty = pretty'

instance Pretty XName where
  pretty = pretty'

-- | Format the string and print it to stdout. Throws exception on
-- invalid syntax.
indent :: String -> IO ()
indent x =
  case reformat x of
    Right b -> TIO.putStrLn (T.toLazyText b)
    Left e -> error e

-- | Format the given source.
reformat :: String -> Either String Builder
reformat x =
  case parseExp x of
    ParseOk v ->
      Right (prettyPrint v)
    ParseFailed _ e ->
      Left e

-- | Pretty print the given printable thing.
prettyPrint :: Pretty a => a -> Builder
prettyPrint v =
  psOutput
    (execState (runPrinter (pretty v))
               (PrintState 0 mempty False 0))
