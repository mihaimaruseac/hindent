{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Instances for pretty printing.

module HIndent.Instances where

import           Data.Int
import           Language.Haskell.Exts.Extension
import           Prelude hiding (exp)

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

-- | Indent spaces: 2.
indentSpaces :: Int64
indentSpaces = 2

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
                         Unboxed -> "(#"
                         Boxed -> "("))
               (do commas (map pretty pats)
                   write (case boxed of
                            Unboxed -> "#)"
                            Boxed -> ")"))
      PList ps ->
        brackets (commas (map pretty ps))
      PParen e ->
        parens (pretty e)
      PRec qname fields ->
        depend (pretty qname)
               (braces
                  (commas (map pretty fields)))
      PAsPat n p ->
        depend (do pretty n
                   write "@")
               (pretty p)
      PWildCard ->
        write "_"
      PIrrPat p ->
        depend (write "~")
               (pretty p)
      PatTypeSig _ p ty ->
        depend (do pretty p
                   write " :: ")
               (pretty ty)
      PViewPat e p ->
        depend (do pretty e
                   write " -> ")
               (pretty p)
      PQuasiQuote name str ->
        brackets (depend (do write "$"
                             string name
                             write "|")
                         (string str))
      PBangPat p ->
        depend (write "!")
               (pretty p)
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
      TyForall mbinds ctx ty ->
        depend (case mbinds of
                  Nothing ->
                    return ()
                  Just ts ->
                    do write "forall "
                       spaced (map pretty ts)
                       write ". ")
               (depend (case ctx of
                          [] -> return ()
                          (_:_) -> parens (mapM_ pretty ctx))
                       (pretty ty))
      TyFun a b ->
        depend (do pretty a
                   write " -> ")
               (pretty b)
      TyTuple boxed tys ->
        depend (write (case boxed of
                         Unboxed -> "(#"
                         Boxed -> "("))
               (do commas (map pretty tys)
                   write (case boxed of
                            Unboxed -> "#)"
                            Boxed -> ")"))
      TyList t ->
        brackets (pretty t)
      TyApp f a ->
        spaced [pretty f,pretty a]
      TyVar n ->
        pretty n
      TyCon p ->
        pretty p
      TyParen e ->
        parens (pretty e)
      TyInfix a op b ->
        depend (do pretty a
                   space)
               (depend (do pretty op
                           space)
                       (pretty b))
      TyKind ty k ->
        parens (do pretty ty
                   write " :: "
                   pretty k)
      TyPromoted{} ->
        error "FIXME: No implementation for TyPromoted."

instance Pretty Exp where
  pretty = exp

exp :: Exp -> Printer ()
exp (InfixApp a op b) =
  do pretty a
     space
     pretty op
     newline
     pretty b
exp (App op a) =
  case flatten op [a] of
    (f,args) ->
      depend (do pretty f
                 space)
             (lined (map pretty args))
  where flatten :: Exp -> [Exp] -> (Exp,[Exp])
        flatten (App f a') b = flatten f (a' : b)
        flatten f as = (f,as)
exp (NegApp e) =
  depend (write "-")
         (pretty e)
exp (Lambda _ ps e) =
  depend (write "\\")
         (do spaced (map pretty ps)
             write " = "
             newline
             indented 1 (pretty e))
exp (Let binds e) =
  do depend (write "let ")
            (pretty binds)
     newline
     depend (write "in ")
            (pretty e)
exp (If p t e) =
  do depend (write "if ")
            (do pretty p
                newline
                depend (write "then ")
                       (pretty t)
                newline
                depend (write "else ")
                       (pretty e))
exp (Paren e) =
  parens (pretty e)
exp (MultiIf _) =
  error "FIXME: No implementation for MultiIf."
exp (Case _ _) =
  error "FIXME: No implementation for Case."
exp (Do stmts) =
  depend (write "do ")
         (lined (map pretty stmts))
exp (MDo stmts) =
  depend (write "mdo ")
         (lined (map pretty stmts))
exp (Tuple boxed exps) =
  depend (write (case boxed of
                   Unboxed -> "(#"
                   Boxed -> "("))
         (do commas (map pretty exps)
             write (case boxed of
                      Unboxed -> "#)"
                      Boxed -> ")"))
exp (TupleSection boxed mexps) =
  depend (write (case boxed of
                   Unboxed -> "(#"
                   Boxed -> "("))
         (do commas (map (maybe (return ()) pretty) mexps)
             write (case boxed of
                      Unboxed -> "#)"
                      Boxed -> ")"))
exp (List es) =
  brackets (commas (map pretty es))
exp (LeftSection e op) =
  parens (depend (do pretty e
                     space)
                 (pretty op))
exp (RightSection e op) =
  parens (depend (do pretty e
                     space)
                 (pretty op))
exp (RecConstr n fs) =
  depend (do pretty n
             space)
         (braces
            (prefixedLined ','
                           (map pretty fs)))
exp (RecUpdate n fs) =
  depend (do pretty n
             space)
         (braces
            (prefixedLined ','
                           (map pretty fs)))
exp (EnumFrom e) =
  brackets (do pretty e
               write " ..")
exp (EnumFromTo e f) =
  brackets (depend (do pretty e
                       write " .. ")
                   (pretty f))
exp (EnumFromThen e t) =
  brackets (depend (do pretty e
                       write ",")
                   (do pretty t
                       write " .."))
exp (EnumFromThenTo e t f) =
  brackets (depend (do pretty e
                       write ",")
                   (depend (do pretty t
                               write " .. ")
                           (pretty f)))
exp (ListComp e qstmt) =
  brackets
    (depend
       (do pretty e
           unless (null qstmt)
                  (write " |"))
       (do space
           prefixedLined ','
                         (map (\(i,x) ->
                                 depend (if i == 0
                                            then return ()
                                            else space)
                                        (pretty x))
                              (zip [0::Integer ..] qstmt))) )
exp (ExpTypeSig _ e t) =
  depend (do pretty e
             write " :: ")
         (pretty t)
exp (VarQuote x) =
  depend (write "'")
         (pretty x)
exp (TypQuote x) =
  depend (write "''")
         (pretty x)
exp (BracketExp b) =
  pretty b
exp (SpliceExp s) =
  pretty s
exp (QuasiQuote n s) =
  brackets (depend (do string n
                       write "|")
                   (do string s
                       write "|"))
exp x@XTag{} = pretty' x
exp x@XETag{} = pretty' x
exp x@XPcdata{} = pretty' x
exp x@XExpTag{} = pretty' x
exp x@XChildTag{} = pretty' x
exp x@Var{} = pretty' x
exp x@IPVar{} = pretty' x
exp x@Con{} = pretty' x
exp x@Lit{} = pretty' x
exp (CorePragma _ _) = error "FIXME: No implementation for CorePragma."
exp (SCCPragma _ _) = error "FIXME: No implementation for SCCPragma."
exp (GenPragma _ _ _ _) = error "FIXME: No implementation for GenPragma."
exp (Proc _ _ _) = error "FIXME: No implementation for Proc."
exp (LeftArrApp _ _) = error "FIXME: No implementation for LeftArrApp."
exp (RightArrApp _ _) = error "FIXME: No implementation for RightArrApp."
exp (LeftArrHighApp _ _) = error "FIXME: No implementation for LeftArrHighApp."
exp (RightArrHighApp _ _) = error "FIXME: No implementation for RightArrHighApp."
exp (LCase _) = error "FIXME: No implementation for LCase."
exp ParComp{} = error "FIXME: No implementation for ParComp."

instance Pretty Stmt where
  pretty x =
    case x of
      Generator _ p e ->
        depend (do pretty p
                   write " <- ")
               (pretty e)
      Qualifier e ->
        pretty e
      LetStmt binds ->
        depend (write "let ")
               (pretty binds)
      RecStmt{} ->
        error "FIXME: No implementation for RecStmt."

instance Pretty QualStmt where
  pretty x =
    case x of
      QualStmt s -> pretty s
      ThenTrans{} -> error "FIXME: No implementation for ThenTrans."
      ThenBy{} -> error "FIXME: No implementation for ThenBy."
      GroupBy{} -> error "FIXME: No implementation for GroupBy."
      GroupUsing{} -> error "FIXME: No implementation for GroupUsing."
      GroupByUsing{} -> error "FIXME: No implementation for GroupByUsing."

instance Pretty Decl where
  pretty = decl

decl :: Decl -> Printer ()
decl (PatBind _ pat mty rhs binds) =
  case mty of
    Just{} -> error "Unimlpemented (Maybe Type) in PatBind."
    Nothing ->
      do pretty pat
         write " = "
         newline
         indented indentSpaces
                  (do pretty rhs
                      unless (nullBinds binds)
                             (do newline
                                 depend (write "where ")
                                        (pretty binds)))
decl (TypeDecl _ _ _ _) =
  error "FIXME: No implementation for TypeDecl."
decl (TypeFamDecl _ _ _ _) =
  error "FIXME: No implementation for TypeFamDecl."
decl (DataDecl _ _ _ _ _ _ _) =
  error "FIXME: No implementation for DataDecl."
decl (GDataDecl _ _ _ _ _ _ _ _) =
  error "FIXME: No implementation for GDataDecl."
decl (DataFamDecl _ _ _ _ _) =
  error "FIXME: No implementation for DataFamDecl."
decl (TypeInsDecl _ _ _) =
  error "FIXME: No implementation for TypeInsDecl."
decl (DataInsDecl _ _ _ _ _) =
  error "FIXME: No implementation for DataInsDecl."
decl (GDataInsDecl _ _ _ _ _ _) =
  error "FIXME: No implementation for GDataInsDecl."
decl (ClassDecl _ _ _ _ _ _) =
  error "FIXME: No implementation for ClassDecl."
decl (InstDecl _ _ _ _ _) =
  error "FIXME: No implementation for InstDecl."
decl (DerivDecl _ _ _ _) =
  error "FIXME: No implementation for DerivDecl."
decl (InfixDecl _ _ _ _) =
  error "FIXME: No implementation for InfixDecl."
decl (DefaultDecl _ _) =
  error "FIXME: No implementation for DefaultDecl."
decl (SpliceDecl _ _) =
  error "FIXME: No implementation for SpliceDecl."
decl (TypeSig _ names ty) =
  depend (do inter (write ", ")
                   (map pretty names)
             write " :: ")
         (pretty ty)
decl (FunBind matches) =
  lined (map pretty matches)
decl (ForImp _ _ _ _ _ _) =
  error "FIXME: No implementation for ForImp."
decl (ForExp _ _ _ _ _) =
  error "FIXME: No implementation for ForExp."
decl (RulePragmaDecl _ _) =
  error "FIXME: No implementation for RulePragmaDecl."
decl (DeprPragmaDecl _ _) =
  error "FIXME: No implementation for DeprPragmaDecl."
decl (WarnPragmaDecl _ _) =
  error "FIXME: No implementation for WarnPragmaDecl."
decl (InlineSig _ _ _ _) =
  error "FIXME: No implementation for InlineSig."
decl (InlineConlikeSig _ _ _) =
  error "FIXME: No implementation for InlineConlikeSig."
decl (SpecSig _ _ _ _) =
  error "FIXME: No implementation for SpecSig."
decl (SpecInlineSig _ _ _ _ _) =
  error "FIXME: No implementation for SpecInlineSig."
decl (InstSig _ _ _ _) =
  error "FIXME: No implementation for InstSig."
decl (AnnPragma _ _) =
  error "FIXME: No implementation for AnnPragma."

instance Pretty Alt where
  pretty x =
    case x of
      Alt _ _ _ _ -> error "FIXME: No implementation for Alt."

instance Pretty Asst where
  pretty x =
    case x of
      ClassA _ _ -> error "FIXME: No implementation for ClassA."
      InfixA _ _ _ -> error "FIXME: No implementation for InfixA."
      IParam _ _ -> error "FIXME: No implementation for IParam."
      EqualP _ _ -> error "FIXME: No implementation for EqualP."

instance Pretty BangType where
  pretty x =
    case x of
      BangedTy _ -> error "FIXME: No implementation for BangedTy."
      UnBangedTy _ -> error "FIXME: No implementation for UnBangedTy."
      UnpackedTy _ -> error "FIXME: No implementation for UnpackedTy."

instance Pretty Binds where
  pretty x =
    case x of
      BDecls ds ->
        lined (map pretty ds)
      IPBinds i ->
        lined (map pretty i)

instance Pretty Bracket where
  pretty x =
    case x of
      ExpBracket _ -> error "FIXME: No implementation for ExpBracket."
      PatBracket _ -> error "FIXME: No implementation for PatBracket."
      TypeBracket _ -> error "FIXME: No implementation for TypeBracket."
      DeclBracket _ -> error "FIXME: No implementation for DeclBracket."

instance Pretty ClassDecl where
  pretty _ =
    error "FIXME: No implementation for ClassDecl."

instance Pretty ConDecl where
  pretty x =
    case x of
      ConDecl _ _ -> error "FIXME: No implementation for ConDecl."
      InfixConDecl _ _ _ -> error "FIXME: No implementation for InfixConDecl."
      RecDecl _ _ -> error "FIXME: No implementation for RecDecl."

instance Pretty FieldUpdate where
  pretty x =
    case x of
      FieldUpdate n e ->
        depend (do pretty n
                   write " = ")
               (pretty e)
      FieldPun n ->
        pretty n
      FieldWildcard ->
        write ".."

instance Pretty GadtDecl where
  pretty x =
    case x of
      GadtDecl _ _ _ -> error "FIXME: No implementation for GadtDecl."

instance Pretty GuardedAlt where
  pretty x =
    case x of
      GuardedAlt _ _ _ -> error "FIXME: No implementation for GuardedAlt."

instance Pretty GuardedAlts where
  pretty x =
    case x of
      UnGuardedAlt _ -> error "FIXME: No implementation for UnGuardedAlt."
      GuardedAlts _ -> error "FIXME: No implementation for GuardedAlts."

instance Pretty GuardedRhs where
  pretty x =
    case x of
      GuardedRhs _ _ _ -> error "FIXME: No implementation for GuardedRhs."

instance Pretty IPBind where
  pretty x =
    case x of
      IPBind _ _ _ -> error "FIXME: No implementation for IPBind."

instance Pretty IfAlt where
  pretty x =
    case x of
      IfAlt _ _ -> error "FIXME: No implementation for IfAlt."

instance Pretty InstDecl where
  pretty _ =  error "FIXME: No implementation for InstDecl."

instance Pretty Match where
  pretty x =
    case x of
      Match _ name pats mty rhs binds ->
        case mty of
          Just{} ->
            error "Unimlpemented (Maybe Type) in Match."
          Nothing ->
            do depend (do pretty name
                          space)
                      (do spaced (map pretty pats)
                          write " = ")
               newline
               indented indentSpaces
                        (do pretty rhs
                            unless (nullBinds binds)
                                   (do newline
                                       depend (write "where ")
                                              (pretty binds)))

nullBinds (BDecls x) = null x
nullBinds (IPBinds x) = null x

instance Pretty Module where
  pretty x =
    case x of
      Module _ _ _ _ _ _ _ -> error "FIXME: No implementation for Module."

instance Pretty PatField where
  pretty x =
    case x of
      PFieldPat n p ->
        depend (do pretty n
                   write " = ")
               (pretty p)
      PFieldPun n ->
        pretty n
      PFieldWildcard ->
        write ".."

instance Pretty QualConDecl where
  pretty x =
    case x of
      QualConDecl _ _ _ _ -> error "FIXME: No implementation for QualConDecl."

instance Pretty Rhs where
  pretty x =
    case x of
      UnGuardedRhs e -> pretty e
      GuardedRhss _ -> error "FIXME: No implementation for GuardedRhss."

instance Pretty Rule where
  pretty x =
    case x of
      Rule _ _ _ _ _ -> error "FIXME: No implementation for Rule."

instance Pretty RuleVar where
  pretty x =
    case x of
      RuleVar _ -> error "FIXME: No implementation for RuleVar."
      TypedRuleVar _ _ -> error "FIXME: No implementation for TypedRuleVar."

instance Pretty Splice where
  pretty x =
    case x of
      IdSplice _ -> error "FIXME: No implementation for IdSplice."
      ParenSplice _ -> error "FIXME: No implementation for ParenSplice."

instance Pretty WarningText where
  pretty x =
    case x of
      DeprText _ -> error "FIXME: No implementation for DeprText."
      WarnText _ -> error "FIXME: No implementation for WarnText."

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
  case parseDeclWithMode parseMode x of
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

-- | Parse mode, includes all extensions, doesn't assume any fixities.
parseMode :: ParseMode
parseMode =
  defaultParseMode { extensions = allExtensions
                   , fixities   = Nothing
                   }
 where allExtensions = filter isDisabledExtention knownExtensions
       isDisabledExtention (DisableExtension _) = False
       isDisabledExtention _                    = True
