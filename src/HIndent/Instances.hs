{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Instances for pretty printing.

module HIndent.Instances () where

import           HIndent.Combinators
import           HIndent.Types

import           Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import           Language.Haskell.Exts.Syntax
import           Prelude hiding (exp)

instance Pretty Pat where
  prettyInternal x =
    case x of
      PLit l -> pretty l
      PNeg l ->
        depend (write "-")
               (pretty l)
      PNPlusK n k ->
        depend (do pretty n
                   write "+")
               (int k)
      PInfixApp a op b ->
        case op of
          Special{} ->
            depend (pretty a)
                   (depend (prettyInfixOp op)
                           (pretty b))
          _ ->
            depend (do pretty a
                       space)
                   (depend (do prettyInfixOp op
                               space)
                           (pretty b))
      PApp f args ->
        depend (do pretty f
                   unless (null args) space)
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
      PParen e -> parens (pretty e)
      PRec qname fields ->
        depend (pretty qname)
               (braces (commas (map pretty fields)))
      PAsPat n p ->
        depend (do pretty n
                   write "@")
               (pretty p)
      PWildCard -> write "_"
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

-- | Pretty print a name for being an infix operator.
prettyInfixOp :: QName -> Printer ()
prettyInfixOp x =
  case x of
    Qual{} -> pretty' x
    UnQual n ->
      case n of
        Ident i -> string ("`" ++ i ++ "`")
        Symbol s -> string s
    Special s ->
      case s of
        UnitCon -> write "()"
        ListCon -> write "[]"
        FunCon -> write "->"
        TupleCon Boxed i ->
          string ("(" ++
                  replicate (i - 1) ',' ++
                  ")")
        TupleCon Unboxed i ->
          string ("(#" ++
                  replicate (i - 1) ',' ++
                  "#)")
        Cons -> write ":"
        UnboxedSingleCon -> write "(##)"

instance Pretty Type where
  prettyInternal x =
    case x of
      TyForall mbinds ctx ty ->
        depend (case mbinds of
                  Nothing -> return ()
                  Just ts ->
                    do write "forall "
                       spaced (map pretty ts)
                       write ". ")
               (depend (maybeCtx ctx)
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
      TyList t -> brackets (pretty t)
      TyApp f a -> spaced [pretty f,pretty a]
      TyVar n -> pretty n
      TyCon p -> pretty p
      TyParen e -> parens (pretty e)
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
  prettyInternal = exp

-- | Render an expression.
exp :: Exp -> Printer ()
exp (InfixApp a op b) =
  depend (do pretty a
             space
             pretty op
             space)
         (do pretty b)
exp (App op a) =
  swing (do pretty f)
        (lined (map pretty args))
  where (f,args) = flatten op [a]
        flatten :: Exp -> [Exp] -> (Exp,[Exp])
        flatten (App f' a') b =
          flatten f' (a' : b)
        flatten f' as = (f',as)
exp (NegApp e) =
  depend (write "-")
         (pretty e)
exp (Lambda _ ps e) =
  depend (write "\\")
         (do spaced (map pretty ps)
             swing (write " -> ")
                   (pretty e))
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
exp (Paren e) = parens (pretty e)
exp (Case e alts) =
  do depend (write "case ")
            (do pretty e
                write " of ")
     newline
     indentSpaces <- getIndentSpaces
     indented indentSpaces (lined (map pretty alts))
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
         (do parens (prefixedLined ','
                                   (map pretty exps))
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
  brackets (prefixedLined ',' (map pretty es))
exp (LeftSection e op) =
  parens (depend (do pretty e
                     space)
                 (pretty op))
exp (RightSection e op) =
  parens (depend (do pretty e
                     space)
                 (pretty op))
exp (RecConstr n fs) =
  do indentSpaces <- getIndentSpaces
     depend (do pretty n
                space)
            (braces (prefixedLined ','
                                   (map (indented indentSpaces . pretty) fs)))
exp (RecUpdate n fs) =
  do indentSpaces <- getIndentSpaces
     depend (do pretty n
                space)
            (braces (prefixedLined ','
                                   (map (indented indentSpaces . pretty) fs)))
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
  brackets (depend (do pretty e
                       unless (null qstmt)
                              (write " |"))
                   (do space
                       prefixedLined
                         ','
                         (map (\(i,x) ->
                                 depend (if i == 0
                                            then return ()
                                            else space)
                                        (pretty x))
                              (zip [0 :: Integer ..] qstmt))))
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
exp (BracketExp b) = pretty b
exp (SpliceExp s) = pretty s
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
exp x@CorePragma{} = pretty' x
exp x@SCCPragma{} = pretty' x
exp x@GenPragma{} = pretty' x
exp x@Proc{} = pretty' x
exp x@LeftArrApp{} = pretty' x
exp x@RightArrApp{} = pretty' x
exp x@LeftArrHighApp{} = pretty' x
exp x@RightArrHighApp{} = pretty' x
exp (LCase _) =
  error "FIXME: No implementation for LCase."
exp (MultiIf _) =
  error "FIXME: No implementation for MultiIf."
exp ParComp{} =
  error "FIXME: No implementation for ParComp."

instance Pretty Stmt where
  prettyInternal x =
    case x of
      Generator _ p e ->
        depend (do pretty p
                   write " <- ")
               (pretty e)
      Qualifier e -> pretty e
      LetStmt binds ->
        depend (write "let ")
               (pretty binds)
      RecStmt{} ->
        error "FIXME: No implementation for RecStmt."

instance Pretty QualStmt where
  prettyInternal x =
    case x of
      QualStmt s -> pretty s
      ThenTrans{} ->
        error "FIXME: No implementation for ThenTrans."
      ThenBy{} ->
        error "FIXME: No implementation for ThenBy."
      GroupBy{} ->
        error "FIXME: No implementation for GroupBy."
      GroupUsing{} ->
        error "FIXME: No implementation for GroupUsing."
      GroupByUsing{} ->
        error "FIXME: No implementation for GroupByUsing."

instance Pretty Decl where
  prettyInternal = decl

-- | Render a declaration.
decl :: Decl -> Printer ()
decl (PatBind _ pat mty rhs binds) =
  case mty of
    Just{} ->
      error "Unimlpemented (Maybe Type) in PatBind."
    Nothing ->
      do pretty pat
         pretty rhs
         indentSpaces <- getIndentSpaces
         unless (nullBinds binds)
                (do newline
                    indented indentSpaces
                             (depend (write "where ")
                                     (pretty binds)))
decl (InstDecl _ ctx name tys decls) =
  do indentSpaces <- getIndentSpaces
     depend (write "instance ")
            (depend (maybeCtx ctx)
                    (depend (do pretty name
                                space)
                            (do spaced (map pretty tys)
                                unless (null decls)
                                       (write " where"))))
     unless (null decls)
            (do newline
                indented indentSpaces (lined (map pretty decls)))
decl (SpliceDecl _ e) = pretty e
decl (TypeSig _ names ty) =
  depend (do inter (write ", ")
                   (map pretty names)
             write " :: ")
         (pretty ty)
decl (FunBind matches) =
  lined (map pretty matches)
decl (ClassDecl _ ctx name tys fundeps decls) =
  do depend (write "class ")
            (depend (maybeCtx ctx)
                    (depend (do pretty name
                                space)
                            (depend (depend (spaced (map pretty tys))
                                            (unless (null fundeps)
                                                    (do write " | "
                                                        commas (map pretty fundeps))))
                                    (unless (null decls)
                                            (write " where")))))
     unless (null decls)
            (do newline
                indentSpaces <- getIndentSpaces
                indented indentSpaces (lined (map pretty decls)))
decl (TypeDecl _ _ _ _) =
  error "FIXME: No implementation for TypeDecl."
decl (TypeFamDecl _ _ _ _) =
  error "FIXME: No implementation for TypeFamDecl."
decl (DataDecl _ dataornew ctx name tyvars condecls _derivs) =
  depend (do pretty dataornew
             space)
         (depend (maybeCtx ctx)
                 (do spaced (pretty name :
                             map pretty tyvars)
                     case condecls of
                       [] -> return ()
                       [x] -> singleCons x
                       xs -> multiCons xs))
  where singleCons x =
          do write " ="
             indentSpaces <- getIndentSpaces
             column indentSpaces
                    (do newline
                        pretty x)
        multiCons xs =
          do newline
             indentSpaces <- getIndentSpaces
             column indentSpaces
                    (depend (write "=")
                            (prefixedLined '|'
                                           (map (depend space . pretty) xs)))
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
decl (DerivDecl _ _ _ _) =
  error "FIXME: No implementation for DerivDecl."
decl (ForImp _ _ _ _ _ _) =
  error "FIXME: No implementation for ForImp."
decl (ForExp _ _ _ _ _) =
  error "FIXME: No implementation for ForExp."
decl (RulePragmaDecl _ _) =
  error "FIXME: No implementation for RulePragmaDecl."
decl (DeprPragmaDecl _ _) =
  error "FIXME: No implementation for DeprPragmaDecl."
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
decl x@WarnPragmaDecl{} = pretty' x
decl x@AnnPragma{} = pretty' x
decl x@InfixDecl{} = pretty' x
decl x@DefaultDecl{} = pretty' x

instance Pretty Alt where
  prettyInternal x =
    case x of
      Alt _ p galts binds ->
        do pretty p
           pretty galts
           unless (nullBinds binds)
                  (do newline
                      indentSpaces <- getIndentSpaces
                      indented indentSpaces
                               (depend (write "where ")
                                       (pretty binds)))

instance Pretty Asst where
  prettyInternal x =
    case x of
      ClassA name types ->
        spaced (pretty name : map pretty types)
      InfixA _ _ _ ->
        error "FIXME: No implementation for InfixA."
      IParam _ _ ->
        error "FIXME: No implementation for IParam."
      EqualP _ _ ->
        error "FIXME: No implementation for EqualP."

instance Pretty BangType where
  prettyInternal x =
    case x of
      BangedTy ty ->
        depend (write "!")
               (pretty ty)
      UnBangedTy ty ->
        pretty ty
      UnpackedTy ty ->
        depend (write "{-# UNPACK #-} !")
               (pretty ty)

instance Pretty Binds where
  prettyInternal x =
    case x of
      BDecls ds -> lined (map pretty ds)
      IPBinds i -> lined (map pretty i)

instance Pretty Bracket where
  prettyInternal x =
    case x of
      ExpBracket _ ->
        error "FIXME: No implementation for ExpBracket."
      PatBracket _ ->
        error "FIXME: No implementation for PatBracket."
      TypeBracket _ ->
        error "FIXME: No implementation for TypeBracket."
      DeclBracket _ ->
        error "FIXME: No implementation for DeclBracket."

instance Pretty ClassDecl where
  prettyInternal x =
    case x of
      ClsDecl d -> pretty d
      ClsDataFam _ ctx n tyvars mkind ->
        depend (write "data ")
               (depend (maybeCtx ctx)
                       (do spaced (pretty n :
                                   map pretty tyvars)
                           case mkind of
                             Nothing -> return ()
                             Just kind ->
                               do write " :: "
                                  pretty kind))
      ClsTyFam _ n tyvars mkind ->
        depend (write "type ")
               (do spaced (pretty n :
                           map pretty tyvars)
                   case mkind of
                     Nothing -> return ()
                     Just kind ->
                       do write " :: "
                          pretty kind)
      ClsTyDef _ this that ->
        do write "type "
           pretty this
           write " = "
           pretty that

instance Pretty ConDecl where
  prettyInternal x =
    case x of
      ConDecl name bangty ->
        depend (do pretty name
                   space)
               (lined (map pretty bangty))
      InfixConDecl a f b ->
        pretty (ConDecl f [a,b])
      RecDecl name fields ->
        depend (pretty name)
               (do space
                   indentSpaces <- getIndentSpaces
                   braces (prefixedLined
                             ','
                             (map (indented indentSpaces . pretty)
                                  (concatMap (\(names,ty) ->
                                                map (,ty) names)
                                             fields))))

instance Pretty (Name,BangType) where
  prettyInternal (name,ty) =
    depend (do pretty name
               write " :: ")
           (pretty ty)

instance Pretty FieldUpdate where
  prettyInternal x =
    case x of
      FieldUpdate n e ->
        swing (do pretty n
                  write " = ")
              (pretty e)
      FieldPun n -> pretty n
      FieldWildcard -> write ".."

instance Pretty GadtDecl where
  prettyInternal x =
    case x of
      GadtDecl _ _ _ ->
        error "FIXME: No implementation for GadtDecl."

instance Pretty GuardedAlts where
  prettyInternal x =
    case x of
      UnGuardedAlt e ->
        swing (write " -> ")
              (pretty e)
      GuardedAlts gas ->
        do newline
           indented 2
                    (lined (map (\p ->
                                   do write "|"
                                      pretty p)
                                gas))

instance Pretty GuardedAlt where
  prettyInternal x =
    case x of
      GuardedAlt _ stmts e ->
        do indented 1
                    (do (prefixedLined
                           ','
                           (map (\p ->
                                   do space
                                      pretty p)
                                stmts)))
           swing (write " -> ")
                 (pretty e)

instance Pretty GuardedRhs where
  prettyInternal x =
    case x of
      GuardedRhs _ stmts e ->
        do indented 1
                    (do prefixedLined
                          ','
                          (map (\p ->
                                  do space
                                     pretty p)
                               stmts))
           swing (write " = ")
                 (pretty e)

instance Pretty IPBind where
  prettyInternal x =
    case x of
      IPBind _ _ _ ->
        error "FIXME: No implementation for IPBind."

instance Pretty IfAlt where
  prettyInternal x =
    case x of
      IfAlt _ _ ->
        error "FIXME: No implementation for IfAlt."

instance Pretty InstDecl where
  prettyInternal i =
    case i of
      InsDecl d -> pretty d
      InsType _ name ty ->
        depend (do write "type "
                   pretty name
                   write " = ")
               (pretty ty)
      _ -> pretty' i

instance Pretty Match where
  prettyInternal x =
    case x of
      Match _ name pats mty rhs binds ->
        case mty of
          Just{} ->
            error "Unimlpemented (Maybe Type) in Match."
          Nothing ->
            do depend (do pretty name
                          space)
                      (spaced (map pretty pats))
               pretty rhs
               unless (nullBinds binds)
                      (do newline
                          indentSpaces <- getIndentSpaces
                          indented indentSpaces
                                   (depend (write "where ")
                                           (pretty binds)))

instance Pretty Module where
  prettyInternal x =
    case x of
      Module _ _ _ _ _ _ _ ->
        error "FIXME: No implementation for Module."

instance Pretty PatField where
  prettyInternal x =
    case x of
      PFieldPat n p ->
        depend (do pretty n
                   write " = ")
               (pretty p)
      PFieldPun n -> pretty n
      PFieldWildcard -> write ".."

instance Pretty QualConDecl where
  prettyInternal x =
    case x of
      QualConDecl _ tyvars ctx d ->
        depend (unless (null tyvars)
                       (do write "forall "
                           spaced (map pretty tyvars)
                           write ". "))
               (depend (maybeCtx ctx)
                       (pretty d))

instance Pretty Rhs where
  prettyInternal x =
    case x of
      UnGuardedRhs e ->
        (swing (write " = ")
               (pretty e))
      GuardedRhss gas ->
        do newline
           indented 2
                    (lined (map (\p ->
                                   do write "|"
                                      pretty p)
                                gas))

instance Pretty Rule where
  prettyInternal x =
    case x of
      Rule _ _ _ _ _ ->
        error "FIXME: No implementation for Rule."

instance Pretty RuleVar where
  prettyInternal x =
    case x of
      RuleVar _ ->
        error "FIXME: No implementation for RuleVar."
      TypedRuleVar _ _ ->
        error "FIXME: No implementation for TypedRuleVar."

instance Pretty Splice where
  prettyInternal x =
    case x of
      IdSplice _ ->
        error "FIXME: No implementation for IdSplice."
      ParenSplice e ->
        depend (write "$")
               (parens (pretty e))

instance Pretty WarningText where
  prettyInternal x =
    case x of
      DeprText _ ->
        error "FIXME: No implementation for DeprText."
      WarnText _ ->
        error "FIXME: No implementation for WarnText."

instance Pretty Tool where
  prettyInternal x =
    case x of
      GHC -> write "GHC"
      HUGS -> write "HUGS"
      NHC98 -> write "NHC98"
      YHC -> write "YHC"
      HADDOCK -> write "HADDOCK"
      UnknownTool t ->
        write (T.fromText (T.pack t))

instance Pretty Activation where
  prettyInternal = pretty'

instance Pretty Annotation where
  prettyInternal = pretty'

instance Pretty Assoc where
  prettyInternal = pretty'

instance Pretty CName where
  prettyInternal = pretty'

instance Pretty CallConv where
  prettyInternal = pretty'

instance Pretty DataOrNew where
  prettyInternal = pretty'

instance Pretty ExportSpec where
  prettyInternal = pretty'

instance Pretty FunDep where
  prettyInternal = pretty'

instance Pretty IPName where
  prettyInternal = pretty'

instance Pretty ImportSpec where
  prettyInternal = pretty'

instance Pretty ImportDecl where
  prettyInternal = pretty'

instance Pretty Kind where
  prettyInternal = pretty'

instance Pretty Literal where
  prettyInternal = pretty'

instance Pretty ModulePragma where
  prettyInternal = pretty'

instance Pretty Name where
  prettyInternal = pretty'

instance Pretty Op where
  prettyInternal = pretty'

instance Pretty PXAttr where
  prettyInternal = pretty'

instance Pretty Promoted where
  prettyInternal = pretty'

instance Pretty QName where
  prettyInternal = pretty'

instance Pretty QOp where
  prettyInternal = pretty'

instance Pretty RPat where
  prettyInternal = pretty'

instance Pretty RPatOp where
  prettyInternal = pretty'

instance Pretty Safety where
  prettyInternal = pretty'

instance Pretty SpecialCon where
  prettyInternal = pretty'

instance Pretty TyVarBind where
  prettyInternal = pretty'

instance Pretty XAttr where
  prettyInternal = pretty'

instance Pretty XName where
  prettyInternal = pretty'
