{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Instances for pretty printing.

module HIndent.Instances where

import           HIndent.Types
import           HIndent.Combinators

import           Control.Monad.State
import           Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import           Language.Haskell.Exts.Syntax
import           Prelude hiding (exp)

-- | Indent spaces: 2.
indentSpaces :: Int64
indentSpaces = 2

-- | Column limit: 80
columnLimit :: Int64
columnLimit = 80

-- | Column limit: 80
smallColumnLimit :: Int64
smallColumnLimit = 50

-- | Make the right hand side dependent if it's flat, otherwise
-- newline it.
dependOrNewline :: Printer () -> Exp -> (Exp -> Printer ()) -> Printer ()
dependOrNewline left right f =
  do flat <- isFlat right
     small <- isSmall (depend left (f right))
     if flat || small
        then depend left
                    (f right)
        else do left
                newline
                (f right)

-- | Is the expression "short"? Used for app heads.
isShort :: Pretty a => a -> Printer Bool
isShort p =
  do line <- gets psLine
     orig <- gets psColumn
     st <- sandbox (pretty p)
     return (psLine st ==
             line &&
             psColumn st <
             psColumn st +
             10)

-- | Is the given expression "small"? I.e. does it fit on one line and
-- under 'smallColumnLimit' columns.
isSmall :: MonadState PrintState m => m a -> m Bool
isSmall p =
  do line <- gets psLine
     st <- sandbox p
     return (psLine st ==
             line &&
             psColumn st <
             smallColumnLimit)

-- | Does printing the given thing overflow column limit? (e.g. 80)
isOverflow :: MonadState PrintState m => m a -> m Bool
isOverflow p =
  do st <- sandbox p
     return (psColumn st >
             smallColumnLimit)

-- | Is the given expression a single-liner when printed?
isSingleLiner :: MonadState PrintState m => m a -> m Bool
isSingleLiner p =
  do line <- gets psLine
     st <- sandbox p
     return (psLine st ==
             line)

-- | Play with a printer and then restore the state to what it was
-- before.
sandbox :: MonadState s m => m a -> m s
sandbox p =
  do orig <- get
     _ <- p
     new <- get
     put orig
     return new

-- | No binds?
nullBinds :: Binds -> Bool
nullBinds (BDecls x) = null x
nullBinds (IPBinds x) =
  null x

-- | Is an expression flat?
isFlat :: Exp -> Printer Bool
isFlat (Lambda _ _ e) = isFlat e
isFlat (InfixApp a _ b) =
  do a' <- isFlat a
     b' <- isFlat b
     return (a' && b')
isFlat (NegApp a) = isFlat a
isFlat (Paren e) =
  isFlat e
isFlat VarQuote{} = return True
isFlat TypQuote{} =
  return True
isFlat (List []) = return True
isFlat Var{} =
  return True
isFlat Lit{} = return True
isFlat Con{} =
  return True
isFlat _ = return False

instance Pretty Pat where
  pretty x =
    case x of
      PLit l -> pretty l
      PNeg l ->
        depend (write "-")
               (pretty l)
      PNPlusK n k ->
        depend (do pretty n
                   write "-")
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
  pretty x =
    case x of
      TyForall mbinds ctx ty ->
        depend (case mbinds of
                  Nothing -> return ()
                  Just ts ->
                    do write "forall "
                       spaced (map pretty ts)
                       write ". ")
               (depend (case ctx of
                          [] -> return ()
                          (_:_) ->
                            do parens (mapM_ pretty ctx)
                               write " => ")
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
  pretty = exp

-- | Render an expression.
exp :: Exp -> Printer ()
exp e@(InfixApp a op b) =
  do is <- isFlat e
     if is
        then do depend (do pretty a
                           space
                           pretty op
                           space)
                       (do pretty b)
        else do pretty a
                space
                pretty op
                newline
                pretty b
exp (App op a) =
  case flatten op [a] of
    (f,args) ->
      do orig <- gets psIndentLevel
         headIsShort <- isShort f
         depend (do pretty f
                    space)
                (do flatish <- fmap ((< 2) .
                                     length .
                                     filter not)
                                    (mapM isFlat args)
                    singleLiner <- isSingleLiner (spaced (map pretty args))
                    if flatish && singleLiner
                       then spaced (map pretty args)
                       else do allSingleLiners <- fmap (all id)
                                                       (mapM (isSingleLiner . pretty) args)
                               if headIsShort || allSingleLiners
                                  then lined (map pretty args)
                                  else do newline
                                          column (orig + indentSpaces) (lined (map pretty args)))
  where flatten :: Exp -> [Exp] -> (Exp,[Exp])
        flatten (App f a') b = flatten f (a' : b)
        flatten f as = (f,as)
exp (NegApp e) =
  depend (write "-")
         (pretty e)
exp (Lambda _ ps e) =
  depend (write "\\")
         (do spaced (map pretty ps)
             dependOrNewline (write " -> ")
                             e
                             (indented 1 .
                              pretty))
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
  do single <- isSingleLiner p
     underflow <- fmap not (isOverflow p)
     if single && underflow
        then p
        else brackets (prefixedLined ',' (map pretty es))
  where p = brackets (commas (map pretty es))
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
         (braces (prefixedLined ',' (map pretty fs)))
exp (RecUpdate n fs) =
  depend (do pretty n
             space)
         (braces (prefixedLined ',' (map pretty fs)))
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
                       prefixedLined ','
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
  brackets
    (depend
       (do string n
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
exp (CorePragma _ _) =
  error "FIXME: No implementation for CorePragma."
exp (SCCPragma _ _) =
  error "FIXME: No implementation for SCCPragma."
exp (GenPragma _ _ _ _) =
  error "FIXME: No implementation for GenPragma."
exp (Proc _ _ _) =
  error "FIXME: No implementation for Proc."
exp (LeftArrApp _ _) =
  error "FIXME: No implementation for LeftArrApp."
exp (RightArrApp _ _) =
  error "FIXME: No implementation for RightArrApp."
exp (LeftArrHighApp _ _) =
  error "FIXME: No implementation for LeftArrHighApp."
exp (RightArrHighApp _ _) =
  error "FIXME: No implementation for RightArrHighApp."
exp (LCase _) =
  error "FIXME: No implementation for LCase."
exp (MultiIf _) =
  error "FIXME: No implementation for MultiIf."
exp ParComp{} =
  error "FIXME: No implementation for ParComp."

instance Pretty Stmt where
  pretty x =
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
  pretty x =
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
  pretty = decl

-- | Render a declaration.
decl :: Decl -> Printer ()
decl (PatBind _ pat mty rhs binds) =
  case mty of
    Just{} ->
      error "Unimlpemented (Maybe Type) in PatBind."
    Nothing ->
      do pretty pat
         pretty rhs
         unless (nullBinds binds)
                (do newline
                    indented indentSpaces
                             (depend (write "where ")
                                     (pretty binds)))

decl (InstDecl _ ctx name tys decls) =
  do depend (write "instance ")
            (depend (unless (null ctx)
                            (do write "("
                                commas (map pretty ctx)
                                write ") => "))
                    (depend (do pretty name
                                space)
                            (do spaced (map pretty tys)
                                unless (null decls)
                                       (write " where"))))
     unless (null decls)
            (do newline
                indented 2 (mapM_ pretty decls))
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
      Alt _ p galts binds ->
        do pretty p
           pretty galts
           unless (nullBinds binds)
                  (do newline
                      indented indentSpaces
                               (depend (write "where ")
                                       (pretty binds)))

instance Pretty Asst where
  pretty x =
    case x of
      ClassA _ _ ->
        error "FIXME: No implementation for ClassA."
      InfixA _ _ _ ->
        error "FIXME: No implementation for InfixA."
      IParam _ _ ->
        error "FIXME: No implementation for IParam."
      EqualP _ _ ->
        error "FIXME: No implementation for EqualP."

instance Pretty BangType where
  pretty x =
    case x of
      BangedTy _ ->
        error "FIXME: No implementation for BangedTy."
      UnBangedTy _ ->
        error "FIXME: No implementation for UnBangedTy."
      UnpackedTy _ ->
        error "FIXME: No implementation for UnpackedTy."

instance Pretty Binds where
  pretty x =
    case x of
      BDecls ds -> lined (map pretty ds)
      IPBinds i -> lined (map pretty i)

instance Pretty Bracket where
  pretty x =
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
  pretty _ =
    error "FIXME: No implementation for ClassDecl."

instance Pretty ConDecl where
  pretty x =
    case x of
      ConDecl _ _ ->
        error "FIXME: No implementation for ConDecl."
      InfixConDecl _ _ _ ->
        error "FIXME: No implementation for InfixConDecl."
      RecDecl _ _ ->
        error "FIXME: No implementation for RecDecl."

instance Pretty FieldUpdate where
  pretty x =
    case x of
      FieldUpdate n e ->
        dependOrNewline (do pretty n
                            write " = ")
                        e
                        pretty
      FieldPun n -> pretty n
      FieldWildcard -> write ".."

instance Pretty GadtDecl where
  pretty x =
    case x of
      GadtDecl _ _ _ ->
        error "FIXME: No implementation for GadtDecl."

instance Pretty GuardedAlts where
  pretty x =
    case x of
      UnGuardedAlt e ->
        dependOrNewline (write " -> ")
                        e
                        (indented 2 .
                         pretty)
      GuardedAlts gas ->
        do newline
           indented 2
                    (lined (map (\p ->
                                   do write "|"
                                      pretty p)
                                gas))

instance Pretty GuardedAlt where
  pretty x =
    case x of
      GuardedAlt _ stmts e ->
        indented 1
                 (do (prefixedLined ','
                                    (map (\p ->
                                            do space
                                               pretty p)
                                         stmts))
                     dependOrNewline (write " -> ")
                                     e
                                     (indented 1 .
                                      pretty))

instance Pretty GuardedRhs where
  pretty x =
    case x of
      GuardedRhs _ stmts e ->
        indented 1
                 (do prefixedLined ','
                                   (map (\p ->
                                           do space
                                              pretty p)
                                        stmts)
                     dependOrNewline (write " = ")
                                     e
                                     (indented 1 .
                                      pretty))

instance Pretty IPBind where
  pretty x =
    case x of
      IPBind _ _ _ ->
        error "FIXME: No implementation for IPBind."

instance Pretty IfAlt where
  pretty x =
    case x of
      IfAlt _ _ ->
        error "FIXME: No implementation for IfAlt."

instance Pretty InstDecl where
  pretty i =
    case i of
      InsDecl d -> pretty d
      InsType _ name ty ->
        depend (do write "type "
                   pretty name
                   write " = ")
               (pretty ty)
      _ -> pretty' i

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
                      (spaced (map pretty pats))
               pretty rhs
               unless (nullBinds binds)
                      (do newline
                          indented indentSpaces
                                   (depend (write "where ")
                                           (pretty binds)))

instance Pretty Module where
  pretty x =
    case x of
      Module _ _ _ _ _ _ _ ->
        error "FIXME: No implementation for Module."

instance Pretty PatField where
  pretty x =
    case x of
      PFieldPat n p ->
        depend (do pretty n
                   write " = ")
               (pretty p)
      PFieldPun n -> pretty n
      PFieldWildcard -> write ".."

instance Pretty QualConDecl where
  pretty x =
    case x of
      QualConDecl _ _ _ _ ->
        error "FIXME: No implementation for QualConDecl."

instance Pretty Rhs where
  pretty x =
    case x of
      UnGuardedRhs e ->
        indented indentSpaces (dependOrNewline (write " = ") e pretty)
      GuardedRhss gas ->
        do newline
           indented 2
                    (lined (map (\p ->
                                   do write "|"
                                      pretty p)
                                gas))

instance Pretty Rule where
  pretty x =
    case x of
      Rule _ _ _ _ _ ->
        error "FIXME: No implementation for Rule."

instance Pretty RuleVar where
  pretty x =
    case x of
      RuleVar _ ->
        error "FIXME: No implementation for RuleVar."
      TypedRuleVar _ _ ->
        error "FIXME: No implementation for TypedRuleVar."

instance Pretty Splice where
  pretty x =
    case x of
      IdSplice _ ->
        error "FIXME: No implementation for IdSplice."
      ParenSplice _ ->
        error "FIXME: No implementation for ParenSplice."

instance Pretty WarningText where
  pretty x =
    case x of
      DeprText _ ->
        error "FIXME: No implementation for DeprText."
      WarnText _ ->
        error "FIXME: No implementation for WarnText."

instance Pretty Tool where
  pretty x =
    case x of
      GHC -> write "GHC"
      HUGS -> write "HUGS"
      NHC98 -> write "NHC98"
      YHC -> write "YHC"
      HADDOCK -> write "HADDOCK"
      UnknownTool t ->
        write (T.fromText (T.pack t))

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
