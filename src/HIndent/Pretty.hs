{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pretty printing.

module HIndent.Pretty
  (
  -- * Printing
    Pretty
  , pretty
  , prettyNoExt
  -- * Insertion
  , write
  , newline
  , space
  , comma
  , int
  , string
  -- * Common node types
  , maybeCtx
  , printComment
  -- * Interspersing
  , inter
  , spaced
  , lined
  , prefixedLined
  , commas
  -- * Wrapping
  , parens
  , brackets
  , braces
  -- * Indentation
  , indented
  , column
  , getColumn
  , depend
  , dependBind
  , swing
  , getIndentSpaces
  , getColumnLimit
  -- * Predicates
  , nullBinds
  -- * Sandboxing
  , sandbox
  -- * Fallback
  , pretty'
  )
  where

import           HIndent.Types

import           Language.Haskell.Exts.Comments
import           Control.Monad.State hiding (state)
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import           Data.Text.Lazy.Builder.Int
import           Data.Typeable
import qualified Language.Haskell.Exts.Annotated as P
import           Language.Haskell.Exts.Annotated.Syntax
import           Language.Haskell.Exts.SrcLoc
import           Prelude hiding (exp)

--------------------------------------------------------------------------------
-- * Pretty printing class

-- | Pretty printing class.
class (Annotated ast,Typeable1 ast) => Pretty ast where
  prettyInternal :: ast NodeInfo -> Printer ()

-- | Pretty print using extenders.
pretty :: (Pretty ast)
       => ast NodeInfo -> Printer ()
pretty a =
  do st <- get
     case st of
       PrintState{psExtenders = es,psUserState = s} ->
         do case listToMaybe (mapMaybe (makePrinter s) es) of
              Just m -> m
              Nothing -> prettyNoExt a
            printComments a
  where makePrinter s (Extender f) =
          case cast a of
            Just v -> Just (f s v)
            Nothing -> Nothing
        makePrinter s (CatchAll f) = f s a

-- | Run the basic printer for the given node without calling an
-- extension hook for this node, but do allow extender hooks in child
-- nodes. Also auto-inserts comments.
prettyNoExt :: (Pretty ast)
            => ast NodeInfo -> Printer ()
prettyNoExt = prettyInternal

-- | Print comments of a node.
printComments :: (Pretty ast)
              => ast NodeInfo -> Printer ()
printComments ast =
  mapM_ (printComment (Just (srcInfoSpan (nodeInfoSpan info)))) comments
  where info = ann ast
        comments = nodeInfoComments info

-- | Pretty print a comment.
printComment :: Maybe SrcSpan -> ComInfo -> Printer ()
printComment mayNodespan (ComInfo (Comment inline cspan str) own) =
  do when own newline
     -- Insert proper amount of space before comment.
     -- This maintains alignment. This cannot force comments
     -- to go before the left-most possible indent (specified by depends).
     case mayNodespan of
       Just nodespan ->
         do let neededSpaces = srcSpanStartColumn cspan -
                               srcSpanEndColumn nodespan
            replicateM_ neededSpaces space
       Nothing -> return ()
     if inline
        then do write "{-"
                string str
                write "-}"
        else do write "--"
                string str
                modify (\s -> s {psEolComment = True})

-- | Pretty print using HSE's own printer. The 'P.Pretty' class here
-- is HSE's.
pretty' :: (Pretty ast, P.Pretty (ast SrcSpanInfo), Functor ast)
        => ast NodeInfo -> Printer ()
pretty' = write . T.fromText . T.pack . P.prettyPrint . fmap nodeInfoSpan

--------------------------------------------------------------------------------
-- * Combinators

-- | Increase indentation level by n spaces for the given printer.
indented :: Int64 -> Printer a -> Printer a
indented i p =
  do level <- gets psIndentLevel
     modify (\s -> s {psIndentLevel = level + i})
     m <- p
     modify (\s -> s {psIndentLevel = level})
     return m

-- | Print all the printers separated by spaces.
spaced :: [Printer ()] -> Printer ()
spaced = inter space

-- | Print all the printers separated by commas.
commas :: [Printer ()] -> Printer ()
commas = inter comma

-- | Print all the printers separated by sep.
inter :: Printer () -> [Printer ()] -> Printer ()
inter sep ps =
  foldr (\(i,p) next ->
           depend (do p
                      if i < length ps
                         then sep
                         else return ())
                  next)
        (return ())
        (zip [1 ..] ps)

-- | Print all the printers separated by newlines.
lined :: [Printer ()] -> Printer ()
lined ps = sequence_ (intersperse newline ps)

-- | Print all the printers separated newlines and optionally a line
-- prefix.
prefixedLined :: Text -> [Printer ()] -> Printer ()
prefixedLined pref ps' =
  case ps' of
    [] -> return ()
    (p:ps) ->
      do p
         indented (fromIntegral
                     (T.length pref *
                      (-1)))
                  (mapM_ (\p' ->
                            do newline
                               depend (write (T.fromText pref)) p')
                         ps)

-- | Set the (newline-) indent level to the given column for the given
-- printer.
column :: Int64 -> Printer a -> Printer a
column i p =
  do level <- gets psIndentLevel
     modify (\s -> s {psIndentLevel = i})
     m <- p
     modify (\s -> s {psIndentLevel = level})
     return m

-- | Get the current indent level.
getColumn :: Printer Int64
getColumn = gets psColumn

-- | Output a newline.
newline :: Printer ()
newline =
  do write "\n"
     modify (\s -> s {psNewline = True})

-- | Make the latter's indentation depend upon the end column of the
-- former.
depend :: Printer () -> Printer b -> Printer b
depend maker dependent =
  do state' <- get
     maker
     st <- get
     col <- gets psColumn
     if state' /= st
        then column col dependent
        else dependent

-- | Make the latter's indentation depend upon the end column of the
-- former.
dependBind :: Printer a -> (a -> Printer b) -> Printer b
dependBind maker dependent =
  do state' <- get
     v <- maker
     st <- get
     col <- gets psColumn
     if state' /= st
        then column col (dependent v)
        else (dependent v)

-- | Wrap in parens.
parens :: Printer a -> Printer a
parens p =
  depend (write "(")
         (do v <- p
             write ")"
             return v)

-- | Wrap in braces.
braces :: Printer a -> Printer a
braces p =
  depend (write "{")
         (do v <- p
             write "}"
             return v)

-- | Wrap in brackets.
brackets :: Printer a -> Printer a
brackets p =
  depend (write "[")
         (do v <- p
             write "]"
             return v)

-- | Write a space.
space :: Printer ()
space = write " "

-- | Write a comma.
comma :: Printer ()
comma = write ","

-- | Write an integral.
int :: Integral n
    => n -> Printer ()
int = write . decimal

-- | Write out a string, updating the current position information.
write :: Builder -> Printer ()
write x =
  do eol <- gets psEolComment
     when (eol && x /= "\n") newline
     state <- get
     let clearEmpty = configClearEmptyLines $ psConfig state
         writingNewline = x == "\n"
         out =
           if psNewline state && not (clearEmpty && writingNewline)
              then T.fromText
                     (T.replicate (fromIntegral (psIndentLevel state))
                                  " ") <>
                   x
              else x
         out' = T.toLazyText out
     modify (\s ->
               s {psOutput = psOutput state <> out
                 ,psNewline = False
                 ,psEolComment = False
                 ,psLine = psLine state + additionalLines
                 ,psColumn =
                    if additionalLines > 0
                       then LT.length (LT.concat (take 1 (reverse srclines)))
                       else psColumn state + LT.length out'})
  where x' = T.toLazyText x
        srclines = LT.lines x'
        additionalLines =
          LT.length (LT.filter (== '\n') x')

-- | Write a string.
string :: String -> Printer ()
string = write . T.fromText . T.pack

-- | Indent spaces, e.g. 2.
getIndentSpaces :: Printer Int64
getIndentSpaces =
  gets (configIndentSpaces . psConfig)

-- | Column limit, e.g. 80
getColumnLimit :: Printer Int64
getColumnLimit =
  gets (configMaxColumns . psConfig)

-- | Play with a printer and then restore the state to what it was
-- before.
sandbox :: MonadState s m
        => m a -> m (a,s)
sandbox p =
  do orig <- get
     a <- p
     new <- get
     put orig
     return (a,new)

-- | No binds?
nullBinds :: Binds NodeInfo -> Bool
nullBinds (BDecls _ x) = null x
nullBinds (IPBinds _ x) = null x

-- | Maybe render a class context.
maybeCtx :: Maybe (Context NodeInfo) -> Printer ()
maybeCtx =
  maybe (return ())
        (\p ->
           pretty p >>
           write " => ")

-- | Swing the second printer below and indented with respect to the first.
swing :: Printer () -> Printer b -> Printer b
swing a b =
  do orig <- gets psIndentLevel
     a
     newline
     indentSpaces <- getIndentSpaces
     column (orig + indentSpaces) b

--------------------------------------------------------------------------------
-- * Instances

instance Pretty Context where
  prettyInternal ctx =
    case ctx of
      CxSingle _ a -> pretty a
      CxTuple _ as ->
        parens (commas (map pretty as))
      CxParen _ c -> parens (pretty c)
      CxEmpty _ -> parens (return ())

instance Pretty Pat where
  prettyInternal x =
    case x of
      PLit _ l -> pretty l
      PNeg _ l ->
        depend (write "-")
               (pretty l)
      PNPlusK _ n k ->
        depend (do pretty n
                   write "+")
               (int k)
      PInfixApp _ a op b ->
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
      PApp _ f args ->
        depend (do pretty f
                   unless (null args) space)
               (spaced (map pretty args))
      PTuple _ boxed pats ->
        depend (write (case boxed of
                         Unboxed -> "(#"
                         Boxed -> "("))
               (do commas (map pretty pats)
                   write (case boxed of
                            Unboxed -> "#)"
                            Boxed -> ")"))
      PList _ ps ->
        brackets (commas (map pretty ps))
      PParen _ e -> parens (pretty e)
      PRec _ qname fields ->
        depend (pretty qname)
               (braces (commas (map pretty fields)))
      PAsPat _ n p ->
        depend (do pretty n
                   write "@")
               (pretty p)
      PWildCard _ -> write "_"
      PIrrPat _ p ->
        depend (write "~")
               (pretty p)
      PatTypeSig _ p ty ->
        depend (do pretty p
                   write " :: ")
               (pretty ty)
      PViewPat _ e p ->
        depend (do pretty e
                   write " -> ")
               (pretty p)
      PQuasiQuote _ name str ->
        brackets (depend (do write "$"
                             string name
                             write "|")
                         (string str))
      PBangPat _ p ->
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
prettyInfixOp :: QName NodeInfo -> Printer ()
prettyInfixOp x =
  case x of
    Qual{} -> pretty' x
    UnQual _ n ->
      case n of
        Ident _ i -> string ("`" ++ i ++ "`")
        Symbol _ s -> string s
    Special _ s -> pretty s

instance Pretty Type where
  prettyInternal x =
    case x of
      TyForall _ mbinds ctx ty ->
        depend (case mbinds of
                  Nothing -> return ()
                  Just ts ->
                    do write "forall "
                       spaced (map pretty ts)
                       write ". ")
               (depend (maybeCtx ctx)
                       (pretty ty))
      TyFun _ a b ->
        depend (do pretty a
                   write " -> ")
               (pretty b)
      TyTuple _ boxed tys ->
        depend (write (case boxed of
                         Unboxed -> "(#"
                         Boxed -> "("))
               (do commas (map pretty tys)
                   write (case boxed of
                            Unboxed -> "#)"
                            Boxed -> ")"))
      TyList _ t -> brackets (pretty t)
      TyApp _ f a -> spaced [pretty f,pretty a]
      TyVar _ n -> pretty n
      TyCon _ p -> pretty p
      TyParen _ e -> parens (pretty e)
      TyInfix _ a op b ->
        depend (do pretty a
                   space)
               (depend (do pretty op
                           space)
                       (pretty b))
      TyKind _ ty k ->
        parens (do pretty ty
                   write " :: "
                   pretty k)
      TyPromoted{} ->
        error "FIXME: No implementation for TyPromoted."

instance Pretty Exp where
  prettyInternal = exp

-- | Render an expression.
exp :: Exp NodeInfo -> Printer ()
exp (InfixApp _ a op b) =
  depend (do pretty a
             space
             pretty op
             space)
         (do pretty b)
exp (App _ op a) =
  swing (do pretty f)
        (lined (map pretty args))
  where (f,args) = flatten op [a]
        flatten :: Exp NodeInfo
                -> [Exp NodeInfo]
                -> (Exp NodeInfo,[Exp NodeInfo])
        flatten (App _ f' a') b =
          flatten f' (a' : b)
        flatten f' as = (f',as)
exp (NegApp _ e) =
  depend (write "-")
         (pretty e)
exp (Lambda _ ps e) =
  depend (write "\\")
         (do spaced (map pretty ps)
             swing (write " -> ")
                   (pretty e))
exp (Let _ binds e) =
  do depend (write "let ")
            (pretty binds)
     newline
     depend (write "in ")
            (pretty e)
exp (If _ p t e) =
  do depend (write "if ")
            (do pretty p
                newline
                depend (write "then ")
                       (pretty t)
                newline
                depend (write "else ")
                       (pretty e))
exp (Paren _ e) = parens (pretty e)
exp (Case _ e alts) =
  do depend (write "case ")
            (do pretty e
                write " of")
     newline
     indentSpaces <- getIndentSpaces
     indented indentSpaces (lined (map pretty alts))
exp (Do _ stmts) =
  depend (write "do ")
         (lined (map pretty stmts))
exp (MDo _ stmts) =
  depend (write "mdo ")
         (lined (map pretty stmts))
exp (Tuple _ boxed exps) =
  depend (write (case boxed of
                   Unboxed -> "(#"
                   Boxed -> "("))
         (do prefixedLined ","
                           (map pretty exps)
             write (case boxed of
                      Unboxed -> "#)"
                      Boxed -> ")"))
exp (TupleSection _ boxed mexps) =
  depend (write (case boxed of
                   Unboxed -> "(#"
                   Boxed -> "("))
         (do commas (map (maybe (return ()) pretty) mexps)
             write (case boxed of
                      Unboxed -> "#)"
                      Boxed -> ")"))
exp (List _ es) =
  brackets (prefixedLined ","
                          (map pretty es))
exp (LeftSection _ e op) =
  parens (depend (do pretty e
                     space)
                 (pretty op))
exp (RightSection _ e op) =
  parens (depend (do pretty e
                     space)
                 (pretty op))
exp (RecConstr _ n fs) =
  do indentSpaces <- getIndentSpaces
     depend (do pretty n
                space)
            (braces (prefixedLined ","
                                   (map (indented indentSpaces . pretty) fs)))
exp (RecUpdate _ n fs) =
  do indentSpaces <- getIndentSpaces
     depend (do pretty n
                space)
            (braces (prefixedLined ","
                                   (map (indented indentSpaces . pretty) fs)))
exp (EnumFrom _ e) =
  brackets (do pretty e
               write " ..")
exp (EnumFromTo _ e f) =
  brackets (depend (do pretty e
                       write " .. ")
                   (pretty f))
exp (EnumFromThen _ e t) =
  brackets (depend (do pretty e
                       write ",")
                   (do pretty t
                       write " .."))
exp (EnumFromThenTo _ e t f) =
  brackets (depend (do pretty e
                       write ",")
                   (depend (do pretty t
                               write " .. ")
                           (pretty f)))
exp (ListComp _ e qstmt) =
  brackets (depend (do pretty e
                       unless (null qstmt)
                              (write " |"))
                   (do space
                       prefixedLined
                         ","
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
exp (VarQuote _ x) =
  depend (write "'")
         (pretty x)
exp (TypQuote _ x) =
  depend (write "''")
         (pretty x)
exp (BracketExp _ b) = pretty b
exp (SpliceExp _ s) = pretty s
exp (QuasiQuote _ n s) =
  brackets (depend (do string n
                       write "|")
                   (do string s
                       write "|"))
exp (LCase _ alts) =
  do write "\\case"
     indentSpaces <- getIndentSpaces
     newline
     indented indentSpaces (lined (map pretty alts))
exp (MultiIf _ alts) =
  depend (write "if ")
         (lined (map (\p ->
                        do write "| "
                           pretty p)
                     alts))
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
exp ParComp{} =
  error "FIXME: No implementation for ParComp."

instance Pretty IfAlt where
  prettyInternal (IfAlt _ cond body) =
    do pretty cond
       swing (write " -> ")
             (pretty body)

instance Pretty Stmt where
  prettyInternal x =
    case x of
      Generator _ p e ->
        depend (do pretty p
                   write " <- ")
               (pretty e)
      Qualifier _ e -> pretty e
      LetStmt _ binds ->
        depend (write "let ")
               (pretty binds)
      RecStmt{} ->
        error "FIXME: No implementation for RecStmt."

instance Pretty QualStmt where
  prettyInternal x =
    case x of
      QualStmt _ s -> pretty s
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
decl :: Decl NodeInfo -> Printer ()
decl (PatBind _ pat mty rhs mbinds) =
  case mty of
    Just e ->
      error ("Unimplemented (Maybe Type) in PatBind." ++ show e)
    Nothing ->
      do pretty pat
         pretty rhs
         indentSpaces <- getIndentSpaces
         case mbinds of
           Nothing -> return ()
           Just binds ->
             do newline
                indented indentSpaces
                         (depend (write "where ")
                                 (pretty binds))
decl (InstDecl _ ctx dhead decls) =
  do indentSpaces <- getIndentSpaces
     depend (write "instance ")
            (depend (maybeCtx ctx)
                    (depend (pretty dhead)
                            (unless (null (fromMaybe [] decls))
                                    (write " where"))))
     unless (null (fromMaybe [] decls))
            (do newline
                indented indentSpaces (lined (map pretty (fromMaybe [] decls))))
decl (SpliceDecl _ e) = pretty e
decl (TypeSig _ names ty) =
  depend (do inter (write ", ")
                   (map pretty names)
             write " :: ")
         (pretty ty)
decl (FunBind _ matches) =
  lined (map pretty matches)
decl (ClassDecl _ ctx dhead fundeps decls) =
  do depend (write "class ")
            (depend (maybeCtx ctx)
                    (depend (do pretty dhead
                                space)
                            (depend (unless (null fundeps)
                                            (do write " | "
                                                commas (map pretty fundeps)))
                                    (unless (null (fromMaybe [] decls))
                                            (write " where")))))
     unless (null (fromMaybe [] decls))
            (do newline
                indentSpaces <- getIndentSpaces
                indented indentSpaces (lined (map pretty (fromMaybe [] decls))))
decl TypeDecl{} =
  error "FIXME: No implementation for TypeDecl."
decl TypeFamDecl{} =
  error "FIXME: No implementation for TypeFamDecl."
decl (DataDecl _ dataornew ctx dhead condecls mderivs) =
  do depend (do pretty dataornew
                space)
            (depend (maybeCtx ctx)
                    (do pretty dhead
                        case condecls of
                          [] -> return ()
                          [x] -> singleCons x
                          xs -> multiCons xs))
     indentSpaces <- getIndentSpaces
     case mderivs of
       Nothing -> return ()
       Just derivs ->
         do newline
            column indentSpaces (pretty derivs)
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
                            (prefixedLined "|"
                                           (map (depend space . pretty) xs)))
decl GDataDecl{} =
  error "FIXME: No implementation for GDataDecl."
decl DataFamDecl{} =
  error "FIXME: No implementation for DataFamDecl."
decl TypeInsDecl{} =
  error "FIXME: No implementation for TypeInsDecl."
decl DataInsDecl{} =
  error "FIXME: No implementation for DataInsDecl."
decl GDataInsDecl{} =
  error "FIXME: No implementation for GDataInsDecl."
decl DerivDecl{} =
  error "FIXME: No implementation for DerivDecl."
decl ForImp{} =
  error "FIXME: No implementation for ForImp."
decl ForExp{} =
  error "FIXME: No implementation for ForExp."
decl RulePragmaDecl{} =
  error "FIXME: No implementation for RulePragmaDecl."
decl DeprPragmaDecl{} =
  error "FIXME: No implementation for DeprPragmaDecl."
decl InlineSig{} =
  error "FIXME: No implementation for InlineSig."
decl InlineConlikeSig{} =
  error "FIXME: No implementation for InlineConlikeSig."
decl SpecSig{} =
  error "FIXME: No implementation for SpecSig."
decl SpecInlineSig{} =
  error "FIXME: No implementation for SpecInlineSig."
decl InstSig{} =
  error "FIXME: No implementation for InstSig."
decl x@WarnPragmaDecl{} = pretty' x
decl x@AnnPragma{} = pretty' x
decl x@InfixDecl{} = pretty' x
decl x@DefaultDecl{} = pretty' x

instance Pretty Deriving where
  prettyInternal (Deriving _ heads) =
    do write "deriving"
       space
       parens (commas (map pretty heads))

instance Pretty Alt where
  prettyInternal x =
    case x of
      Alt _ p galts mbinds ->
        do pretty p
           pretty galts
           case mbinds of
             Nothing -> return ()
             Just binds ->
               do newline
                  indentSpaces <- getIndentSpaces
                  indented indentSpaces
                           (depend (write "where ")
                                   (pretty binds))

instance Pretty Asst where
  prettyInternal x =
    case x of
      ClassA _ name types ->
        spaced (pretty name :
                map pretty types)
      InfixA{} ->
        error "FIXME: No implementation for InfixA."
      IParam{} ->
        error "FIXME: No implementation for IParam."
      EqualP _ a b ->
        do pretty a
           write " ~ "
           pretty b

instance Pretty BangType where
  prettyInternal x =
    case x of
      BangedTy _ ty ->
        depend (write "!")
               (pretty ty)
      UnBangedTy _ ty -> pretty ty
      UnpackedTy _ ty ->
        depend (write "{-# UNPACK #-} !")
               (pretty ty)

instance Pretty Binds where
  prettyInternal x =
    case x of
      BDecls _ ds -> lined (map pretty ds)
      IPBinds _ i -> lined (map pretty i)

instance Pretty ClassDecl where
  prettyInternal x =
    case x of
      ClsDecl _ d -> pretty d
      ClsDataFam _ ctx h mkind ->
        depend (write "data ")
               (depend (maybeCtx ctx)
                       (do pretty h
                           (case mkind of
                              Nothing ->
                                return ()
                              Just kind ->
                                do write " :: "
                                   pretty kind)))
      ClsTyFam _ h mkind ->
        depend (write "type ")
               (depend (pretty h)
                       (case mkind of
                          Nothing -> return ()
                          Just kind ->
                            do write " :: "
                               pretty kind))
      ClsTyDef _ this that ->
        do write "type "
           pretty this
           write " = "
           pretty that

instance Pretty ConDecl where
  prettyInternal x =
    case x of
      ConDecl _ name bangty ->
        depend (do pretty name
                   space)
               (lined (map pretty bangty))
      InfixConDecl l a f b ->
        pretty (ConDecl l f [a,b])
      RecDecl _ name fields ->
        depend (do pretty name
                   write " ")
               (do depend (write "{")
                          (prefixedLined ","
                                         (map pretty fields))
                   write "}")

instance Pretty FieldDecl where
  prettyInternal (FieldDecl _ names ty) =
    depend (do commas (map pretty names)
               write " :: ")
           (pretty ty)

instance Pretty FieldUpdate where
  prettyInternal x =
    case x of
      FieldUpdate _ n e ->
        swing (do pretty n
                  write " = ")
              (pretty e)
      FieldPun _ n -> pretty n
      FieldWildcard _ -> write ".."

instance Pretty GuardedAlts where
  prettyInternal x =
    case x of
      UnGuardedAlt _ e ->
        swing (write " -> ")
              (pretty e)
      GuardedAlts _ gas ->
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
                           ","
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
                          ","
                          (map (\p ->
                                  do space
                                     pretty p)
                               stmts))
           swing (write " = ")
                 (pretty e)

instance Pretty InstDecl where
  prettyInternal i =
    case i of
      InsDecl _ d -> pretty d
      InsType _ name ty ->
        depend (do write "type "
                   pretty name
                   write " = ")
               (pretty ty)
      _ -> pretty' i

instance Pretty Match where
  prettyInternal x =
    case x of
      Match _ name pats rhs mbinds ->
        do depend (do pretty name
                      space)
                  (spaced (map pretty pats))
           pretty rhs
           case mbinds of
             Nothing -> return ()
             Just binds ->
               do newline
                  indentSpaces <- getIndentSpaces
                  indented indentSpaces
                           (depend (write "where ")
                                   (pretty binds))
      InfixMatch _ pat1 name pats rhs mbinds ->
        do depend (do pretty pat1
                      space
                      case name of
                        Ident _ i ->
                          string ("`" ++ i ++ "`")
                        Symbol _ s -> string s)
                  (do space
                      spaced (map pretty pats))
           pretty rhs
           case mbinds of
             Nothing -> return ()
             Just binds ->
               do newline
                  indentSpaces <- getIndentSpaces
                  indented indentSpaces
                           (depend (write "where ")
                                   (pretty binds))

instance Pretty PatField where
  prettyInternal x =
    case x of
      PFieldPat _ n p ->
        depend (do pretty n
                   write " = ")
               (pretty p)
      PFieldPun _ n -> pretty n
      PFieldWildcard _ -> write ".."

instance Pretty QualConDecl where
  prettyInternal x =
    case x of
      QualConDecl _ tyvars ctx d ->
        depend (unless (null (fromMaybe [] tyvars))
                       (do write "forall "
                           spaced (map pretty (fromMaybe [] tyvars))
                           write ". "))
               (depend (maybeCtx ctx)
                       (pretty d))

instance Pretty Rhs where
  prettyInternal x =
    case x of
      UnGuardedRhs _ e ->
        (swing (write " = ")
               (pretty e))
      GuardedRhss _ gas ->
        do newline
           indented 2
                    (lined (map (\p ->
                                   do write "|"
                                      pretty p)
                                gas))

instance Pretty Splice where
  prettyInternal x =
    case x of
      IdSplice _ str ->
        do write "$"
           string str
      ParenSplice _ e ->
        depend (write "$")
               (parens (pretty e))

instance Pretty InstHead where
  prettyInternal x =
    case x of
      IHead _ name tys ->
        spaced (pretty name :
                map pretty tys)
      IHInfix l a o b -> pretty (IHead l o [a,b])
      IHParen _ h -> parens (pretty h)

instance Pretty DeclHead where
  prettyInternal x =
    case x of
      DHead _ name tys ->
        spaced (pretty name :
                map pretty tys)
      DHInfix l a o b -> pretty (DHead l o [a,b])
      DHParen _ h -> parens (pretty h)

instance Pretty SpecialCon where
  prettyInternal s =
    case s of
      UnitCon _ -> write "()"
      ListCon _ -> write "[]"
      FunCon _ -> write "->"
      TupleCon _ Boxed i ->
        string ("(" ++
                replicate (i - 1) ',' ++
                ")")
      TupleCon _ Unboxed i ->
        string ("(#" ++
                replicate (i - 1) ',' ++
                "#)")
      Cons _ -> write ":"
      UnboxedSingleCon _ -> write "(##)"

--------------------------------------------------------------------------------
-- * Unimplemented or incomplete printers

instance Pretty Module where
  prettyInternal x =
    case x of
      Module _ mayModHead pragmas imps decls ->
        do case mayModHead of
             Nothing -> return ()
             Just modHead -> pretty' modHead
           inter newline $ map pretty pragmas
           inter newline $ map pretty imps
           inter newline $ map pretty decls
      XmlPage{} ->
        error "FIXME: No implementation for XmlPage."
      XmlHybrid{} ->
        error "FIXME: No implementation for XmlHybrid."

instance Pretty Bracket where
  prettyInternal x =
    case x of
      ExpBracket _ p ->
        brackets (depend (write "|")
                         (do pretty p
                             write "|"))
      PatBracket _ _ ->
        error "FIXME: No implementation for PatBracket."
      TypeBracket _ _ ->
        error "FIXME: No implementation for TypeBracket."
      DeclBracket _ _ ->
        error "FIXME: No implementation for DeclBracket."

instance Pretty IPBind where
  prettyInternal x =
    case x of
      IPBind _ _ _ ->
        error "FIXME: No implementation for IPBind."

--------------------------------------------------------------------------------
-- * Fallback printers

instance Pretty DataOrNew where
  prettyInternal = pretty'

instance Pretty FunDep where
  prettyInternal = pretty'

instance Pretty Kind where
  prettyInternal = pretty'

instance Pretty Literal where
  prettyInternal = pretty'

instance Pretty Name where
  prettyInternal = pretty'

instance Pretty QName where
  prettyInternal = pretty'

instance Pretty QOp where
  prettyInternal = pretty'

instance Pretty TyVarBind where
  prettyInternal = pretty'

instance Pretty ModuleHead where
  prettyInternal = pretty'

instance Pretty ModulePragma where
  prettyInternal = pretty'

instance Pretty ImportDecl where
  prettyInternal = pretty'

instance Pretty ModuleName where
  prettyInternal (ModuleName _ name) =
    write (T.fromString name)

instance Pretty ImportSpecList where
  prettyInternal = pretty'

instance Pretty ImportSpec where
  prettyInternal = pretty'
