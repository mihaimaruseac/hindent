{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
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
  , withCtx
  , printComment
  , printComments
  , withCaseContext
  , rhsSeparator
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
  , indentedBlock
  , column
  , getColumn
  , getLineNum
  , depend
  , dependBind
  , swing
  , swingBy
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

import           Control.Monad.State.Strict hiding (state)
import qualified Data.ByteString.Builder as S
import           Data.Foldable (traverse_)
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Monoid hiding (Alt)
import           Data.Typeable
import           HIndent.Types
import qualified Language.Haskell.Exts as P
import           Language.Haskell.Exts.Comments
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax
import           Prelude hiding (exp)

--------------------------------------------------------------------------------
-- * Pretty printing class

-- | Pretty printing class.
class (Annotated ast,Typeable ast) => Pretty ast where
  prettyInternal :: ast NodeInfo -> Printer ()

-- | Pretty print using extenders.
pretty :: (Pretty ast)
       => ast NodeInfo -> Printer ()
pretty a = do
    printComments Before a
    depend (prettyNoExt a) (printComments After a)

-- | Run the basic printer for the given node without calling an
-- extension hook for this node, but do allow extender hooks in child
-- nodes. Also auto-inserts comments.
prettyNoExt :: (Pretty ast)
            => ast NodeInfo -> Printer ()
prettyNoExt = prettyInternal

-- | Print comments of a node.
printComments :: (Pretty ast)
              => ComInfoLocation -> ast NodeInfo -> Printer ()
printComments loc' ast = do
  let correctLocation comment = comInfoLocation comment == Just loc'
      commentsWithLocation = filter correctLocation (nodeInfoComments info)
  comments <- return $ map comInfoComment commentsWithLocation

  forM_ comments $ \comment -> do
    -- Preceeding comments must have a newline before them.
    hasNewline <- gets psNewline
    when (not hasNewline && loc' == Before) newline

    printComment (Just $ srcInfoSpan $ nodeInfoSpan info) comment
  where info = ann ast

-- | Pretty print a comment.
printComment :: MonadState (PrintState) m => Maybe SrcSpan -> Comment -> m ()
printComment mayNodespan (Comment inline cspan str) =
  do -- Insert proper amount of space before comment.
     -- This maintains alignment. This cannot force comments
     -- to go before the left-most possible indent (specified by depends).
     case mayNodespan of
       Just nodespan ->
         do let neededSpaces = srcSpanStartColumn cspan -
                               max 1 (srcSpanEndColumn nodespan)
            replicateM_ neededSpaces space
       Nothing -> return ()

     if inline
        then do write "{-"
                string str
                write "-}"
                when (1 == srcSpanStartColumn cspan) $
                  modify (\s -> s {psEolComment = True})
        else do write "--"
                string str
                modify (\s ->
                          s {psEolComment = True})

-- | Pretty print using HSE's own printer. The 'P.Pretty' class here
-- is HSE's.
pretty' :: (Pretty ast,P.Pretty (ast SrcSpanInfo),MonadState (PrintState) m)
        => ast NodeInfo -> m ()
pretty' = write . P.prettyPrint . fmap nodeInfoSpan

--------------------------------------------------------------------------------
-- * Combinators

-- | Increase indentation level by n spaces for the given printer.
indented :: MonadState (PrintState) m => Int64 -> m a -> m a
indented i p =
  do level <- gets psIndentLevel
     modify (\s -> s {psIndentLevel = level + i})
     m <- p
     modify (\s -> s {psIndentLevel = level})
     return m

indentedBlock :: MonadState (PrintState) m => m a -> m a
indentedBlock p =
  do indentSpaces <- getIndentSpaces
     indented indentSpaces p

-- | Print all the printers separated by spaces.
spaced :: MonadState (PrintState) m => [m ()] -> m ()
spaced = inter space

-- | Print all the printers separated by commas.
commas :: MonadState (PrintState) m => [m ()] -> m ()
commas = inter comma

-- | Print all the printers separated by sep.
inter :: MonadState (PrintState) m => m () -> [m ()] -> m ()
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
lined :: MonadState (PrintState) m => [m ()] -> m ()
lined ps = sequence_ (intersperse newline ps)

-- | Print all the printers separated newlines and optionally a line
-- prefix.
prefixedLined :: MonadState (PrintState) m => String -> [m ()] -> m ()
prefixedLined pref ps' =
  case ps' of
    [] -> return ()
    (p:ps) ->
      do p
         indented (fromIntegral
                     (length pref *
                      (-1)))
                  (mapM_ (\p' ->
                            do newline
                               depend (write pref) p')
                         ps)

-- | Set the (newline-) indent level to the given column for the given
-- printer.
column :: MonadState (PrintState) m => Int64 -> m a -> m a
column i p =
  do level <- gets psIndentLevel
     modify (\s -> s {psIndentLevel = i})
     m <- p
     modify (\s -> s {psIndentLevel = level})
     return m

-- | Get the current indent level.
getColumn :: MonadState (PrintState) m => m Int64
getColumn = gets psColumn

-- | Get the current line number.
getLineNum :: MonadState (PrintState) m => m Int64
getLineNum = gets psLine

-- | Output a newline.
newline :: MonadState (PrintState) m => m ()
newline =
  do write "\n"
     modify (\s -> s {psNewline = True})

-- | Set the context to a case context, where RHS is printed with -> .
withCaseContext :: MonadState (PrintState) m
                => Bool -> m a -> m a
withCaseContext bool pr =
  do original <- gets psInsideCase
     modify (\s -> s {psInsideCase = bool})
     result <- pr
     modify (\s -> s {psInsideCase = original})
     return result

-- | Get the current RHS separator, either = or -> .
rhsSeparator :: MonadState (PrintState) m
             => m ()
rhsSeparator =
  do inCase <- gets psInsideCase
     if inCase
        then write "->"
        else write "="

-- | Make the latter's indentation depend upon the end column of the
-- former.
depend :: MonadState (PrintState) m => m () -> m b -> m b
depend maker dependent =
  do state' <- get
     maker
     st <- get
     col <- gets psColumn
     if psLine state' /= psLine st || psColumn state' /= psColumn st
        then column col dependent
        else dependent

-- | Make the latter's indentation depend upon the end column of the
-- former.
dependBind :: MonadState (PrintState) m => m a -> (a -> m b) -> m b
dependBind maker dependent =
  do state' <- get
     v <- maker
     st <- get
     col <- gets psColumn
     if psLine state' /= psLine st || psColumn state' /= psColumn st
        then column col (dependent v)
        else (dependent v)

-- | Wrap in parens.
parens :: MonadState (PrintState) m => m a -> m a
parens p =
  depend (write "(")
         (do v <- p
             write ")"
             return v)

-- | Wrap in braces.
braces :: MonadState (PrintState) m => m a -> m a
braces p =
  depend (write "{")
         (do v <- p
             write "}"
             return v)

-- | Wrap in brackets.
brackets :: MonadState (PrintState) m => m a -> m a
brackets p =
  depend (write "[")
         (do v <- p
             write "]"
             return v)

-- | Write a space.
space :: MonadState (PrintState) m => m ()
space = write " "

-- | Write a comma.
comma :: MonadState (PrintState) m => m ()
comma = write ","

-- | Write an integral.
int :: (MonadState (PrintState) m)
    => Integer -> m ()
int = write . show

-- | Write out a string, updating the current position information.
write :: MonadState (PrintState) m => String -> m ()
write x =
  do eol <- gets psEolComment
     when (eol && x /= "\n") newline
     state <- get
     let clearEmpty =
           configClearEmptyLines (psConfig state)
         writingNewline = x == "\n"
         out :: String
         out =
           if psNewline state &&
              not (clearEmpty && writingNewline)
              then (replicate (fromIntegral (psIndentLevel state))
                               ' ') <>
                   x
              else x
     modify (\s ->
               s {psOutput = psOutput state <> S.stringUtf8 out
                 ,psNewline = False
                 ,psEolComment = False
                 ,psLine = psLine state + fromIntegral additionalLines
                 ,psColumn =
                    if additionalLines > 0
                       then fromIntegral (length (concat (take 1 (reverse srclines))))
                       else psColumn state + fromIntegral (length out)})
  where srclines = lines x
        additionalLines =
          length (filter (== '\n') x)

-- | Write a string.
string :: MonadState (PrintState) m => String -> m ()
string = write

-- | Indent spaces, e.g. 2.
getIndentSpaces :: MonadState (PrintState) m => m Int64
getIndentSpaces =
  gets (configIndentSpaces . psConfig)

-- | Column limit, e.g. 80
getColumnLimit :: MonadState (PrintState) m => m Int64
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

-- | Render a type with a context, or not.
withCtx :: (Pretty ast)
        => Maybe (ast NodeInfo) -> Printer b -> Printer b
withCtx Nothing m = m
withCtx (Just ctx) m =
  do pretty ctx
     write " =>"
     newline
     m

-- | Maybe render an overlap definition.
maybeOverlap ::  Maybe (Overlap NodeInfo) -> Printer ()
maybeOverlap =
  maybe (return ())
        (\p ->
           pretty p >>
           space)

-- | Swing the second printer below and indented with respect to the first.
swing :: Printer () -> Printer b -> Printer ()
swing a b =
  do orig <- gets psIndentLevel
     a
     (fits,st) <- fitsOnOneLine (do space
                                    b)
     if fits
        then put st
        else do newline
                indentSpaces <- getIndentSpaces
                column (orig + indentSpaces) b
                return ()

-- | Swing the second printer below and indented with respect to the first by
-- the specified amount.
swingBy :: Int64 -> Printer() -> Printer b -> Printer b
swingBy i a b =
  do orig <- gets psIndentLevel
     a
     newline
     column (orig + i) b

--------------------------------------------------------------------------------
-- * Instances

instance Pretty Context where
  prettyInternal  =
    context

instance Pretty Pat where
  prettyInternal x =
    case x of
      PLit _ sign l -> pretty sign >> pretty l
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
        do indentSpaces <- getIndentSpaces
           depend (do pretty qname
                      space)
                  (braces (prefixedLined ","
                                         (map (indented indentSpaces . pretty) fields)))

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
prettyInfixOp ::  QName NodeInfo -> Printer ()
prettyInfixOp x =
  case x of
    Qual _ mn n ->
      case n of
        Ident _ i -> do write "`"; pretty mn; write "."; string i; write "`";
        Symbol _ s -> do pretty mn; write "."; string s;
    UnQual _ n ->
      case n of
        Ident _ i -> string ("`" ++ i ++ "`")
        Symbol _ s -> string s
    Special _ s -> pretty s

instance Pretty Type where
  prettyInternal  =
    typ

instance Pretty Exp where
  prettyInternal = exp'

-- | Render an expression.
exp :: Exp NodeInfo -> Printer ()
exp (OverloadedLabel _ _) = error "FIXME: No implementation for OverloadedLabel"
exp (TypeApp _ _) = error "FIXME: No implementation for TypeApp"
exp (ExprHole {}) = write "_"
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
         (do spaced (map (\(i,x) -> do case (i,x) of
                                         (0,PIrrPat{}) -> space
                                         (0,PBangPat{}) -> space
                                         _ -> return ()
                                       pretty x) (zip [0..] ps))
             swing (write " ->")
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
     indentedBlock (lined (map (withCaseContext True . pretty) alts))
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
exp (RecUpdate _ n fs) =
  depend (do pretty n
             space)
         (braces (prefixedLined ","
                                (map (indentedBlock . pretty) fs)))
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
  brackets (do pretty e
               unless (null qstmt)
                      (do newline
                          indented (-1)
                                   (write "|")
                          prefixedLined ","
                                        (map pretty qstmt)))
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
     newline
     indentedBlock (lined (map (withCaseContext True . pretty) alts))
exp (MultiIf _ alts) =
  withCaseContext
    True
    (depend (write "if ")
            (lined (map (\p ->
                           do write "| "
                              pretty p)
                        alts)))
exp (Lit _ lit) = prettyInternal lit
exp x@XTag{} = pretty' x
exp x@XETag{} = pretty' x
exp x@XPcdata{} = pretty' x
exp x@XExpTag{} = pretty' x
exp x@XChildTag{} = pretty' x
exp x@Var{} = pretty' x
exp x@IPVar{} = pretty' x
exp x@Con{} = pretty' x
exp x@CorePragma{} = pretty' x
exp x@SCCPragma{} = pretty' x
exp x@GenPragma{} = pretty' x
exp x@Proc{} = pretty' x
exp x@LeftArrApp{} = pretty' x
exp x@RightArrApp{} = pretty' x
exp x@LeftArrHighApp{} = pretty' x
exp x@RightArrHighApp{} = pretty' x
exp x@ParArray{} = pretty' x
exp x@ParArrayFromTo{} = pretty' x
exp x@ParArrayFromThenTo{} = pretty' x
exp x@ParArrayComp{} = pretty' x
exp ParComp{} =
  error "FIXME: No implementation for ParComp."
exp (OverloadedLabel _ label) = string ('#' : label)

instance Pretty Stmt where
  prettyInternal =
    stmt

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
  prettyInternal = decl'

-- | Render a declaration.
decl ::  Decl NodeInfo -> Printer ()
decl (PatBind _ pat rhs' mbinds) =
  do pretty pat
     withCaseContext False (pretty rhs')
     case mbinds of
       Nothing -> return ()
       Just binds ->
         do newline
            indentedBlock (depend (write "where ")
                                  (pretty binds))
decl (InstDecl _ moverlap dhead decls) =
  do depend (write "instance ")
            (depend (maybeOverlap moverlap)
                    (depend (pretty dhead)
                            (unless (null (fromMaybe [] decls))
                                    (write " where"))))
     unless (null (fromMaybe [] decls))
            (do newline
                indentedBlock (lined (map pretty (fromMaybe [] decls))))
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
            (withCtx ctx
                     (depend (do pretty dhead
                                 space)
                             (depend (unless (null fundeps)
                                             (do write " | "
                                                 commas (map pretty fundeps)))
                                     (unless (null (fromMaybe [] decls))
                                             (write " where")))))
     unless (null (fromMaybe [] decls))
            (do newline
                indentedBlock (lined (map pretty (fromMaybe [] decls))))
decl (TypeDecl _ typehead typ') =
  depend (write "type ")
         (depend (pretty typehead)
                 (depend (write " = ")
                         (pretty typ')))

decl TypeFamDecl{} =
  error "FIXME: No implementation for TypeFamDecl."
decl (DataDecl _ dataornew ctx dhead condecls mderivs) =
  do depend (do pretty dataornew
                space)
            (withCtx ctx
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

decl (InlineSig _ inline _ name) = do
  write "{-# "

  unless inline $ write "NO"
  write "INLINE "
  pretty name

  write " #-}"
decl x = pretty' x

instance Pretty Deriving where
  prettyInternal (Deriving _ heads) =
    do write "deriving"
       space
       let heads' =
             if length heads == 1
                then map stripParens heads
                else heads
       parens (commas (map pretty heads'))
    where stripParens (IParen _ iRule) = stripParens iRule
          stripParens x = x

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
                  indentedBlock (depend (write "where ")
                                (pretty binds))

instance Pretty Asst where
  prettyInternal x =
    case x of
      ClassA _ name types -> spaced (pretty name : map pretty types)
      i@InfixA{} -> pretty' i
      IParam{} -> error "FIXME: No implementation for IParam."
      EqualP _ a b ->
        do pretty a
           write " ~ "
           pretty b
      ParenA _ asst -> parens (pretty asst)
      AppA _ name tys -> spaced (pretty name : map pretty tys)
      WildCardA _ name ->
        case name of
          Nothing -> write "_"
          Just n ->
            do write "_"
               pretty n

instance Pretty BangType where
  prettyInternal x =
    case x of
      BangedTy _ -> write "!"
      LazyTy _ -> write "~"
      NoStrictAnnot _ -> return ()

instance Pretty Unpackedness where
  prettyInternal (Unpack _) = write "{-# UNPACK -#}"
  prettyInternal (NoUnpack _) = write "{-# NOUNPACK -#}"
  prettyInternal (NoUnpackPragma _) = return ()

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
               (withCtx ctx
                        (do pretty h
                            (case mkind of
                               Nothing -> return ()
                               Just kind ->
                                 do write " :: "
                                    pretty kind)))
      ClsTyFam _ h bindings mkind ->
        depend (write "type ")
               (depend (pretty h)
                       (depend (mapM_ (\kind -> write " :: " >> pretty kind) mkind)
                               (mapM_ pretty bindings)))
      ClsTyDef _ (TypeEqn _ this that) ->
        do write "type "
           pretty this
           write " = "
           pretty that
      ClsDefSig _ name ty ->
        do write "default "
           pretty name
           write " :: "
           pretty ty

instance Pretty ConDecl where
  prettyInternal x =
    conDecl x

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
                  write " =")
               (pretty e)
      FieldPun _ n -> pretty n
      FieldWildcard _ -> write ".."

instance Pretty GuardedRhs where
  prettyInternal  =
    guardedRhs
    {-case x of
      GuardedRhs _ stmts e ->
        do indented 1
                    (do prefixedLined
                          ","
                          (map (\p ->
                                  do space
                                     pretty p)
                               stmts))
           swing (write " " >> rhsSeparator >> write " ")
                 (pretty e)-}

instance Pretty InjectivityInfo where
  prettyInternal x = pretty' x

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
  prettyInternal = match
    {-case x of
      Match _ name pats rhs' mbinds ->
        do depend (do pretty name
                      space)
                  (spaced (map pretty pats))
           withCaseContext False (pretty rhs')
           case mbinds of
             Nothing -> return ()
             Just binds ->
               do newline
                  indentedBlock (depend (write "where ")
                                        (pretty binds))
      InfixMatch _ pat1 name pats rhs' mbinds ->
        do depend (do pretty pat1
                      space
                      case name of
                        Ident _ i ->
                          string ("`" ++ i ++ "`")
                        Symbol _ s -> string s)
                  (do space
                      spaced (map pretty pats))
           withCaseContext False (pretty rhs')
           case mbinds of
             Nothing -> return ()
             Just binds ->
               do newline
                  indentedBlock (depend (write "where ")
                                        (pretty binds))-}

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
               (withCtx ctx
                       (pretty d))

instance Pretty Rhs where
  prettyInternal =
    rhs

instance Pretty Splice where
  prettyInternal x =
    case x of
      IdSplice _ str ->
        do write "$"
           string str
      ParenSplice _ e ->
        depend (write "$")
               (parens (pretty e))

instance Pretty InstRule where
  prettyInternal (IParen _ rule) = parens $ pretty rule
  prettyInternal (IRule _ mvarbinds mctx ihead) =
    do case mvarbinds of
         Nothing -> return ()
         Just xs -> spaced (map pretty xs)
       withCtx mctx (pretty ihead)

instance Pretty InstHead where
  prettyInternal x =
    case x of
      -- Base cases
      IHCon _ name -> pretty name
      IHInfix _ typ' name ->
        depend (pretty typ')
               (do space
                   prettyInfixOp name)
      -- Recursive application
      IHApp _ ihead typ' ->
        depend (pretty ihead)
               (do space
                   pretty typ')
      -- Wrapping in parens
      IHParen _ h -> parens (pretty h)

instance Pretty DeclHead where
  prettyInternal x =
    case x of
      DHead _ name -> pretty name
      DHParen _ h -> parens (pretty h)
      DHInfix _ var name ->
        do pretty var
           space
           write "`"
           pretty name
           write "`"
      DHApp _ dhead var ->
        depend (pretty dhead)
               (do space
                   pretty var)

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

instance Pretty Overlap where
  prettyInternal (Overlap _) = write "{-# OVERLAP #-}"
  prettyInternal (NoOverlap _) = write "{-# NO_OVERLAP #-}"
  prettyInternal (Incoherent _) = write "{-# INCOHERENT #-}"

instance Pretty Sign where
  prettyInternal (Signless _) = return ()
  prettyInternal (Negative _) = write "-"

--------------------------------------------------------------------------------
-- * Unimplemented or incomplete printers

instance Pretty Module where
  prettyInternal x =
    case x of
      Module _ mayModHead pragmas imps decls ->
        inter (do newline
                  newline)
              (mapMaybe (\(isNull,r) ->
                           if isNull
                              then Nothing
                              else Just r)
                        [(null pragmas,inter newline (map pretty pragmas))
                        ,(case mayModHead of
                            Nothing -> (True,return ())
                            Just modHead -> (False,pretty modHead))
                        ,(null imps,inter newline (map pretty imps))
                        ,(null decls
                         ,interOf newline
                                  (map (\case
                                          r@TypeSig{} -> (1,pretty r)
                                          r -> (2,pretty r))
                                       decls))])
        where interOf i ((c,p):ps) =
                case ps of
                  [] -> p
                  _ ->
                    do p
                       replicateM_ c i
                       interOf i ps
              interOf _ [] = return ()
      XmlPage{} -> error "FIXME: No implementation for XmlPage."
      XmlHybrid{} -> error "FIXME: No implementation for XmlHybrid."

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
      d@(DeclBracket _ _) -> pretty' d

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

instance Pretty ResultSig where
  prettyInternal (KindSig _ kind) = pretty kind
  prettyInternal (TyVarSig _ tyVarBind) = pretty tyVarBind

instance Pretty Literal where
  prettyInternal (String _ _ rep) = do
    write "\""
    string rep
    write "\""
  prettyInternal (Char _ _ rep) = do
    write "'"
    string rep
    write "'"
  prettyInternal (PrimString _ _ rep) = do
    write "\""
    string rep
    write "\"#"
  prettyInternal (PrimChar _ _ rep) = do
    write "'"
    string rep
    write "'#"
  -- We print the original notation (because HSE doesn't track Hex
  -- vs binary vs decimal notation).
  prettyInternal (Int _l _i originalString) =
    string originalString
  prettyInternal (Frac _l _r originalString) =
    string originalString
  prettyInternal x = pretty' x

instance Pretty Name where
  prettyInternal = pretty'

instance Pretty QName where
  prettyInternal = pretty'

instance Pretty QOp where
  prettyInternal = pretty'

instance Pretty TyVarBind where
  prettyInternal = pretty'

instance Pretty ModuleHead where
  prettyInternal (ModuleHead _ name mwarnings mexports) =
    do write "module "
       pretty name
       maybe (return ()) pretty mwarnings
       maybe (return ())
             (\exports ->
                do newline
                   indented 2 (pretty exports)
                   newline
                   space)
             mexports
       write " where"

instance Pretty ModulePragma where
  prettyInternal = pretty'

instance Pretty ImportDecl where
  prettyInternal = pretty'

instance Pretty ModuleName where
  prettyInternal (ModuleName _ name) =
    write name

instance Pretty ImportSpecList where
  prettyInternal = pretty'

instance Pretty ImportSpec where
  prettyInternal = pretty'

instance Pretty WarningText where
  prettyInternal (DeprText _ s) =
    write "{-# DEPRECATED " >> string s >> write " #-}"
  prettyInternal (WarnText _ s) =
    write "{-# WARNING " >> string s >> write " #-}"

instance Pretty ExportSpecList where
  prettyInternal (ExportSpecList _ es) =
    parens (prefixedLined ","
                          (map pretty es))

instance Pretty ExportSpec where
  prettyInternal = pretty'

-- Do statements need to handle infix expression indentation specially because
-- do x *
--    y
-- is two invalid statements, not one valid infix op.
stmt :: Stmt NodeInfo -> Printer ()
stmt (Qualifier _ e@(InfixApp _ a op b)) =
  do col <- fmap (psColumn . snd)
                 (sandbox (write ""))
     infixApp e a op b (Just col)
stmt (Generator _ p e) =
  do indentSpaces <- getIndentSpaces
     pretty p
     indented indentSpaces
              (dependOrNewline
                 (write " <- ")
                 e
                 pretty)
stmt x = case x of
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

-- | Make the right hand side dependent if it fits on one line,
-- otherwise send it to the next line.
dependOrNewline :: Printer ()
                -> Exp NodeInfo
                -> (Exp NodeInfo -> Printer ())
                -> Printer ()
dependOrNewline left right f =
  do (fits,st) <- fitsOnOneLine renderDependent
     if fits
        then put st
        else do left
                newline
                (f right)
  where renderDependent = depend left (f right)

-- | Handle do and case specially and also space out guards more.
rhs :: Rhs NodeInfo -> Printer ()
rhs (UnGuardedRhs _ (Do _ dos)) =
  do inCase <- gets psInsideCase
     write (if inCase then " -> " else " = ")
     indentSpaces <- getIndentSpaces
     let indentation | inCase = indentSpaces
                     | otherwise = max 2 indentSpaces
     swingBy indentation
             (write "do")
             (lined (map pretty dos))
rhs (UnGuardedRhs _ e) =
  do (fits,st) <-
       fitsOnOneLine
         (do write " "
             rhsSeparator
             write " "
             pretty e)
     if fits
        then put st
        else swing (write " " >> rhsSeparator)
                    (pretty e)
rhs (GuardedRhss _ gas) =
  do newline
     indented 2
              (lined (map (\p ->
                             do write "|"
                                pretty p)
                          gas))

-- | Implement dangling right-hand-sides.
guardedRhs :: GuardedRhs NodeInfo -> Printer ()
-- | Handle do specially.

guardedRhs (GuardedRhs _ stmts (Do _ dos)) =
  do indented 1
              (do prefixedLined
                    ","
                    (map (\p ->
                            do space
                               pretty p)
                         stmts))
     inCase <- gets psInsideCase
     write (if inCase then " -> " else " = ")
     swing (write "do")
            (lined (map pretty dos))
guardedRhs (GuardedRhs _ stmts e) =
  do (fits,st) <-
       fitsOnOneLine
         (indented 1
                   (do prefixedLined
                         ","
                         (map (\p ->
                                 do space
                                    pretty p)
                              stmts)))
     put st
     if fits
        then do (fits',st') <-
                  fitsOnOneLine
                    (do write " "
                        rhsSeparator
                        write " "
                        pretty e)
                if fits'
                   then put st'
                   else swingIt
        else swingIt
  where swingIt =
          swing (write " " >> rhsSeparator)
                 (pretty e)


-- | Expression customizations.
exp' :: Exp NodeInfo -> Printer ()
-- | Do after lambda should swing.
exp' (Lambda _ pats (Do l stmts)) =
  do
     (fits,st) <-
       fitsOnOneLine
         (do write "\\"
             spaced (map pretty pats)
             write " -> "
             pretty (Do l stmts))
     if fits
        then put st
        else swing (do write "\\"
                       spaced (map pretty pats)
                       write " -> do")
                    (lined (map pretty stmts))
-- | Space out tuples.
exp' (Tuple _ boxed exps) =
  depend (write (case boxed of
                   Unboxed -> "(#"
                   Boxed -> "("))
         (do single <- isSingleLiner p
             underflow <- fmap not (isOverflow p)
             if single && underflow
                then p
                else prefixedLined ","
                                   (map (depend space . pretty) exps)
             write (case boxed of
                      Unboxed -> "#)"
                      Boxed -> ")"))
  where p = inter (write ", ") (map pretty exps)
-- | Space out tuples.
exp' (TupleSection _ boxed mexps) =
  depend (write (case boxed of
                   Unboxed -> "(#"
                   Boxed -> "("))
         (do inter (write ", ") (map (maybe (return ()) pretty) mexps)
             write (case boxed of
                      Unboxed -> "#)"
                      Boxed -> ")"))
-- | Infix apps, same algorithm as ChrisDone at the moment.
exp' e@(InfixApp _ a op b) =
  infixApp e a op b Nothing
-- | If bodies are indented 4 spaces. Handle also do-notation.
exp' (If _ if' then' else') =
  do depend (write "if ")
            (pretty if')
     newline
     indentSpaces <- getIndentSpaces
     indented indentSpaces
              (do branch "then " then'
                  newline
                  branch "else " else')
     -- Special handling for do.
  where branch str e =
          case e of
            Do _ stmts ->
              do write str
                 write "do"
                 newline
                 indentSpaces <- getIndentSpaces
                 indented indentSpaces (lined (map pretty stmts))
            _ ->
              depend (write str)
                     (pretty e)
-- | App algorithm similar to ChrisDone algorithm, but with no
-- parent-child alignment.
exp' (App _ op a) =
  do (fits,st) <-
       fitsOnOneLine (spaced (map pretty (f : args)))
     if fits
        then put st
        else do pretty f
                newline
                spaces <- getIndentSpaces
                indented spaces (lined (map pretty args))
  where (f,args) = flatten op [a]
        flatten :: Exp NodeInfo
                -> [Exp NodeInfo]
                -> (Exp NodeInfo,[Exp NodeInfo])
        flatten (App _ f' a') b =
          flatten f' (a' : b)
        flatten f' as = (f',as)
-- | Space out commas in list.
exp' (List _ es) =
  do single <- isSingleLiner p
     underflow <- fmap not (isOverflow p)
     if single && underflow
        then p
        else brackets (prefixedLined ","
                                     (map (depend space . pretty) es))
  where p =
          brackets (inter (write ", ")
                          (map pretty es))
exp' (RecUpdate _ exp'' updates) = recUpdateExpr (pretty exp'') updates
exp' (RecConstr _ qname updates) = recUpdateExpr (pretty qname) updates
exp' (Let _ binds e) =
  depend (write "let ")
         (do pretty binds
             newline
             indented (-4) (depend (write "in ")
                                   (pretty e)))
exp' (ListComp _ e qstmt) =
  brackets (do space
               pretty e
               unless (null qstmt)
                      (do newline
                          indented (-1)
                                   (write "|")
                          prefixedLined ","
                                        (map (\x -> do space
                                                       pretty x
                                                       space)
                                             qstmt)))
exp' e = exp e

match :: Match NodeInfo -> Printer ()
match (Match _ name pats rhs' mbinds) =
  do depend (do pretty name
                space)
            (spaced (map pretty pats))
     withCaseContext False (pretty rhs')
     forM_ mbinds bindingGroup
match (InfixMatch _ pat1 name pats rhs' mbinds) =
  do depend (do pretty pat1
                space
                case name of
                  Ident _ i ->
                    string ("`" ++ i ++ "`")
                  Symbol _ s -> string s)
            (do space
                spaced (map pretty pats))
     withCaseContext False (pretty rhs')
     forM_ mbinds bindingGroup

-- | Format contexts with spaces and commas between class constraints.
context :: Context NodeInfo -> Printer ()
context ctx@(CxTuple _ asserts) =
  do (fits,st) <-
       fitsOnOneLine
         (parens (inter (comma >> space)
                        (map pretty asserts)))
     if fits
        then put st
        else prettyNoExt ctx
context ctx = case ctx of
                CxSingle _ a -> pretty a
                CxTuple _ as ->
                  parens (prefixedLined ","
                                        (map pretty as))
                CxEmpty _ -> parens (return ())

unboxParens :: MonadState (PrintState) m => m a -> m a
unboxParens p =
  depend (write "(# ")
         (do v <- p
             write " #)"
             return v)

typ :: Type NodeInfo -> Printer ()
typ (TyTuple _ Boxed types) = parens $ inter (write ", ") $ map pretty types
typ (TyTuple _ Unboxed types) = unboxParens $ inter (write ", ") $ map pretty types
typ x = case x of
          TyForall _ mbinds ctx ty ->
            depend (case mbinds of
                      Nothing -> return ()
                      Just ts ->
                        do write "forall "
                           spaced (map pretty ts)
                           write ". ")
                   (withCtx ctx (pretty ty))
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
          TyParArray _ t ->
            brackets (do write ":"
                         pretty t
                         write ":")
          TyApp _ f a -> spaced [pretty f,pretty a]
          TyVar _ n -> pretty n
          TyCon _ p -> pretty p
          TyParen _ e -> parens (pretty e)
          TyInfix _ a op b ->
            depend (do pretty a
                       space)
                   (depend (do prettyInfixOp op
                               space)
                           (pretty b))
          TyKind _ ty k ->
            parens (do pretty ty
                       write " :: "
                       pretty k)
          TyBang _ bangty unpackty right ->
            do pretty unpackty
               pretty bangty
               pretty right
          TyEquals _ left right ->
            do pretty left
               write " ~ "
               pretty right
          ty@TyPromoted{} -> pretty' ty
          TySplice{} -> error "FIXME: No implementation for TySplice."
          TyWildCard _ name ->
            case name of
              Nothing -> write "_"
              Just n ->
                do write "_"
                   pretty n
          _ -> error ("FIXME: No implementation for " ++ show x)

-- | Specially format records. Indent where clauses only 2 spaces.
decl' :: Decl NodeInfo -> Printer ()
-- | Pretty print type signatures like
--
-- foo :: (Show x, Read x)
--     => (Foo -> Bar)
--     -> Maybe Int
--     -> (Char -> X -> Y)
--     -> IO ()
--
decl' (TypeSig _ names ty') =
  do small <- isSmall (declTy ty')
     if small
        then depend (do inter (write ", ")
                              (map pretty names)
                        write " :: ")
                    (declTy ty')
        else do inter (write ", ")
                      (map pretty names)
                newline
                indentSpaces <- getIndentSpaces
                indented indentSpaces
                         (depend (write ":: ")
                                 (declTy ty'))
  where declTy dty =
          case dty of
            TyForall _ mbinds mctx ty ->
              do case mbinds of
                   Nothing -> return ()
                   Just ts ->
                     do write "forall "
                        spaced (map pretty ts)
                        write ". "
                        newline
                 case mctx of
                   Nothing -> prettyTy ty
                   Just ctx ->
                     do pretty ctx
                        newline
                        indented (-3)
                                 (depend (write "=> ")
                                         (prettyTy ty))
            _ -> prettyTy dty
        collapseFaps (TyFun _ arg result) = arg : collapseFaps result
        collapseFaps e = [e]
        prettyTy ty =
          do small <- isSmall (pretty ty)
             if small
                then pretty ty
                else case collapseFaps ty of
                       [] -> pretty ty
                       tys ->
                         prefixedLined "-> "
                                       (map pretty tys)
decl' (PatBind _ pat rhs' mbinds) =
  withCaseContext False $
    do pretty pat
       pretty rhs'
       forM_ mbinds bindingGroup

-- | Handle records specially for a prettier display (see guide).
decl' (DataDecl _ dataornew ctx dhead condecls@[_] mderivs)
  | any isRecord condecls =
    do depend (do pretty dataornew
                  unless (null condecls) space)
              (withCtx ctx
                       (do pretty dhead
                           multiCons condecls))
       case mderivs of
         Nothing -> return ()
         Just derivs -> pretty derivs
  where multiCons xs =
          depend (write " =")
                 (inter (write "|")
                        (map (depend space . qualConDecl) xs))
decl' e = decl e

-- | Use special record display, used by 'dataDecl' in a record scenario.
qualConDecl :: QualConDecl NodeInfo -> Printer ()
qualConDecl x =
  case x of
    QualConDecl _ tyvars ctx d ->
      depend (unless (null (fromMaybe [] tyvars))
                     (do write "forall "
                         spaced (map pretty (fromMaybe [] tyvars))
                         write ". "))
             (withCtx ctx (recDecl d))

-- | Fields are preceded with a space.
conDecl :: ConDecl NodeInfo -> Printer ()
conDecl (RecDecl _ name fields) =
  depend (do pretty name
             write " ")
         (do depend (write "{")
                    (prefixedLined ","
                                   (map (depend space . pretty) fields))
             write "}")
conDecl x = case x of
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

-- | Record decls are formatted like: Foo
-- { bar :: X
-- }
recDecl :: ConDecl NodeInfo -> Printer ()
recDecl (RecDecl _ name fields) =
  do pretty name
     indentSpaces <- getIndentSpaces
     newline
     column indentSpaces
            (do depend (write "{")
                       (prefixedLined ","
                                      (map (depend space . pretty) fields))
                newline
                write "} ")
recDecl r = prettyNoExt r

recUpdateExpr :: Printer () -> [FieldUpdate NodeInfo] -> Printer ()
recUpdateExpr expWriter updates = do
    expWriter
    newline
    indentSpaces <- getIndentSpaces
    mapM
      (\(i,x) -> do
         if i == 0
           then write "{ "
           else write ", "
         pretty x
         newline)
      (zip [0 ..] updates)
    write "}"

--------------------------------------------------------------------------------
-- Predicates

-- | Is the decl a record?
isRecord :: QualConDecl t -> Bool
isRecord (QualConDecl _ _ _ RecDecl{}) = True
isRecord _ = False

-- | Does printing the given thing overflow column limit? (e.g. 80)
isOverflow :: MonadState (PrintState) m => m a -> m Bool
isOverflow p =
  do (_,st) <- sandbox p
     columnLimit <- getColumnLimit
     return (psColumn st > columnLimit)

-- | Does printing the given thing overflow column limit? (e.g. 80)
fitsOnOneLine :: MonadState (PrintState) m => m a -> m (Bool,PrintState)
fitsOnOneLine p =
  do line <- gets psLine
     (_,st) <- sandbox p
     columnLimit <- getColumnLimit
     return (psLine st == line && psColumn st < columnLimit,st)

-- | Is the given expression a single-liner when printed?
isSingleLiner :: MonadState (PrintState) m
              => m a -> m Bool
isSingleLiner p =
  do line <- gets psLine
     (_,st) <- sandbox p
     return (psLine st == line)

isSmall :: MonadState PrintState m => m a -> m Bool
isSmall p =
  do overflows <- isOverflow p
     oneLine <- isSingleLiner p
     return (not overflows && oneLine)

bindingGroup :: Binds NodeInfo -> Printer ()
bindingGroup binds =
  do newline
     indented 2
              (do write "where"
                  newline
                  indented 2 (pretty binds))

infixApp :: Exp NodeInfo
         -> Exp NodeInfo
         -> QOp NodeInfo
         -> Exp NodeInfo
         -> Maybe Int64
         -> Printer ()
infixApp e a op b indent =
  do (fits,st) <-
       fitsOnOneLine
         (spaced (map (\link ->
                         case link of
                           OpChainExp e' -> pretty e'
                           OpChainLink qop -> pretty qop)
                      (flattenOpChain e)))
     if fits
        then put st
        else do prettyWithIndent a
                space
                pretty op
                newline
                case indent of
                  Nothing -> prettyWithIndent b
                  Just col ->
                    do indentSpaces <- getIndentSpaces
                       column (col + indentSpaces)
                              (prettyWithIndent b)
  where prettyWithIndent e' =
          case e' of
            (InfixApp _ a' op' b') ->
              infixApp e' a' op' b' indent
            _ -> pretty e'

-- | A link in a chain of operator applications.
data OpChainLink l
  = OpChainExp (Exp l)
  | OpChainLink (QOp l)
  deriving (Show)

-- | Flatten a tree of InfixApp expressions into a chain of operator
-- links.
flattenOpChain :: Exp l -> [OpChainLink l]
flattenOpChain (InfixApp _ left op right) =
  flattenOpChain left <>
  [OpChainLink op] <>
  flattenOpChain right
flattenOpChain e = [OpChainExp e]
