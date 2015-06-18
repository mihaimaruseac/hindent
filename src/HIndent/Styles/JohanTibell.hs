{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stub module for Johan Tibell's style.
--
-- Documented here: <https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md>
--
-- Questions:
--
-- How to indent after a guarded alt/rhs?
-- How to indent let?
-- How to indent large ADT constructors types?

module HIndent.Styles.JohanTibell where

import Control.Monad hiding (forM_)
import Control.Monad.State.Class
import Data.Foldable (forM_)
import Data.Int
import Data.Maybe
import HIndent.Pretty
import HIndent.Styles.ChrisDone (infixApp)
import HIndent.Types
import Language.Haskell.Exts.Annotated.Syntax
import Prelude hiding (exp)

--------------------------------------------------------------------------------
-- Style configuration

-- | A short function name.
shortName :: Int64
shortName = 10

-- | Empty state.
data State =
  State

-- | The printer style.
johanTibell :: Style
johanTibell =
  Style {styleName = "johan-tibell"
        ,styleAuthor = "Chris Done"
        ,styleDescription = "Style modeled from Johan's style guide here: <https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md>"
        ,styleInitialState = State
        ,styleExtenders =
           [Extender decl
           ,Extender match
           ,Extender context
           ,Extender typ
           ,Extender conDecl
           ,Extender exp
           ,Extender guardedRhs
           ,Extender rhs
           ,Extender stmt
           ,Extender fieldupdate
           ]
        ,styleDefConfig =
           defaultConfig {configMaxColumns = 80
                         ,configIndentSpaces = 4}
        ,styleCommentPreprocessor = return}

--------------------------------------------------------------------------------
-- Extenders

-- Do statements need to handle infix expression indentation specially because
-- do x *
--    y
-- is two invalid statements, not one valid infix op.
stmt :: Stmt NodeInfo -> Printer s ()
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
stmt e = prettyNoExt e

-- | Make the right hand side dependent if it fits on one line,
-- otherwise send it to the next line.
dependOrNewline :: Printer t ()
                -> Exp NodeInfo
                -> (Exp NodeInfo -> Printer t ())
                -> Printer t ()
dependOrNewline left right f =
  do (fits,st) <- fitsOnOneLine renderDependent
     if fits
        then put st
        else do left
                newline
                (f right)
  where renderDependent = depend left (f right)

-- | Handle do and case specially and also space out guards more.
rhs :: Rhs NodeInfo -> Printer s ()
rhs (UnGuardedRhs _ (Do _ dos)) =
  do inCase <- gets psInsideCase
     write (if inCase then " -> " else " = ")
     indentSpaces <- getIndentSpaces
     let indentation | inCase = indentSpaces
                     | otherwise = max 4 indentSpaces
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
        else swing (write " " >> rhsSeparator >> write " ")
                   (pretty e)
rhs (GuardedRhss _ gas) =
  do newline
     indented 2
              (lined (map (\p ->
                             do write "|"
                                pretty p)
                          gas))

-- | Implement dangling right-hand-sides.
guardedRhs :: GuardedRhs NodeInfo -> Printer s ()
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
          swing (write " " >> rhsSeparator >> write " ")
                (pretty e)


-- | Expression customizations.
exp :: Exp NodeInfo -> Printer s ()
-- | Space out tuples.
exp (Tuple _ boxed exps) =
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
exp (TupleSection _ boxed mexps) =
  depend (write (case boxed of
                   Unboxed -> "(#"
                   Boxed -> "("))
         (do inter (write ", ") (map (maybe (return ()) pretty) mexps)
             write (case boxed of
                      Unboxed -> "#)"
                      Boxed -> ")"))
-- | Infix apps, same algorithm as ChrisDone at the moment.
exp e@(InfixApp _ a op b) =
  infixApp e a op b Nothing
-- | If bodies are indented 4 spaces. Handle also do-notation.
exp (If _ if' then' else') =
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
exp (App _ op a) =
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
exp (List _ es) =
  do single <- isSingleLiner p
     underflow <- fmap not (isOverflow p)
     if single && underflow
        then p
        else brackets (prefixedLined ","
                                     (map (depend space . pretty) es))
  where p =
          brackets (inter (write ", ")
                          (map pretty es))
exp (RecUpdate _ exp' updates) = recUpdateExpr (pretty exp') updates
exp (RecConstr _ qname updates) = recUpdateExpr (pretty qname) updates
exp (Let _ binds e) =
  depend (write "let ")
         (do pretty binds
             newline
             indented (-4) (depend (write "in ")
                                   (pretty e)))
exp e = prettyNoExt e

match :: Match NodeInfo -> Printer s ()
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
context :: Context NodeInfo -> Printer s ()
context (CxTuple _ asserts) =
  parens $ inter (comma >> space) $ map pretty asserts
context ctx = prettyNoExt ctx

unboxParens :: MonadState (PrintState s) m => m a -> m a
unboxParens p =
  depend (write "(# ")
         (do v <- p
             write " #)"
             return v)

typ :: Type NodeInfo -> Printer s ()
typ (TyTuple _ Boxed types) = parens $ inter (write ", ") $ map pretty types
typ (TyTuple _ Unboxed types) = unboxParens $ inter (write ", ") $ map pretty types
typ ty = prettyNoExt ty

-- | Specially format records. Indent where clauses only 2 spaces.
decl :: Decl NodeInfo -> Printer s ()
-- | Pretty print type signatures like
--
-- foo :: (Show x, Read x)
--     => (Foo -> Bar)
--     -> Maybe Int
--     -> (Char -> X -> Y)
--     -> IO ()
--
decl (TypeSig _ names ty') =
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
decl (PatBind _ pat rhs' mbinds) =
  withCaseContext False $
    do pretty pat
       pretty rhs'
       forM_ mbinds bindingGroup

-- | Handle records specially for a prettier display (see guide).
decl (DataDecl _ dataornew ctx dhead condecls@[_] mderivs)
  | any isRecord condecls =
    do depend (do pretty dataornew
                  unless (null condecls) space)
              (depend (maybeCtx ctx)
                      (do pretty dhead
                          multiCons condecls))
       case mderivs of
         Nothing -> return ()
         Just derivs -> pretty derivs
  where multiCons xs =
          depend (write " =")
                 (inter (write "|")
                        (map (depend space . qualConDecl) xs))
decl e = prettyNoExt e

-- | Use special record display, used by 'dataDecl' in a record scenario.
qualConDecl :: QualConDecl NodeInfo -> Printer s ()
qualConDecl x =
    case x of
        QualConDecl _ tyvars ctx d ->
            depend
                (unless
                     (null (fromMaybe [] tyvars))
                     (do write "forall "
                         spaced (map pretty (fromMaybe [] tyvars))
                         write ". "))
                (depend
                     (maybeCtx ctx)
                     (recDecl d))

-- | Fields are preceded with a space.
conDecl :: ConDecl NodeInfo -> Printer s ()
conDecl (RecDecl _ name fields) =
  depend (do pretty name
             write " ")
         (do depend (write "{")
                    (prefixedLined ","
                                   (map (depend space . pretty) fields))
             write "}")
conDecl e = prettyNoExt e

-- | Record decls are formatted like: Foo
-- { bar :: X
-- }
recDecl :: ConDecl NodeInfo -> Printer s ()
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

recUpdateExpr :: Printer s () -> [FieldUpdate NodeInfo] -> Printer s ()
recUpdateExpr expWriter updates = do
  expWriter
  newline
  indentSpaces <- getIndentSpaces
  write "{ "
  -- -2 because the "{ " moved us 2 chars to the right.
  indented (indentSpaces -2) $ do
    prefixedLined ", " $ map pretty updates
    newline
  write "}"

--------------------------------------------------------------------------------
-- Predicates

-- | Is the decl a record?
isRecord :: QualConDecl t -> Bool
isRecord (QualConDecl _ _ _ RecDecl{}) = True
isRecord _ = False

-- | Does printing the given thing overflow column limit? (e.g. 80)
isOverflow :: MonadState (PrintState s) m => m a -> m Bool
isOverflow p =
  do (_,st) <- sandbox p
     columnLimit <- getColumnLimit
     return (psColumn st > columnLimit)

-- | Does printing the given thing overflow column limit? (e.g. 80)
fitsOnOneLine :: MonadState (PrintState s) m => m a -> m (Bool,PrintState s)
fitsOnOneLine p =
  do line <- gets psLine
     (_,st) <- sandbox p
     columnLimit <- getColumnLimit
     return (psLine st == line && psColumn st < columnLimit,st)

-- | Is the given expression a single-liner when printed?
isSingleLiner :: MonadState (PrintState s) m
              => m a -> m Bool
isSingleLiner p =
  do line <- gets psLine
     (_,st) <- sandbox p
     return (psLine st == line)

-- | Is the expression "short"? Used for app heads.
isShort :: (Pretty ast)
        => ast NodeInfo -> Printer s (Bool)
isShort p =
  do line <- gets psLine
     orig <- fmap (psColumn . snd) (sandbox (write ""))
     (_,st) <- sandbox (pretty p)
     return (psLine st == line &&
             (psColumn st < orig + shortName))

-- | Is an expression flat?
isFlat :: Exp NodeInfo -> Printer s Bool
isFlat (Lambda _ _ e) = isFlat e
isFlat (App _ a b) =
  return (isName a && isName b)
  where isName (Var{}) = True
        isName _ = False
isFlat (NegApp _ a) = isFlat a
isFlat VarQuote{} = return True
isFlat TypQuote{} = return True
isFlat (List _ []) = return True
isFlat Var{} = return True
isFlat Lit{} = return True
isFlat Con{} = return True
isFlat (LeftSection _ e _) = isFlat e
isFlat (RightSection _ _ e) = isFlat e
isFlat _ = return False

-- | rhs on field update on the same line as lhs.
fieldupdate :: FieldUpdate NodeInfo -> Printer s ()
fieldupdate e =
  case e of
    FieldUpdate _ n e' -> do pretty n
                             write " = "
                             pretty e'
    _ -> prettyNoExt e

isSmall :: MonadState (PrintState t) m => m a -> m Bool
isSmall p =
  do overflows <- isOverflow p
     oneLine <- isSingleLiner p
     return (not overflows && oneLine)

bindingGroup :: Binds NodeInfo -> Printer s ()
bindingGroup binds =
  do newline
     indented 2
              (do write "where"
                  newline
                  indented 2 (pretty binds))
