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

module HIndent.Styles.JohanTibell
  (johanTibell)
  where

import Control.Monad
import Control.Monad.State.Class
import Data.Int
import Data.Maybe
import HIndent.Pretty
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
           ,Extender conDecl
           ,Extender exp
           ,Extender guardedRhs
           ,Extender rhs
           ,Extender ifAlt
           ,Extender alts
           ,Extender guardedAlt]
        ,styleDefConfig =
           Config {configMaxColumns = 80
                  ,configIndentSpaces = 4}}

--------------------------------------------------------------------------------
-- Extenders

-- | Handle do specially and also space out guards more.
rhs :: t -> Rhs NodeInfo -> Printer ()
rhs _ x =
  case x of
    UnGuardedRhs _ (Do _ dos) ->
      swing (write " = do")
            (lined (map pretty dos))
    GuardedRhss _ gas ->
      do newline
         indentSpaces <- getIndentSpaces
         indented indentSpaces
                  (lined (map (\p ->
                                 do write "|"
                                    pretty p)
                              gas))
    _ -> prettyNoExt x

-- | Case alts.
alts :: t -> GuardedAlts NodeInfo -> Printer ()
-- | Handle do specially.
alts _ x =
  case x of
    UnGuardedAlt _ (Do _ dos) ->
      swing (write " -> do")
            (lined (map pretty dos))
    GuardedAlts _ gas ->
      do newline
         indentSpaces <- getIndentSpaces
         indented indentSpaces
                  (lined (map (\p ->
                                 do write "|"
                                    pretty p)
                              gas))
    _ -> prettyNoExt x

-- | Handle do specially.
guardedAlt _ x =
  case x of
    GuardedAlt _ stmts (Do _ dos) ->
      do indented 1
                  (do (prefixedLined
                         ","
                         (map (\p ->
                                 do space
                                    pretty p)
                              stmts)))
         swing (write " -> do ")
               (lined (map pretty dos))
    _ -> prettyNoExt x

-- | Handle do specially.
ifAlt _ (IfAlt _ cond (Do _ dos)) =
  do pretty cond
     swing (write " -> do")
           (lined (map pretty dos))
ifAlt _ e = prettyNoExt e

-- | Implement dangling right-hand-sides.
guardedRhs :: t -> GuardedRhs NodeInfo -> Printer ()
-- | Handle do specially.
guardedRhs _ (GuardedRhs _ stmts (Do _ dos)) =
  do indented 1
              (do prefixedLined
                    ","
                    (map (\p ->
                            do space
                               pretty p)
                         stmts))
     swing (write " = do")
           (lined (map pretty dos))
guardedRhs _ e = prettyNoExt e

-- | Expression customizations.
exp :: t -> Exp NodeInfo -> Printer ()
-- | Space out tuples.
exp _ (Tuple _ boxed exps) =
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
exp _ (TupleSection _ boxed mexps) =
  depend (write (case boxed of
                   Unboxed -> "(#"
                   Boxed -> "("))
         (do inter (write ", ") (map (maybe (return ()) pretty) mexps)
             write (case boxed of
                      Unboxed -> "#)"
                      Boxed -> ")"))
-- | Infix apps, same algorithm as ChrisDone at the moment.
exp _ e@(InfixApp _ a op b) =
  infixApp e a op b Nothing
-- | If bodies are indented 4 spaces. Handle also do-notation.
exp _ (If _ if' then' else') =
  do depend (write "if ")
            (pretty if')
     newline
     indentSpaces <- getIndentSpaces
     indented indentSpaces
              (do branch "then " then'
                  newline
                  branch "else " else')
     -- Special handling for do.
  where branch string e =
          case e of
            Do _ stmts ->
              do write string
                 write "do"
                 newline
                 indentSpaces <- getIndentSpaces
                 indented indentSpaces (lined (map pretty stmts))
            _ ->
              depend (write string)
                     (pretty e)
-- | App algorithm similar to ChrisDone algorithm, but with no
-- parent-child alignment.
exp _ (App _ op a) =
  do orig <- gets psIndentLevel
     headIsShort <- isShort f
     depend (do pretty f
                space)
            (do flats <- mapM isFlat args
                flatish <- fmap ((< 2) . length . filter not)
                                (return flats)
                singleLiner <- isSingleLiner (spaced (map pretty args))
                overflow <- isOverflow (spaced (map pretty args))
                if singleLiner &&
                   ((headIsShort && flatish) ||
                    all id flats) &&
                   not overflow
                   then spaced (map pretty args)
                   else do newline
                           indentSpaces <- getIndentSpaces
                           column (orig + indentSpaces)
                                  (lined (map pretty args)))
  where (f,args) = flatten op [a]
        flatten :: Exp NodeInfo
                -> [Exp NodeInfo]
                -> (Exp NodeInfo,[Exp NodeInfo])
        flatten (App _ f' a') b =
          flatten f' (a' : b)
        flatten f' as = (((f',as)))
-- | Space out commas in list.
exp _ (List _ es) =
  do single <- isSingleLiner p
     underflow <- fmap not (isOverflow p)
     if single && underflow
        then p
        else brackets (prefixedLined ","
                                     (map (depend space . pretty) es))
  where p =
          brackets (inter (write ", ")
                          (map pretty es))
exp _ e = prettyNoExt e

-- | Specially format records. Indent where clauses only 2 spaces.
decl :: t -> Decl NodeInfo -> Printer ()
decl _ (PatBind _ pat mty rhs mbinds) =
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
                indented 2
                         (do write "where "
                             newline
                             indented 2 (pretty binds))
-- | Handle records specially for a prettier display (see guide).
decl _ (DataDecl _ dataornew ctx dhead condecls@[_] mderivs)
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
decl _ e = prettyNoExt e

-- | Use special record display, used by 'dataDecl' in a record scenario.
qualConDecl :: QualConDecl NodeInfo -> Printer ()
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
conDecl :: t -> ConDecl NodeInfo -> Printer ()
conDecl _ (RecDecl _ name fields) =
  depend (do pretty name
             write " ")
         (do depend (write "{")
                    (prefixedLined ","
                                   (map (depend space . pretty) fields))
             write "}")
conDecl _ e = prettyNoExt e

--------------------------------------------------------------------------------
-- Predicates

-- | Is the decl a record?
isRecord :: QualConDecl t -> Bool
isRecord (QualConDecl _ _ _ RecDecl{}) = True
isRecord _ = False

-- | Does printing the given thing overflow column limit? (e.g. 80)
isOverflow :: Printer a -> Printer Bool
isOverflow p =
  do (_,st) <- sandbox p
     columnLimit <- getColumnLimit
     return (psColumn st > columnLimit)

-- | Is the given expression a single-liner when printed?
isSingleLiner :: MonadState PrintState m
              => m a -> m Bool
isSingleLiner p =
  do line <- gets psLine
     (_,st) <- sandbox p
     return (psLine st == line)

-- | Is the expression "short"? Used for app heads.
isShort :: (Pretty ast)
        => ast NodeInfo -> Printer (Bool)
isShort p =
  do line <- gets psLine
     orig <- fmap (psColumn . snd) (sandbox (write ""))
     (_,st) <- sandbox (pretty p)
     return (psLine st == line &&
             (psColumn st < orig + shortName))

-- | Is an expression flat?
isFlat :: Exp NodeInfo -> Printer Bool
isFlat (Lambda _ _ e) = isFlat e
isFlat (App _ a b) =
  return (isName a && isName b)
  where isName (Var{}) = True
        isName _ = False
isFlat (InfixApp _ a _ b) =
  do a' <- isFlat a
     b' <- isFlat b
     return (a' && b')
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

--------------------------------------------------------------------------------
-- Helpers

infixApp :: (Pretty ast,Pretty ast1,Pretty ast2)
         => Exp NodeInfo
         -> ast NodeInfo
         -> ast1 NodeInfo
         -> ast2 NodeInfo
         -> Maybe Int64
         -> Printer ()
infixApp e a op b indent =
  do is <- isFlat e
     overflow <- isOverflow
                   (depend (do pretty a
                               space
                               pretty op
                               space)
                           (do pretty b))
     if is && not overflow
        then do depend (do pretty a
                           space
                           pretty op
                           space)
                       (do pretty b)
        else do pretty a
                space
                pretty op
                newline
                case indent of
                  Nothing -> pretty b
                  Just col ->
                    do indentSpaces <- getIndentSpaces
                       column (col + indentSpaces)
                              (pretty b)

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
                write "} ")
recDecl r = prettyNoExt r
