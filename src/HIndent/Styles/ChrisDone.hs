{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Chris Done's style.
--
-- Documented here: <https://github.com/chrisdone/haskell-style-guide>

module HIndent.Styles.ChrisDone
  (chrisdone)
  where

import Control.Monad.State.Class
import Data.Int
import HIndent.Combinators
import HIndent.Instances ()
import HIndent.Types
import Language.Haskell.Exts.Syntax
import Prelude hiding (exp)

-- | A short function name.
shortName :: Int64
shortName = 10

-- | Column limit: 50
smallColumnLimit :: Int64
smallColumnLimit = 50

-- | Empty state.
data State = State

-- | The printer style.
chrisdone :: Style
chrisdone =
  Style {styleName = "chrisdone"
        ,styleAuthor = "Chris Done"
        ,styleDescription = "Chris Done's personal style. Documented here: <https://github.com/chrisdone/haskell-style-guide>"
        ,styleInitialState = State
        ,styleExtenders =
           [Extender exp
           ,Extender fieldupdate
           ,Extender rhs
           ,Extender guardedrhs
           ,Extender guardedalt
           ,Extender unguardedalt]
        ,styleDefConfig =
           Config {configMaxColumns = 80
                  ,configIndentSpaces = 2}}

-- | I want field updates to be dependent or newline.
fieldupdate :: t -> FieldUpdate -> Printer ()
fieldupdate _ e =
  case e of
    FieldUpdate n e' ->
      dependOrNewline
        (do pretty n
            write " = ")
        e'
        pretty
    _ -> prettyInternal e


rhs :: State -> Rhs -> Printer ()
rhs _ (UnGuardedRhs e) =
  do indentSpaces <- getIndentSpaces
     indented indentSpaces
              (dependOrNewline (write " = ")
                               e
                               pretty)
rhs _ e = prettyInternal e

-- | I want guarded RHS be dependent or newline.
guardedrhs :: State -> GuardedRhs -> Printer ()
guardedrhs _ (GuardedRhs _ stmts e) =
  indented 1
           (do prefixedLined
                 ','
                 (map (\p ->
                         do space
                            pretty p)
                      stmts)
               dependOrNewline
                 (write " = ")
                 e
                 (indented 1 .
                  pretty))

-- | I want guarded alts be dependent or newline.
guardedalt :: State -> GuardedAlt -> Printer ()
guardedalt _ (GuardedAlt _ stmts e) =
  indented 1
           (do (prefixedLined
                  ','
                  (map (\p ->
                          do space
                             pretty p)
                       stmts))
               dependOrNewline
                 (write " -> ")
                 e
                 (indented 1 .
                  pretty))

-- | I want unguarded alts be dependent or newline.
unguardedalt :: State -> GuardedAlts -> Printer ()
unguardedalt _ (UnGuardedAlt e) =
  dependOrNewline
    (write " -> ")
    e
    (indented 2 .
     pretty)
unguardedalt _ e = prettyInternal e

-- | Expressions
exp :: State -> Exp -> Printer ()
-- Infix applications will render on one line if possible, otherwise
-- if any of the arguments are not "flat" then that expression is
-- line-separated.
exp _ e@(InfixApp a op b) =
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
-- | We try to render everything on a flat line. More than one of the
-- arguments are not flat and it wouldn't be a single liner.
-- If the head is short we depend, otherwise we swing.
exp _ (App op a) =
  do orig <- gets psIndentLevel
     headIsShort <- isShort f
     depend (do pretty f
                space)
            (do flats <- mapM isFlat args
                flatish <- fmap ((< 2) . length . filter not)
                                (return flats)
                singleLiner <- isSingleLiner (spaced (map pretty args))
                if singleLiner &&
                   ((headIsShort && flatish) ||
                    all id flats)
                   then spaced (map pretty args)
                   else do allSingleLiners <- fmap (all id)
                                                   (mapM (isSingleLiner . pretty) args)
                           if headIsShort || allSingleLiners
                              then lined (map pretty args)
                              else do newline
                                      indentSpaces <- getIndentSpaces
                                      column (orig + indentSpaces)
                                             (lined (map pretty args)))
  where (f,args) = flatten op [a]
        flatten :: Exp -> [Exp] -> (Exp,[Exp])
        flatten (App f' a') b =
          flatten f' (a' : b)
        flatten f' as = (f',as)
-- | Lambdas are dependent if they can be.
exp _ (Lambda _ ps b) =
  depend (write "\\")
         (do spaced (map pretty ps)
             dependOrNewline
               (write " -> ")
               b
               (indented 1 .
                pretty))
exp _ e = prettyInternal e

-- | Is the expression "short"? Used for app heads.
isShort :: (Pretty a,Show a) => a -> Printer Bool
isShort p =
  do line <- gets psLine
     orig <- fmap psColumn (sandbox (write ""))
     st <- sandbox (pretty p)
     return (psLine st ==
             line &&
             (psColumn st <
              orig +
              shortName))

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

-- | Make the right hand side dependent if it's flat, otherwise
-- newline it.
dependOrNewline :: Printer () -> Exp -> (Exp -> Printer ()) -> Printer ()
dependOrNewline left right f =
  do flat <- isFlat right
     small <- isSmall (depend left (f right))
     if flat || small
        then depend left (f right)
        else do left
                newline
                (f right)

-- | Is an expression flat?
isFlat :: Exp -> Printer Bool
isFlat (Lambda _ _ e) = isFlat e
isFlat (App a b) = return (isName a && isName b)
  where isName (Var{}) = True
        isName _ = False
isFlat (InfixApp a _ b) =
  do a' <- isFlat a
     b' <- isFlat b
     return (a' && b')
isFlat (NegApp a) = isFlat a
isFlat VarQuote{} = return True
isFlat TypQuote{} = return True
isFlat (List []) = return True
isFlat Var{} = return True
isFlat Lit{} = return True
isFlat Con{} = return True
isFlat (LeftSection e _) = isFlat e
isFlat (RightSection _ e) = isFlat e
isFlat _ = return False
