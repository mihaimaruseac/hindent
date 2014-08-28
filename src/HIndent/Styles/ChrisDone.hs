{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Chris Done's style.
--
-- Documented here: <https://github.com/chrisdone/haskell-style-guide>

module HIndent.Styles.ChrisDone
  (chrisdone)
  where

import Control.Monad.State.Class
import HIndent.Combinators
import HIndent.Instances ()
import HIndent.Types
import Language.Haskell.Exts.Syntax
import Prelude hiding (exp)

-- | Empty state.
data State = State

-- | The printer style.
chrisdone :: Style
chrisdone =
  Style {styleName = "chrisdone"
        ,styleAuthor = "Chris Done"
        ,styleDescription = "Chris Done's personal style. Documented here: <https://github.com/chrisdone/haskell-style-guide>"
        ,styleInitialState = State
        ,styleExtenders = [Extender exp]}

exp :: State -> Exp -> Printer ()
exp _ e =
  case e of
    (InfixApp a op b) ->
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
    (App op a) ->
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
                                          column (orig + indentSpaces)
                                                 (lined (map pretty args)))
      where (f,args) = flatten op [a]
            flatten :: Exp -> [Exp] -> (Exp,[Exp])
            flatten (App f' a') b =
              flatten f' (a' : b)
            flatten f' as = (f',as)
    (Lambda _ ps b) ->
      depend (write "\\")
             (do spaced (map pretty ps)
                 dependOrNewline
                   (write " -> ")
                   b
                   (indented 1 .
                    pretty))
    _ -> prettyInternal e

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
