{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Enno Cramer's Style.

module HIndent.Styles.Cramer (cramer) where

import Control.Monad (forM_, unless, when)
import Control.Monad.State.Strict (MonadState, get, gets, put)

import Data.List (intersperse, sortOn)

import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts (prettyPrint)

import HIndent.Pretty hiding (inter, spaced)
import HIndent.Types

-- | Line breaking mode for syntactical constructs.
data LineBreak
  = Free    -- ^ Break whenever
  | Single  -- ^ Force single line (if possible)
  | Multi   -- ^ Force multiple lines
  deriving (Eq,Enum,Show)

-- | Printer state.
data State =
  State {cramerLineBreak :: LineBreak     -- ^ Current line breaking mode
        }
  deriving (Show)

-- | Syntax shortcut for Extenders.
type Extend f = f NodeInfo -> Printer State ()

-- | Style definition.
cramer :: Style
cramer =
  Style {styleName = "cramer"
        ,styleAuthor = "Enno Cramer"
        ,styleDescription = "Enno Cramer's style"
        ,styleInitialState =
           State {cramerLineBreak = Free}
        ,styleExtenders =
           [Extender extModuleHead
           ,Extender extExportSpecList
           ,Extender extImportDecl
           ,Extender extDecl
           ,Extender extConDecl
           ,Extender extDeriving
           ,Extender extRhs
           ,Extender extContext
           ,Extender extType
           ,Extender extPat
           ,Extender extExp
           ,Extender extMatch]
        ,styleDefConfig =
           defaultConfig {configMaxColumns = 80
                         ,configIndentSpaces = 4
                         ,configClearEmptyLines = True}
        ,styleCommentPreprocessor = return}

--------------------------------------------------------------------------------
-- Helper

-- | Return whether a data type has only empty constructors.
isEnum :: Decl NodeInfo -> Bool
isEnum (DataDecl _ (DataType _) Nothing (DHead _ _) constructors _) =
  all isSimple constructors
  where isSimple (QualConDecl _ Nothing Nothing (ConDecl _ _ [])) = True
        isSimple _ = False
isEnum _ = False

-- | Specialized forM_ for Maybe.
maybeM_ :: Monad m
        => Maybe a -> (a -> m ()) -> m ()
maybeM_ = forM_

-- | Simplified HIndent.Pretty.inter that does not modify the indent level.
inter :: MonadState (PrintState s) m
      => m () -> [m ()] -> m ()
inter sep = sequence_ . intersperse sep

-- | Simplified HIndent.Pretty.spaced that does not modify the indent level.
spaced :: MonadState (PrintState s) m
       => [m ()] -> m ()
spaced = inter space

-- | Indent one level.
indentFull :: MonadState (PrintState s) m
           => m a -> m a
indentFull p = getIndentSpaces >>= flip indented p

-- | Indent a half level.
indentHalf :: MonadState (PrintState s) m
           => m a -> m a
indentHalf p = getIndentSpaces >>= flip indented p . (`div` 2)

-- | Set indentation level to current column.
align :: MonadState (PrintState s) m
      => m a -> m a
align p =
  do col <- getColumn
     indent <- gets psIndentLevel
     column (max col indent) p

-- | Update the line breaking mode and restore afterwards.
withLineBreak
  :: LineBreak -> Printer State a -> Printer State a
withLineBreak lb p =
  do old <- gets (cramerLineBreak . psUserState)
     modifyState $ \s -> s {cramerLineBreak = lb}
     result <- p
     modifyState $ \s -> s {cramerLineBreak = old}
     return result

-- | Use the first printer if it fits on a single line within the
-- column limit, otherwise use the second.
attemptSingleLine
  :: Printer State a -> Printer State a -> Printer State a
attemptSingleLine single multi =
  do prevState <- get
     case cramerLineBreak . psUserState $ prevState of
       Single -> single
       Multi -> multi
       Free ->
         do result <- withLineBreak Single single
            col <- getColumn
            row <- getLineNum
            if row == psLine prevState &&
               col <= configMaxColumns (psConfig prevState)
               then return result
               else do put prevState
                       multi

-- | Same as attemptSingleLine, but execute the second printer in Multi
-- mode.  Used in type signatures to force either a single line or
-- have each `->` on a line by itself.
attemptSingleLineType
  :: Printer State a -> Printer State a -> Printer State a
attemptSingleLineType single multi =
  attemptSingleLine single
                    (withLineBreak Multi multi)

-- | Format a list-like structure on a single line.
listSingleLine :: Pretty a
               => String
               -> String
               -> String
               -> [a NodeInfo]
               -> Printer State ()
listSingleLine open close _ [] =
  do string open
     space
     string close
listSingleLine open close sep xs =
  do string open
     space
     inter (string sep >> space) $ map pretty xs
     space
     string close

-- | Format a list-like structure with each element on a line by
-- itself.
listMultiLine
  :: Pretty a
  => String -> String -> String -> [a NodeInfo] -> Printer State ()
listMultiLine open close _ [] =
  align $
  do string open
     newline
     string close
listMultiLine open close sep xs =
  align $
  do string open
     space
     inter (newline >> string sep >> space) $ map pretty xs
     newline
     string close

-- | Format a list-like structure on a single line, if possible, or
-- each element on a line by itself.
listAttemptSingleLine :: Pretty a
                      => String
                      -> String
                      -> String
                      -> [a NodeInfo]
                      -> Printer State ()
listAttemptSingleLine open close sep xs =
  attemptSingleLine (listSingleLine open close sep xs)
                    (listMultiLine open close sep xs)

-- | Format a list-like structure, automatically breaking lines when
-- the next separator and item do not fit within the column limit.
listAutoWrap
  :: Pretty a
  => String -> String -> String -> [a NodeInfo] -> Printer State ()
listAutoWrap open close sep ps =
  align $
  do string open
     unless (null ps) $
       do space
          pretty $ head ps
          forM_ (map (\p -> string sep >> space >> pretty p)
                     (tail ps)) $
            \p ->
              do fits <- fitsColumnLimit p
                 unless fits newline
                 p
          space
     string close
  where fitsColumnLimit p =
          fmap fst . sandbox $
          do _ <- p
             col <- getColumn
             limit <- gets (configMaxColumns . psConfig)
             return $ col < limit

--------------------------------------------------------------------------------
-- Printer for reused syntactical constructs

whereBinds :: Binds NodeInfo -> Printer State ()
whereBinds binds =
  do newline
     indentHalf $
       do write "where"
          newline
          indentHalf $ pretty binds

rhsExpr :: Exp NodeInfo -> Printer State ()
-- No line break before do
rhsExpr expr@Do{} =
  do space
     rhsSeparator
     space
     pretty expr
rhsExpr expr =
  do space
     rhsSeparator
     attemptSingleLine single multi
  where single = space >> pretty expr
        multi = newline >> indentFull (pretty expr)

guardedRhsExpr
  :: GuardedRhs NodeInfo -> Printer State ()
guardedRhsExpr (GuardedRhs _ guards expr) =
  depend (write "| ") $
  do inter (write ", ") $ map pretty guards
     rhsExpr expr

-- | Pretty print a name for being an infix operator.
prettyInfixOp :: MonadState (PrintState s) m
              => QName NodeInfo -> m ()
prettyInfixOp op =
  case op of
    Qual{} ->
      do write "`"
         pretty' op
         write "`"
    UnQual _ n ->
      case n of
        Ident _ i -> string ("`" ++ i ++ "`")
        Symbol _ s -> string s
    Special _ s -> pretty s

--------------------------------------------------------------------------------
-- Extenders

-- Empty or single item export list on one line, otherwise one item
-- per line with parens and comma aligned
extModuleHead :: Extend ModuleHead
extModuleHead (ModuleHead _ name mwarn mexports) =
  do mapM_ pretty mwarn
     write "module "
     pretty name
     maybeM_ mexports $ \exports -> pretty exports
     write " where"

-- Align export list, one item per line
extExportSpecList :: Extend ExportSpecList
extExportSpecList (ExportSpecList _ exports) =
  case exports of
    [] -> write " ( )"
    [e] -> write " ( " >> pretty e >> write " )"
    _ ->
      do newline
         indentFull $ listMultiLine "(" ")" "," exports

-- Align import statements
extImportDecl :: Extend ImportDecl
extImportDecl ImportDecl{..} =
  do if importQualified
        then write "import qualified "
        else write "import           "
     pretty importModule
     maybeM_ importAs $
       \name ->
         do write " as "
            pretty name
     maybeM_ importSpecs $
       \(ImportSpecList _ importHiding specs) ->
         do space
            when importHiding $ write "hiding "
            listAutoWrap "(" ")" "," $ sortOn prettyPrint specs

extDecl :: Extend Decl
-- Align data constructors
extDecl decl@(DataDecl _ dataOrNew mcontext declHead constructors mderiv) =
  do mapM_ pretty mcontext
     pretty dataOrNew
     space
     pretty declHead
     if isEnum decl
        then attemptSingleLine single multi
        else multi
     maybeM_ mderiv $ \deriv -> indentFull $ newline >> pretty deriv
  where single =
          do write " = "
             inter (write " | ") $ map pretty constructors
        multi =
          align $
          do write " = "
             inter (newline >> write " | ") $ map pretty constructors
-- Type signature either on a single line or split at arrows, aligned with '::'
extDecl (TypeSig _ names ty) =
  do inter (write ", ") $ map pretty names
     space
     attemptSingleLineType (write ":: " >> pretty ty)
                           (align $ write ":: " >> pretty ty)
-- Half-indent for where clause, half-indent binds
extDecl (PatBind _ pat rhs mbinds) =
  do pretty pat
     pretty rhs
     maybeM_ mbinds whereBinds
extDecl other = prettyNoExt other

extConDecl :: Extend ConDecl
-- No extra space after empty constructor
extConDecl (ConDecl _ name []) = pretty name
extConDecl other = prettyNoExt other

-- Derived instances separated by comma and space, no line breaking
extDeriving :: Extend Deriving
extDeriving (Deriving _ instHeads) =
  do write "deriving "
     case instHeads of
       [x] -> pretty x
       xs -> parens $ inter (write ", ") $ map pretty xs

extRhs :: Extend Rhs
extRhs (UnGuardedRhs _ expr) = rhsExpr expr
extRhs (GuardedRhss _ [rhs]) = space >> guardedRhsExpr rhs
extRhs (GuardedRhss _ rhss) =
  forM_ rhss $
  \rhs ->
    do newline
       indentFull $ guardedRhsExpr rhs

-- Type constraints on a single line
extContext :: Extend Context
extContext (CxTuple _ ctxs) = parens $ inter (write ", ") $ map pretty ctxs
extContext other = prettyNoExt other

extType :: Extend Type
extType (TyForall _ mforall mcontext ty) =
  do maybeM_ mforall $
       \vars ->
         do write "forall "
            spaced $ map pretty vars
            write ". "
     maybeM_ mcontext $
       \contexts ->
         attemptSingleLineType
           (pretty contexts >> write " => " >> pretty ty)
           (pretty contexts >> newline >> write "=> " >> pretty ty)
-- Type signature should line break at each arrow if necessary
extType (TyFun _ from to) =
  attemptSingleLineType (pretty from >> write " -> " >> pretty to)
                        (pretty from >> newline >> write "-> " >> pretty to)
-- Parentheses reset forced line breaking
extType (TyParen _ ty) = withLineBreak Free $ parens $ pretty ty
extType other = prettyNoExt other

extPat :: Extend Pat
-- Infix application with space around operator
extPat (PInfixApp _ arg1 op arg2) =
  do pretty arg1
     space
     prettyInfixOp op
     space
     pretty arg2
extPat other = prettyNoExt other

extExp :: Extend Exp
-- Function application on a single line or align arguments
extExp expr@(App _ fun arg) = attemptSingleLine single multi
  where single = pretty fun >> space >> pretty arg
        multi =
          pretty fun' >> space >> align (lined $ map pretty $ reverse args')
        (fun',args') = collectArgs expr
        collectArgs
          :: Exp NodeInfo -> (Exp NodeInfo,[Exp NodeInfo])
        collectArgs (App _ g y) =
          let (f,args) = collectArgs g
          in (f,y : args)
        collectArgs nonApp = (nonApp,[])
-- Infix application on a single line or indented rhs
extExp (InfixApp _ arg1 op arg2) =
  do pretty arg1
     space
     pretty op
     -- No line break before do
     case arg2 of
       Do{} -> single
       _ -> attemptSingleLine single multi
  where single = space >> pretty arg2
        multi = newline >> indentFull (pretty arg2)
-- No line break before do
extExp (Lambda _ pats expr) =
  do write "\\"
     spaced $ map pretty pats
     write " ->"
     -- No line break before do
     case expr of
       Do{} -> single
       _ -> attemptSingleLine single multi
  where single = space >> pretty expr
        multi = newline >> indentFull (pretty expr)
-- Line break and indent after do
extExp (Do _ stmts) =
  do write "do"
     newline
     indentFull . lined $ map pretty stmts
extExp other = prettyNoExt other

extMatch :: Extend Match
-- Indent where same as for top level decl
extMatch (Match _ name pats rhs mbinds) =
  do pretty name
     space
     spaced $ map pretty pats
     pretty rhs
     maybeM_ mbinds whereBinds
extMatch other = prettyNoExt other
