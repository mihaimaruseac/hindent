{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Enno Cramer's Style.

module HIndent.Styles.Cramer (cramer) where

import Control.Monad (forM_, replicateM_, unless, when)
import Control.Monad.State.Strict (MonadState, get, gets, put)

import Data.List (intersperse, sortOn)
import Data.Maybe (catMaybes, isJust, mapMaybe)

import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.SrcLoc
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
        ,cramerLangPragmaLength :: Int    -- ^ Padding length for pragmas
        ,cramerModuleImportLength :: Int  -- ^ Padding length for module imports
        ,cramerRecordFieldLength :: Int   -- ^ Padding length for record fields
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
           State {cramerLineBreak = Free
                 ,cramerLangPragmaLength = 0
                 ,cramerModuleImportLength = 0
                 ,cramerRecordFieldLength = 0}
        ,styleExtenders =
           [Extender extModule
           ,Extender extModulePragma
           ,Extender extModuleHead
           ,Extender extExportSpecList
           ,Extender extImportDecl
           ,Extender extDecl
           ,Extender extConDecl
           ,Extender extFieldDecl
           ,Extender extDeriving
           ,Extender extRhs
           ,Extender extContext
           ,Extender extType
           ,Extender extPat
           ,Extender extExp
           ,Extender extStmt
           ,Extender extMatch
           ,Extender extBinds
           ,Extender extFieldUpdate]
        ,styleDefConfig =
           defaultConfig {configMaxColumns = 80
                         ,configIndentSpaces = 4
                         ,configClearEmptyLines = True}
        ,styleCommentPreprocessor = return}

--------------------------------------------------------------------------------
-- Helper

-- | Turn a Name into a String
nameStr :: Name a -> String
nameStr (Ident _ s) = s
nameStr (Symbol _ s) = "(" ++ s ++ ")"

-- | Extract the name as a String from a ModuleName
moduleName :: ModuleName a -> String
moduleName (ModuleName _ s) = s

-- | Extract the names of a ModulePragma
pragmaNames :: ModulePragma a -> [String]
pragmaNames (LanguagePragma _ names) = map nameStr names
pragmaNames _ = []

-- | Return whether a data type has only empty constructors.
isEnum :: Decl NodeInfo -> Bool
isEnum (DataDecl _ (DataType _) Nothing (DHead _ _) constructors _) =
  all isSimple constructors
  where isSimple (QualConDecl _ Nothing Nothing (ConDecl _ _ [])) = True
        isSimple _ = False
isEnum _ = False

-- | If the given String is smaller than the given length, pad on
-- right with spaces until the length matches.
padRight :: Int -> String -> String
padRight l s = take (max l (length s)) (s ++ repeat ' ')

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

-- | Like `inter newline . map pretty`, but preserve empty lines
-- between elements.
preserveLineSpacing
  :: (Pretty ast,Annotated ast)
  => [ast NodeInfo] -> Printer State ()
preserveLineSpacing [] = return ()
preserveLineSpacing asts@(first:rest) =
  do pretty first
     forM_ (zip asts rest) $
       \(prev,cur) ->
         do replicateM_ (max 1 $ delta cur prev)
                        newline
            pretty cur
  where delta cur prev =
          let prevLine =
                srcSpanEndLine . srcInfoSpan . nodeInfoSpan . ann $ prev
              curLine =
                srcSpanStartLine . srcInfoSpan . nodeInfoSpan . ann $ cur
          in curLine - prevLine

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

tupleExpr
  :: Pretty ast
  => Boxed -> [ast NodeInfo] -> Printer State ()
tupleExpr boxed exprs = attemptSingleLine single multi
  where single =
          do string open
             inter (write ", ") $ map pretty exprs
             string close
        multi = listMultiLine open close "," exprs
        (open,close) =
          case boxed of
            Unboxed -> ("(#","#)")
            Boxed -> ("(",")")

listExpr :: Pretty ast
         => [ast NodeInfo] -> Printer State ()
listExpr [] = write "[]"
listExpr xs = listAttemptSingleLine "[" "]" "," xs

recordExpr
  :: (Pretty ast,Pretty ast')
  => ast NodeInfo -> [ast' NodeInfo] -> Printer State ()
recordExpr expr updates =
  do pretty expr
     space
     listAttemptSingleLine "{" "}" "," updates

ifExpr :: (Printer State () -> Printer State ())
       -> Exp NodeInfo
       -> Exp NodeInfo
       -> Exp NodeInfo
       -> Printer State ()
ifExpr indent cond true false = attemptSingleLine single multi
  where single = spaced [if',then',else']
        multi =
          align $
          do if'
             indent $
               do newline
                  then'
                  newline
                  else'
        if' = write "if " >> pretty cond
        then' = write "then " >> pretty true
        else' = write "else " >> pretty false

letExpr
  :: Binds NodeInfo -> Printer State () -> Printer State ()
letExpr binds expr =
  align $
  do depend (write "let ") $ pretty binds
     newline
     write "in"
     expr

typeSig :: Type NodeInfo -> Printer State ()
typeSig ty =
  attemptSingleLineType (write ":: " >> pretty ty)
                        (align $ write ":: " >> pretty ty)

--------------------------------------------------------------------------------
-- Extenders

extModule :: Extend Module
extModule (Module _ mhead pragmas imports decls) =
  do modifyState $ \s -> s {cramerLangPragmaLength = pragLen
                           ,cramerModuleImportLength = modLen}
     inter (newline >> newline) $
       catMaybes [unless' (null pragmas) $ preserveLineSpacing pragmas
                 ,pretty <$> mhead
                 ,unless' (null imports) $ preserveLineSpacing imports
                 ,unless' (null decls) $
                  do forM_ (init decls) $
                       \decl ->
                         do pretty decl
                            newline
                            unless (skipNewline decl) newline
                     pretty (last decls)]
  where pragLen = maximum $ map length $ concatMap pragmaNames pragmas
        modLen = maximum $ map (length . moduleName . importModule) imports
        unless' cond expr =
          if not cond
             then Just expr
             else Nothing
        skipNewline TypeSig{} = True
        skipNewline _ = False
extModule other = prettyNoExt other

-- Align closing braces of pragmas
extModulePragma :: Extend ModulePragma
extModulePragma (LanguagePragma _ names) =
  do namelen <- gets (cramerLangPragmaLength . psUserState)
     forM_ names $
       \name ->
         do write "{-# LANGUAGE "
            string $ padRight namelen $ nameStr name
            write " #-}"
extModulePragma other = prettyNoExt other

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
    [e]
      | not (hasComments e) -> write " ( " >> pretty e >> write " )"
    (first:rest) ->
      do newline
         indentFull $
           do write "( "
              commentCol <- getColumn
              align $ prettyExportSpec "" commentCol first
              forM_ rest $
                \export ->
                  do newline
                     prettyExportSpec ", " commentCol export
              newline
              write ")"
  where hasComments = any (not . null . nodeInfoComments)
        printCommentsSimple loc ast =
          let info = ann ast
              rawComments =
                filter (\l -> comInfoLocation l == Just loc) $
                nodeInfoComments info
          in do preprocessor <- gets psCommentPreprocessor
                comments <- preprocessor $ map comInfoComment rawComments
                forM_ comments $
                  printComment (Just $ srcInfoSpan $ nodeInfoSpan info)
        prettyExportSpec prefix col spec =
          do column col $ printCommentsSimple Before spec
             string prefix
             prettyNoExt spec
             printCommentsSimple After spec

-- Align import statements
extImportDecl :: Extend ImportDecl
extImportDecl ImportDecl{..} =
  do if importQualified
        then write "import qualified "
        else write "import           "
     namelen <- gets (cramerModuleImportLength . psUserState)
     if isJust importAs || isJust importSpecs
        then string $ padRight namelen $ moduleName importModule
        else string $ moduleName importModule
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
     typeSig ty
-- Half-indent for where clause, half-indent binds
extDecl (PatBind _ pat rhs mbinds) =
  do pretty pat
     pretty rhs
     maybeM_ mbinds whereBinds
extDecl other = prettyNoExt other

extConDecl :: Extend ConDecl
-- No extra space after empty constructor
extConDecl (ConDecl _ name []) = pretty name
-- Align record fields
extConDecl (RecDecl _ name fields) =
  do modifyState $ \s -> s {cramerRecordFieldLength = fieldLen}
     pretty name
     space
     case fields of
       [] -> write "{ }"
       [_] -> listAttemptSingleLine "{" "}" "," fields
       _ -> listMultiLine "{" "}" "," fields
  where fieldLen = maximum $ map (length . nameStr) fnames
        fnames =
          mapMaybe (\(FieldDecl _ ns _) ->
                      case ns of
                        [n] -> Just n
                        _ -> Nothing)
                   fields
extConDecl other = prettyNoExt other

extFieldDecl :: Extend FieldDecl
extFieldDecl (FieldDecl _ [name] ty) =
  do namelen <- gets (cramerRecordFieldLength . psUserState)
     string $ padRight namelen $ nameStr name
     space
     typeSig ty
extFieldDecl other = prettyNoExt other

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
-- Tuple types on one line, with space after comma
extType (TyTuple _ boxed tys) = withLineBreak Single $ tupleExpr boxed tys
extType other = prettyNoExt other

extPat :: Extend Pat
-- Infix application with space around operator
extPat (PInfixApp _ arg1 op arg2) =
  do pretty arg1
     space
     prettyInfixOp op
     space
     pretty arg2
-- Tuple patterns on one line, with space after comma
extPat (PTuple _ boxed pats) = withLineBreak Single $ tupleExpr boxed pats
-- List patterns on one line, with space after comma
extPat (PList _ pats) = withLineBreak Single $ listExpr pats
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
-- If-then-else on one line or newline and indent before then and else
extExp (If _ cond true false) = ifExpr id cond true false
-- Newline before in
extExp (Let _ binds expr@Do{}) = letExpr binds $ space >> pretty expr
extExp (Let _ binds expr) = letExpr binds $ newline >> indentFull (pretty expr)
-- Tuples on a single line (no space inside parens but after comma) or
-- one element per line with parens and comma aligned
extExp (Tuple _ boxed exprs) = tupleExpr boxed exprs
-- List on a single line or one item per line with aligned brackets and comma
extExp (List _ exprs) = listExpr exprs
-- Record construction and update on a single line or one line per
-- field with aligned braces and comma
extExp (RecConstr _ qname updates) = recordExpr qname updates
extExp (RecUpdate _ expr updates) = recordExpr expr updates
-- Line break and indent after do
extExp (Do _ stmts) =
  do write "do"
     newline
     indentFull . lined $ map pretty stmts
extExp other = prettyNoExt other

extStmt :: Extend Stmt
extStmt (Qualifier _ (If _ cond true false)) = ifExpr indentFull cond true false
extStmt other = prettyNoExt other

extMatch :: Extend Match
-- Indent where same as for top level decl
extMatch (Match _ name pats rhs mbinds) =
  do pretty name
     space
     spaced $ map pretty pats
     pretty rhs
     maybeM_ mbinds whereBinds
extMatch other = prettyNoExt other

-- Preserve empty lines between bindings
extBinds :: Extend Binds
extBinds (BDecls _ decls) = preserveLineSpacing decls
extBinds other = prettyNoExt other

-- No line break after equal sign
extFieldUpdate :: Extend FieldUpdate
extFieldUpdate (FieldUpdate _ qname expr) =
  do pretty qname
     write " = "
     pretty expr
extFieldUpdate other = prettyNoExt other
