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
import Language.Haskell.Exts.Comments
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
           ,Extender extDeclHead
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

-- | Return an ast node's SrcSpan.
nodeSrcSpan :: Annotated a => a NodeInfo -> SrcSpan
nodeSrcSpan = srcInfoSpan . nodeInfoSpan . ann

-- | Turn a Name into a String
nameStr :: Name a -> String
nameStr (Ident _ s) = s
nameStr (Symbol _ s) = "(" ++ s ++ ")"

-- | The difference between current column and indent level to force a
-- line break in reduceIndent.
maxDependOverhead :: Integral a => a
maxDependOverhead = 20

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

-- | Return whether a data type has only zero or one constructor.
isSingletonType :: Decl NodeInfo -> Bool
isSingletonType (DataDecl _ _ _ _ [] _) = True
isSingletonType (DataDecl _ _ _ _ [ _ ] _) = True
isSingletonType _ = False

-- | If the given String is smaller than the given length, pad on
-- right with spaces until the length matches.
padRight :: Int -> String -> String
padRight l s = take (max l (length s)) (s ++ repeat ' ')

-- | Return comments with matching location.
filterComments :: Annotated a => (Maybe ComInfoLocation -> Bool) -> a NodeInfo -> [ComInfo]
filterComments f = filter (f . comInfoLocation) . nodeInfoComments . ann

-- | Return whether an AST node has matching comments.
hasComments :: Annotated a => (Maybe ComInfoLocation -> Bool) -> a NodeInfo -> Bool
hasComments f = not . null . filterComments f

-- | Copy comments marked After from one AST node to another.
copyComments :: (Annotated ast1,Annotated ast2)
             => ComInfoLocation
             -> ast1 NodeInfo
             -> ast2 NodeInfo
             -> ast2 NodeInfo
copyComments loc from to = amap updateComments to
  where updateComments info = info { nodeInfoComments = oldComments ++ newComments }
        oldComments = filterComments (/= Just loc) to
        newComments = filterComments (== Just loc) from

-- | Return the number of line breaks between AST nodes.
lineDelta
  :: (Annotated ast1,Annotated ast2)
  => ast1 NodeInfo -> ast2 NodeInfo -> Int
lineDelta prev next = nextLine - prevLine
  where prevLine = maximum (prevNodeLine : prevCommentLines)
        nextLine = minimum (nextNodeLine : nextCommentLines)
        prevNodeLine = srcSpanEndLine . nodeSrcSpan $ prev
        nextNodeLine = srcSpanStartLine . nodeSrcSpan $ next
        prevCommentLines =
          map (srcSpanEndLine . commentSrcSpan) $
          filterComments (== Just After) prev
        nextCommentLines =
          map (srcSpanStartLine . commentSrcSpan) $
          filterComments (== Just Before) next
        commentSrcSpan = annComment . comInfoComment
        annComment (Comment _ sp _) = sp

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
  do st <- get
     let col =
           if psEolComment st
              then psIndentLevel st
              else max (psColumn st)
                       (psIndentLevel st)
     column col p

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
         do replicateM_ (max 1 $ lineDelta prev cur)
                        newline
            pretty cur

-- | `reduceIndent short long printer` produces either `short printer`
-- or `newline >> indentFull (long printer)`, depending on whether the
-- current column is sufficiently near to the current indentation depth.
--
-- The function is used to avoid overly big dependent indentation by
-- heuristically breaking and non-dependently indenting.
reduceIndent :: (Printer State () -> Printer State ())
             -> (Printer State () -> Printer State ())
             -> Printer State ()
             -> Printer State ()
reduceIndent short long printer =
  do linebreak <- gets (cramerLineBreak . psUserState)
     case linebreak of
       Single -> single
       Multi -> multi
       Free ->
         do curCol <- getColumn
            curIndent <- gets psIndentLevel
            indentSpaces <- gets (configIndentSpaces . psConfig)
            if (curCol - curIndent - indentSpaces) < maxDependOverhead
               then single
               else multi
  where single = short printer
        multi = newline >> indentFull (long printer)

-- | Either simply precede the given printer with a space, or with
-- indent the the printer after a newline, depending on the available
-- space.
spaceOrIndent :: Printer State () -> Printer State ()
spaceOrIndent = reduceIndent (\p -> space >> p) id

-- | Special casing for `do` blocks and leading comments
inlineExpr :: (Printer State () -> Printer State ()) -> Exp NodeInfo -> Printer State ()
inlineExpr _ expr
  | not (null (filterComments (== (Just Before)) expr)) =
    do newline
       indentFull $ pretty expr
inlineExpr _ expr@Do{} =
  do space
     pretty expr
inlineExpr fmt expr = fmt (pretty expr)

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
rhsExpr expr =
  do space
     rhsSeparator
     inlineExpr spaceOrIndent expr

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

letExpr :: Binds NodeInfo -> Exp NodeInfo -> Printer State ()
letExpr binds expr =
  align $
  do depend (write "let ") $ pretty binds
     newline
     write "in"
     inlineExpr (\p -> newline >> indentFull p) expr

infixExpr :: Exp NodeInfo -> Printer State ()
-- No line break before do
infixExpr (InfixApp _ arg1 op arg2@Do{}) =
  spaced [pretty arg1,pretty op,pretty arg2]
-- Try to preserve existing line break before and after infix ops
infixExpr (InfixApp _ arg1 op arg2)
  | deltaBefore /= 0 && deltaAfter /= 0 =
    align $ inter newline [pretty arg1,pretty op,pretty arg2]
  | deltaBefore /= 0 || deltaAfter /= 0 =
    pretty arg1 >>
    preserveLinebreak
      deltaBefore
      (pretty op >>
       preserveLinebreak deltaAfter
                         (pretty arg2))
  | otherwise = attemptSingleLine single multi
  where single = spaced [pretty arg1,pretty op,pretty arg2]
        multi =
          do pretty arg1
             space
             pretty op
             newline
             indentFull $ pretty arg2
        preserveLinebreak delta p =
          if delta > 0
             then newline >> indentFull p
             else space >> p
        deltaBefore = lineDelta arg1 op
        deltaAfter = lineDelta op arg2
infixExpr _ = error "not an InfixApp"

applicativeExpr :: Exp NodeInfo
                -> [(QOp NodeInfo,Exp NodeInfo)]
                -> Printer State ()
applicativeExpr ctor args = attemptSingleLine single multi
  where single = spaced (pretty ctor : map prettyArg args)
        multi =
          do pretty ctor
             depend space $ inter newline $ map prettyArg args
        prettyArg (op,arg) = pretty op >> space >> pretty arg

typeSig :: Type NodeInfo -> Printer State ()
typeSig ty =
  attemptSingleLineType (write ":: " >> pretty ty)
                        (align $ write ":: " >> pretty ty)

typeInfixExpr
  :: Type NodeInfo -> Printer State ()
-- As HIndent does not know about operator precedence, preserve
-- existing line breaks, but do not add new ones.
typeInfixExpr (TyInfix _ arg1 op arg2)
  | deltaBefore /= 0 && deltaAfter /= 0 =
    align $ inter newline [pretty arg1,prettyInfixOp op,pretty arg2]
  | deltaBefore /= 0 || deltaAfter /= 0 =
    pretty arg1 >>
    preserveLinebreak
      deltaBefore
      (prettyInfixOp op >>
       preserveLinebreak deltaAfter
                         (pretty arg2))
  | otherwise = spaced [pretty arg1,prettyInfixOp op,pretty arg2]
  where preserveLinebreak delta p =
          if delta > 0
             then newline >> indentFull p
             else space >> p
        deltaBefore = lineDelta arg1 op
        deltaAfter = lineDelta op arg2
typeInfixExpr _ = error "not a TyInfix"

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
-- Avoid increasing whitespace after OPTIONS string
extModulePragma (OptionsPragma _ mtool opt) =
  do write "{-# OPTIONS"
     maybeM_ mtool $ \tool -> do write "_"
                                 string $ prettyPrint tool
     space
     string $ trim opt
     write " #-}"
  where trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
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
      | not (hasComments (const True) e) -> write " ( " >> pretty e >> write " )"
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
  where printCommentsSimple loc ast =
          let rawComments = filterComments (== Just loc) ast
          in do preprocessor <- gets psCommentPreprocessor
                comments <- preprocessor $ map comInfoComment rawComments
                forM_ comments $
                  printComment (Just $ nodeSrcSpan ast)
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
-- No dependent indentation for type decls
extDecl (TypeDecl _ declhead ty) =
  do write "type "
     pretty declhead
     write " = "
     indentFull $ pretty ty
-- Fix whitespace before 'where' in class decl
extDecl (ClassDecl _ mcontext declhead fundeps mdecls) =
  do depend (write "class ") $
       depend (maybeCtx mcontext) $
         depend (pretty declhead) $
           depend (unless (null fundeps) $
               write " | " >> inter (write ", ") (map pretty fundeps)) $
             when (isJust mdecls) $ write " where"
     maybeM_ mdecls $
       \decls ->
         do newline
            indentFull $ preserveLineSpacing decls
-- Align data constructors
extDecl decl@(DataDecl _ dataOrNew mcontext declHead constructors mderiv) =
  do mapM_ pretty mcontext
     pretty dataOrNew
     space
     pretty declHead
     write " ="
     if isEnum decl || isSingletonType decl
        then attemptSingleLine single multi
        else multi
     maybeM_ mderiv $ \deriv -> indentFull $ newline >> pretty deriv
  where single =
          do space
             inter (write " | ") $ map pretty constructors
        multi =
          reduceIndent
            (depend space . indented (-2))
            (\p -> write "  " >> p)
            (inter (newline >> write "| ") $ map pretty constructors)
-- Type signature either on a single line or split at arrows, aligned with '::'
extDecl (TypeSig _ names ty) =
  do inter (write ", ") $ map pretty names
     space
     typeSig ty
-- Preserve empty lines between function matches
extDecl (FunBind  _ matches) = preserveLineSpacing matches
-- Half-indent for where clause, half-indent binds
extDecl (PatBind _ pat rhs mbinds) =
  do pretty pat
     withCaseContext False $ pretty rhs
     maybeM_ mbinds whereBinds
extDecl other = prettyNoExt other

-- Do not modify indent level
extDeclHead :: Extend DeclHead
extDeclHead (DHApp _ dhead var) =
    do pretty dhead
       space
       pretty var
extDeclHead other = prettyNoExt other

extConDecl :: Extend ConDecl
-- No extra space after empty constructor
extConDecl (ConDecl _ name []) = pretty name
extConDecl (ConDecl _ name tys) = attemptSingleLine single multi
    where single = spaced $ pretty name : map pretty tys
          multi = depend (pretty name >> space) $ lined $ map pretty tys
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
extType (TyForall _ mforall mcontext ty) = attemptSingleLine single multi
  where single =
          do maybeM_ mforall $ \vars -> prettyForall vars >> space
             maybeM_ mcontext $ \context -> pretty context >> write " => "
             pretty ty
        multi =
          do maybeM_ mforall $ \vars -> prettyForall vars >> newline
             maybeM_ mcontext $
               \context -> pretty context >> newline >> write "=> "
             pretty ty
        prettyForall vars =
          do write "forall "
             spaced $ map pretty vars
             write "."
-- Type signature should line break at each arrow if necessary
extType (TyFun _ from to) =
  attemptSingleLineType (pretty from >> write " -> " >> pretty to)
                        (pretty from >> newline >> write "-> " >> pretty to)
-- Parentheses reset forced line breaking
extType (TyParen _ ty) = withLineBreak Free $ parens $ pretty ty
-- Tuple types on one line, with space after comma
extType (TyTuple _ boxed tys) = withLineBreak Free $ tupleExpr boxed tys
-- Infix application
extType expr@TyInfix{} = typeInfixExpr expr
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
        collectArgs app@(App _ g y) =
          let (f,args) = collectArgs g
          in (f,copyComments After app y : args)
        collectArgs nonApp = (nonApp,[])
-- Infix application on a single line or indented rhs
extExp expr@InfixApp{} =
  if all (isApplicativeOp . fst) opArgs && isFmap (fst $ head opArgs)
     then applicativeExpr firstArg opArgs
     else infixExpr expr
  where (firstArg,opArgs) = collectOpExps expr
        collectOpExps
          :: Exp NodeInfo -> (Exp NodeInfo,[(QOp NodeInfo,Exp NodeInfo)])
        collectOpExps app@(InfixApp _ left op right) =
          let (ctorLeft,argsLeft) = collectOpExps left
              (ctorRight,argsRight) = collectOpExps right
          in (ctorLeft,argsLeft ++ [(op,copyComments After app ctorRight)] ++ argsRight)
        collectOpExps e = (e,[])
        isApplicativeOp :: QOp NodeInfo -> Bool
        isApplicativeOp (QVarOp _ (UnQual _ (Symbol _ s))) =
          head s == '<' && last s == '>'
        isApplicativeOp _ = False
        isFmap :: QOp NodeInfo -> Bool
        isFmap (QVarOp _ (UnQual _ (Symbol _ "<$>"))) = True
        isFmap _ = False
-- No space after lambda
extExp (Lambda _ pats expr) =
  do write "\\"
     maybeSpace
     spaced $ map pretty pats
     write " ->"
     inlineExpr (\p -> attemptSingleLine (space >> p) (spaceOrIndent p)) expr
  where maybeSpace =
          case pats of
            PBangPat{}:_ -> space
            PIrrPat{}:_ -> space
            _ -> return ()
-- If-then-else on one line or newline and indent before then and else
extExp (If _ cond true false) = ifExpr id cond true false
-- Newline before in
extExp (Let _ binds expr) = letExpr binds expr
-- Tuples on a single line (no space inside parens but after comma) or
-- one element per line with parens and comma aligned
extExp (Tuple _ boxed exprs) = tupleExpr boxed exprs
-- List on a single line or one item per line with aligned brackets and comma
extExp (List _ exprs) = listExpr exprs
-- Record construction and update on a single line or one line per
-- field with aligned braces and comma
extExp (RecConstr _ qname updates) = recordExpr qname updates
extExp (RecUpdate _ expr updates) = recordExpr expr updates
-- Full indentation for case alts and preserve empty lines between alts
extExp (Case _ expr alts) =
  do write "case "
     pretty expr
     write " of"
     newline
     withCaseContext True $ indentFull $ preserveLineSpacing alts
-- Line break and indent after do
extExp (Do _ stmts) =
  do write "do"
     newline
     indentFull $ preserveLineSpacing stmts
extExp (ListComp _ e qstmt) =
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
-- Type signatures like toplevel decl
extExp (ExpTypeSig _ expr ty) =
  do pretty expr
     space
     typeSig ty
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
     withCaseContext False $ pretty rhs
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
