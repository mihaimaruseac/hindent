{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards, RankNTypes #-}
{-# OPTIONS_GHC  -fno-warn-name-shadowing  #-}

module HIndent.Styles.Gibiansky where

import           Data.Foldable
import           Control.Applicative ((<$>))
import           Control.Monad (unless, when, replicateM_)
import           Control.Monad.State (gets, get, put)
import           Data.Maybe (isNothing)

import           HIndent.Pretty
import           HIndent.Types

import           Language.Haskell.Exts.Annotated.Syntax
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Comments
import           Prelude hiding (exp, all, mapM_, minimum, and, maximum, any)

-- | Empty state.
data State = State { gibianskyForceSingleLine :: Bool }

-- | The printer style.
gibiansky :: Style
gibiansky = Style { styleName = "gibiansky"
                  , styleAuthor = "Andrew Gibiansky"
                  , styleDescription = "Andrew Gibiansky's style"
                  , styleInitialState = State { gibianskyForceSingleLine = False }
                  , styleExtenders = [ Extender imp
                                     , Extender modl
                                     , Extender context
                                     , Extender derivings
                                     , Extender typ
                                     , Extender exprs
                                     , Extender rhss
                                     , Extender guardedRhs
                                     , Extender decls
                                     , Extender condecls
                                     , Extender alt
                                     , Extender moduleHead
                                     , Extender exportList
                                     , Extender fieldUpdate
                                     , Extender pragmas
                                     , Extender pat
                                     ]
                  , styleDefConfig = defaultConfig { configMaxColumns = 100
                                                   , configIndentSpaces = indentSpaces
                                                   , configClearEmptyLines = True
                                                   }
                  }

-- | Number of spaces to indent by.
indentSpaces :: Integral a => a
indentSpaces = 2

-- | Printer to indent one level.
indentOnce :: Printer s ()
indentOnce = replicateM_ indentSpaces $ write " "

-- | How many exports to format in a single line.
-- If an export list has more than this, it will be formatted as multiple lines.
maxSingleLineExports :: Integral a => a
maxSingleLineExports = 4

attemptSingleLine :: Printer State a -> Printer State a -> Printer State a
attemptSingleLine single multiple = do
  prevState <- get
  if gibianskyForceSingleLine $ psUserState prevState
    then single
    else do
      -- Try printing on one line.
      modifyState $ \st -> st { gibianskyForceSingleLine = True }
      result <- single
      modifyState $ \st -> st { gibianskyForceSingleLine = False }

      --  If it doesn't fit, reprint on multiple lines.
      col <- getColumn
      maxColumns <- configMaxColumns <$> gets psConfig
      if col > maxColumns
        then do
          put prevState
          multiple
        else return result

--------------------------------------------------------------------------------
-- Extenders
type Extend f = f NodeInfo -> Printer State ()

-- | Format whole modules.
modl :: Extend Module
modl (Module _ mayModHead pragmas imps decls) = do
  onSeparateLines pragmas
  unless (null pragmas) $
    unless (null imps && null decls && isNothing mayModHead) $
      newline >> newline

  forM_ mayModHead $ \modHead -> do
    pretty modHead
    unless (null imps && null decls) (newline >> newline)

  onSeparateLines imps
  unless (null imps || null decls) (newline >> newline)
  onSeparateLines decls
modl m = prettyNoExt m

-- | Format pragmas differently (language pragmas).
pragmas :: Extend ModulePragma
pragmas (LanguagePragma _ names) = do
  write "{-# LANGUAGE "
  inter (write ", ") $ map pretty names
  write " #-}"
pragmas p = prettyNoExt p

-- | Format patterns.
pat :: Extend Pat
pat (PTuple _ boxed pats) = writeTuple boxed pats
pat (PList _ pats) = singleLineList pats
pat (PRec _ name fields) = recUpdateExpr (pretty name) (map pretty fields)
pat p = prettyNoExt p

-- | Format import statements.
imp :: Extend ImportDecl
imp ImportDecl{..} = do
  write "import "
  write $ if importQualified
            then "qualified "
            else "          "
  pretty importModule

  forM_ importAs $ \name -> do
    write " as "
    pretty name

  forM_ importSpecs $ \speclist -> do
    write " "
    pretty speclist

-- | Format contexts with spaces and commas between class constraints.
context :: Extend Context
context (CxTuple _ asserts) =
  parens $ inter (comma >> space) $ map pretty asserts
context ctx = prettyNoExt ctx

-- | Format deriving clauses with spaces and commas between class constraints.
derivings :: Extend Deriving
derivings (Deriving _ instHeads) = do
  write "deriving "
  go instHeads

  where
    go insts
      | length insts == 1 = pretty $ head insts
      | otherwise = parens $ inter (comma >> space) $ map pretty insts

-- | Format function type declarations.
typ :: Extend Type
-- For contexts, check whether the context and all following function types
-- are on the same line. If they are, print them on the same line; otherwise
-- print the context and each argument to the function on separate lines.
typ (TyForall _ _ (Just ctx) rest) =
  if all (sameLine ctx) $ collectTypes rest
    then do
      pretty ctx
      write " => "
      pretty rest
    else do
      col <- getColumn
      pretty ctx
      column (col - 3) $ do
        newline
        write "=> "
        indented 3 $ pretty rest
typ (TyTuple _ boxed types) = writeTuple boxed types
typ ty@(TyFun _ from to) =
  -- If the function argument types are on the same line,
  -- put the entire function type on the same line.
  if all (sameLine from) $ collectTypes ty
    then do
      pretty from
      write " -> "
      pretty to
    else do
      -- If the function argument types are on different lines,
      -- write one argument type per line.
      col <- getColumn
      pretty from
      column (col - 3) $ do
        newline
        write "-> "
        indented 3 $ pretty to
typ t = prettyNoExt t

writeTuple :: Pretty ast => Boxed -> [ast NodeInfo] -> Printer State ()
writeTuple boxed vals = parens $ do
  boxed'
  inter (write ", ") $ map pretty vals
  boxed'
  where
    boxed' =
      case boxed of
        Boxed   -> return ()
        Unboxed -> write "#"

sameLine :: (Annotated ast, Annotated ast') => ast NodeInfo -> ast' NodeInfo -> Bool
sameLine x y = line x == line y
  where
    line :: Annotated ast => ast NodeInfo -> Int
    line = startLine . nodeInfoSpan . ann

collectTypes :: Type l -> [Type l]
collectTypes (TyFun _ from to) = from : collectTypes to
collectTypes ty = [ty]

exprs :: Extend Exp
exprs exp@Let{} = letExpr exp
exprs exp@App{} = appExpr exp
exprs exp@Do{} = doExpr exp
exprs exp@List{} = listExpr exp
exprs exp@(InfixApp _ _ (QVarOp _ (UnQual _ (Symbol _ "$"))) _) = dollarExpr exp
exprs exp@(InfixApp _ _ (QVarOp _ (UnQual _ (Symbol _ "<*>"))) _) = applicativeExpr exp
exprs exp@InfixApp{} = opExpr exp
exprs exp@Lambda{} = lambdaExpr exp
exprs exp@Case{} = caseExpr exp
exprs exp@LCase{} = lambdaCaseExpr exp
exprs exp@If{} = ifExpr exp
exprs (RecUpdate _ exp updates) = recUpdateExpr (pretty exp) (map pretty updates)
exprs (RecConstr _ qname updates) = recUpdateExpr (pretty qname) (map pretty updates)
exprs (Tuple _ _ exps) = parens $ inter (write ", ") $ map pretty exps
exprs exp = prettyNoExt exp

letExpr :: Exp NodeInfo -> Printer State ()
letExpr (Let _ binds result) = do
  cols <- depend (write "let ") $ do
            col <- getColumn
            writeWhereBinds binds
            return $ col - 4
  column cols $ do
    newline
    write "in "
    pretty result
letExpr _ = error "Not a let"

keepingColumn :: Printer State () -> Printer State ()
keepingColumn printer = do
  col <- getColumn
  ind <- gets psIndentLevel
  column (max col ind) printer

appExpr :: Exp NodeInfo -> Printer State ()
appExpr app@(App _ f x) = do
  prevState <- get
  prevLine <- getLineNum
  attemptSingleLine singleLine multiLine
  curLine <- getLineNum

  -- If the multiline version takes more than two lines,
  -- print everything with one argument per line.
  when (curLine - prevLine > 1) $ do
    -- Restore to before printing.
    put prevState

    allArgsSeparate <- not <$> canSingleLine (pretty f)
    if allArgsSeparate
      then separateArgs app
      else keepingColumn $ do
        pretty f
        newline
        indented indentSpaces $ pretty x

  where
    singleLine = spaced [pretty f, pretty x]
    multiLine = keepingColumn $ do
      pretty f
      newline
      indentOnce
      pretty x

    canSingleLine :: Printer State a -> Printer State Bool
    canSingleLine printer = do
      st <- get
      prevLine <- getLineNum
      _ <- printer
      curLine <- getLineNum
      put st
      return $ prevLine == curLine

    -- Separate a function application into the function
    -- and all of its arguments. Arguments are returned in reverse order.
    collectArgs :: Exp NodeInfo -> (Exp NodeInfo, [Exp NodeInfo])
    collectArgs (App _ g y) =
      let (fun, args) = collectArgs g
      in (fun, y : args)
    collectArgs nonApp = (nonApp, [])

    separateArgs :: Exp NodeInfo -> Printer State ()
    separateArgs expr =
      let (fun, args) = collectArgs expr
      in keepingColumn $ do
        pretty fun
        newline
        indented indentSpaces $ lined $ map pretty $ reverse args
appExpr _ = error "Not an app"

doExpr :: Exp NodeInfo -> Printer State ()
doExpr (Do _ stmts) = do
  write "do"
  newline
  indented indentSpaces $ onSeparateLines stmts
doExpr _ = error "Not a do"

listExpr :: Exp NodeInfo -> Printer State ()
listExpr (List _ els) = attemptSingleLine (singleLineList els) (multiLineList els)
listExpr _ = error "Not a list"

singleLineList :: Pretty a => [a NodeInfo] -> Printer State ()
singleLineList exps = do
  write "["
  inter (write ", ") $ map pretty exps
  write "]"

multiLineList :: [Exp NodeInfo] -> Printer State ()
multiLineList [] = write "[]"
multiLineList (first:exps) = keepingColumn $ do
  write "[ "
  pretty first
  forM_ exps $ \el -> do
    newline
    write ", "
    pretty el
  newline
  write "]"

dollarExpr :: Exp NodeInfo -> Printer State ()
dollarExpr (InfixApp _ left op right) = do
  pretty left
  write " "
  pretty op
  if needsNewline right
    then do
      newline
      depend indentOnce $ pretty right
    else do
      write " "
      pretty right

  where
    needsNewline Case{} = True
    needsNewline exp = lineDelta exp op > 0
dollarExpr _ = error "Not an application"

applicativeExpr :: Exp NodeInfo -> Printer State ()
applicativeExpr exp@InfixApp{} =
  case applicativeArgs of
    Just (first:second:rest) ->
      attemptSingleLine (singleLine first second rest) (multiLine first second rest)
    _ -> prettyNoExt exp
  where
    singleLine :: Exp NodeInfo -> Exp NodeInfo -> [Exp NodeInfo] -> Printer State ()
    singleLine first second rest = spaced
                                     [ pretty first
                                     , write "<$>"
                                     , pretty second
                                     , write "<*>"
                                     , inter (write " <*> ") $ map pretty rest
                                     ]

    multiLine :: Exp NodeInfo -> Exp NodeInfo -> [Exp NodeInfo] -> Printer State ()
    multiLine first second rest = do
      pretty first
      depend (write " ") $ do
        write "<$> "
        pretty second
        forM_ rest $ \val -> do
          newline
          write "<*> "
          pretty val

    applicativeArgs :: Maybe [Exp NodeInfo]
    applicativeArgs = collectApplicativeExps exp

    collectApplicativeExps :: Exp NodeInfo -> Maybe [Exp NodeInfo]
    collectApplicativeExps (InfixApp _ left op right)
      | isFmap op = return [left, right]
      | isAp op = do
          start <- collectApplicativeExps left
          return $ start ++ [right]
      | otherwise = Nothing
    collectApplicativeExps x = return [x]

    isFmap :: QOp NodeInfo -> Bool
    isFmap (QVarOp _ (UnQual _ (Symbol _ "<$>"))) = True
    isFmap _ = False

    isAp :: QOp NodeInfo -> Bool
    isAp (QVarOp _ (UnQual _ (Symbol _ "<*>"))) = True
    isAp _ = False
applicativeExpr _ = error "Not an application"

opExpr :: Exp NodeInfo -> Printer State ()
opExpr (InfixApp _ left op right) = keepingColumn $ do
  pretty left

  let delta = lineDelta op left
  if delta == 0
    then space
    else replicateM_ delta newline

  pretty op

  let delta = lineDelta right op
  if delta == 0
    then space
    else replicateM_ delta newline

  pretty right
opExpr exp = prettyNoExt exp

lambdaExpr :: Exp NodeInfo -> Printer State ()
lambdaExpr (Lambda _ pats exp) = do
  write "\\"
  spaced $ map pretty pats
  write " ->"
  attemptSingleLine (write " " >> pretty exp) $ do
    newline
    indentOnce
    pretty exp
lambdaExpr _ = error "Not a lambda"

caseExpr :: Exp NodeInfo -> Printer State ()
caseExpr (Case _ exp alts) = do
  depend (write "case ") $ do
    pretty exp
    write " of"
  newline

  writeCaseAlts alts
caseExpr _ = error "Not a case"

lambdaCaseExpr :: Exp NodeInfo -> Printer State ()
lambdaCaseExpr (LCase _ alts) = do
  write "\\case"
  newline
  writeCaseAlts alts
lambdaCaseExpr _ = error "Not a lambda case"

ifExpr :: Exp NodeInfo -> Printer State ()
ifExpr (If _ cond thenExpr elseExpr) =
  depend (write "if") $ do
    write " "
    pretty cond
    newline
    write "then "
    pretty thenExpr
    newline
    write "else "
    pretty elseExpr
ifExpr _ = error "Not an if statement"

writeCaseAlts :: [Alt NodeInfo] -> Printer State ()
writeCaseAlts alts = do
  allSingle <- and <$> mapM isSingle alts
  withCaseContext True $ indented indentSpaces $ do
    prettyPr <- if allSingle
                then do
                  maxPatLen <- maximum <$> mapM (patternLen . altPattern) alts
                  return $ prettyCase (Just maxPatLen)
                else return $ prettyCase Nothing

    case alts of 
      [] -> return ()
      first:rest -> do
        printComments Before first
        prettyPr first
        forM_ (zip alts rest) $ \(prev, cur) -> do
          replicateM_ (max 1 $ lineDelta cur prev) newline
          printComments Before cur
          prettyPr cur

  where
    isSingle :: Alt NodeInfo -> Printer State Bool
    isSingle alt' = fst <$> sandbox
                              (do
                                 line <- gets psLine
                                 pretty alt'
                                 line' <- gets psLine
                                 return $ line == line')

    altPattern :: Alt l -> Pat l
    altPattern (Alt _ p _ _) = p

    patternLen :: Pat NodeInfo -> Printer State Int
    patternLen pat = fromIntegral <$> fst <$> sandbox
                                                (do
                                                   col <- getColumn
                                                   pretty pat
                                                   col' <- getColumn
                                                   return $ col' - col)

    prettyCase :: Maybe Int -> Alt NodeInfo -> Printer State ()
    prettyCase mpatlen (Alt _ p galts mbinds) = do
      -- Padded pattern
      case mpatlen of
        Just patlen -> do
          col <- getColumn
          pretty p
          col' <- getColumn
          replicateM_ (patlen - fromIntegral (col' - col)) space
        Nothing -> pretty p

      case galts of
        UnGuardedRhs{} -> pretty galts
        GuardedRhss{}  -> indented indentSpaces $ pretty galts

      --  Optional where clause!
      forM_ mbinds $ \binds -> do
        newline
        indented indentSpaces $ depend (write "where ") (pretty binds)


recUpdateExpr :: Printer State () -> [Printer State ()] -> Printer State ()
recUpdateExpr expWriter updates =
  if null updates
    then do
      expWriter
      write "{}"
    else attemptSingleLine single mult

  where
    single = do
      expWriter
      write " { "
      inter (write ", ") updates
      write " }"
    mult = do
      expWriter
      newline
      indented indentSpaces $ keepingColumn $ do
        write "{ "
        head updates
        forM_ (tail updates) $ \update -> do
          newline
          write ", "
          update
        newline
        write "}"

rhss :: Extend Rhs
rhss (UnGuardedRhs rhsLoc exp) = do
  write " "
  rhsSeparator
  if onNextLine exp
    then indented indentSpaces $ do
      newline
      pretty exp
    else do
      space
      pretty exp

  where
    -- Cannot use lineDelta because we need to look at rhs start line, not end line
    prevLine = srcSpanStartLine . srcInfoSpan . nodeInfoSpan $ rhsLoc
    curLine = astStartLine exp
    emptyLines = curLine - prevLine

    onNextLine Let{} = True
    onNextLine Case{} = True
    onNextLine _ = emptyLines > 0
rhss (GuardedRhss _ rs) =
  lined $ flip map rs $ \a@(GuardedRhs _ stmts exp) -> do
    let manyStmts = length stmts > 1
        remainder = do
          if manyStmts then newline else write " "
          rhsRest exp
        writeStmts = 
          case stmts of
            x:xs -> do
              pretty x
              forM_ xs $ \x -> write "," >> newline >> pretty x
            [] -> return ()

    printComments Before a
    if manyStmts
      then do 
        depend (write "| ") writeStmts
        remainder
      else
        depend (write "| ") $ writeStmts >> remainder


guardedRhs :: Extend GuardedRhs
guardedRhs (GuardedRhs _ stmts exp) = do
  indented 1 $ prefixedLined "," (map (\p -> space >> pretty p) stmts)
  write " "
  rhsRest exp

rhsRest :: Pretty ast => ast NodeInfo -> Printer State ()
rhsRest exp = do
  rhsSeparator
  write " "
  pretty exp

decls :: Extend Decl
decls (DataDecl _ dataOrNew Nothing declHead constructors mayDeriving) = do
  depend (pretty dataOrNew >> write " ") $ do
    pretty declHead
    case constructors of
      [] -> return ()
      [x] -> do
        write " ="
        pretty x
      (x:xs) ->
        depend (write " ") $ do
          write "="
          pretty x
          forM_ xs $ \constructor -> do
            newline
            write "|"
            pretty constructor

  forM_ mayDeriving $ \deriv -> do
    newline
    indented indentSpaces $ pretty deriv
decls (PatBind _ pat rhs mbinds) = funBody [pat] rhs mbinds
decls (FunBind _ matches) =
  lined $ flip map matches $ \match -> do
    (name, pat, rhs, mbinds) <- case match of
                                  Match _ name pat rhs mbinds -> return (name, pat, rhs, mbinds)
                                  InfixMatch _ left name pat rhs mbinds -> do
                                    pretty left
                                    write " "
                                    return (name, pat, rhs, mbinds)

    case name of
      Symbol _ name' -> string name'
      name' -> pretty name'
    write " "
    funBody pat rhs mbinds
decls decl = prettyNoExt decl

funBody :: [Pat NodeInfo] -> Rhs NodeInfo -> Maybe (Binds NodeInfo) -> Printer State ()
funBody pat rhs mbinds = do
  spaced $ map pretty pat

  withCaseContext False $
    case rhs of
      UnGuardedRhs{} -> pretty rhs
      GuardedRhss{} -> do
        newline
        indented indentSpaces $ pretty rhs

  -- Process the binding group, if it exists.
  forM_ mbinds $ \binds -> do
    newline
    -- Add an extra newline after do blocks.
    when (isDoBlock rhs) newline
    indented indentSpaces $ do
      write "where"
      newline
      indented indentSpaces $ writeWhereBinds binds

writeWhereBinds :: Binds NodeInfo -> Printer State ()
writeWhereBinds ds@(BDecls _ binds) = do
  printComments Before ds
  onSeparateLines binds
writeWhereBinds binds = prettyNoExt binds

-- Print all the ASTs on separate lines, respecting user spacing.
onSeparateLines :: (Pretty ast, Annotated ast) => [ast NodeInfo] -> Printer State ()
onSeparateLines [] = return ()
onSeparateLines vals = do
  let vals' = map (amap fixSpans) vals
      (first:rest) = vals'

  
  pretty first
  forM_ (zip vals' rest) $ \(prev, cur) -> do
    replicateM_ (max 1 $ lineDelta cur prev) newline
    pretty cur

fixSpans :: NodeInfo -> NodeInfo
fixSpans info =
  let infoSpan = nodeInfoSpan info
      srcSpan = srcInfoSpan infoSpan

      points = srcInfoPoints infoSpan
      lastPt = last points

      prevLastPt = last (init points)
      prevPtEnd = (srcSpanEndLine prevLastPt, srcSpanEndColumn prevLastPt)

      lastPtEndLoc = (srcSpanEndLine lastPt, srcSpanEndColumn lastPt)
      invalidLastPt = srcSpanStartLine lastPt == srcSpanEndLine lastPt &&
                      srcSpanStartColumn lastPt > srcSpanEndColumn lastPt

      infoEndLoc = (srcSpanEndLine srcSpan, srcSpanEndColumn srcSpan)
  in if length points > 1 && lastPtEndLoc == infoEndLoc && invalidLastPt
       then info { nodeInfoSpan = infoSpan { srcInfoSpan = setEnd srcSpan prevPtEnd } }
       else info
  where
    setEnd (SrcSpan fname startL startC _ _) (endL, endC) = SrcSpan fname startL startC endL endC


astStartLine :: Annotated ast => ast NodeInfo -> Int
astStartLine decl =
  let info = ann decl
      comments = nodeInfoComments info
      befores = filter ((== Just Before) . comInfoLocation) comments
      commentStartLine (Comment _ sp _) = srcSpanStartLine sp
  in if null befores
       then startLine $ nodeInfoSpan info
       else minimum $ map (commentStartLine . comInfoComment) befores

isDoBlock :: Rhs l -> Bool
isDoBlock (UnGuardedRhs _ Do{}) = True
isDoBlock _ = False

condecls :: Extend ConDecl
condecls (ConDecl _ name bangty) =
  depend (space >> pretty name) $
    forM_ bangty $ \ty -> space >> pretty ty
condecls decl@(RecDecl _ name fields) = if hasComments decl
                                        then multiRec
                                        else attemptSingleLine singleRec multiRec
  where
    singleRec = space >> depend (pretty name >> space) recBody
    multiRec = do
      newline
      indented indentSpaces $ keepingColumn $ do
        pretty name
        newline
        indented indentSpaces recBody

    recBody = do
      write "{ "
      writeFields fields
      write "}"

    writeFields [] = return ()
    writeFields [x] = do
      pretty x
      eol <- gets psEolComment
      unless eol space
    writeFields (first:rest) = do
        singleLine <- gets (gibianskyForceSingleLine . psUserState)

        pretty first
        unless singleLine newline
        forM_ rest $ \field -> do
          comma
          space
          pretty field
          unless singleLine newline

        when singleLine space
condecls other = prettyNoExt other

hasComments :: Foldable ast => ast NodeInfo -> Bool
hasComments = any (not . null . nodeInfoComments)

alt :: Extend Alt
alt (Alt _ p rhs mbinds) = do
  pretty p
  case rhs of
    UnGuardedRhs{} -> pretty rhs
    GuardedRhss{}  -> indented indentSpaces $ pretty rhs
  forM_ mbinds $ \binds -> do
    newline
    indented indentSpaces $
      depend (write "where ") (pretty binds)

moduleHead :: Extend ModuleHead
moduleHead (ModuleHead _ name mwarn mexports) = do
  forM_ mwarn pretty
  write "module "
  pretty name
  forM_ mexports $ \exports -> do
    space
    pretty exports
  write " where"

exportList :: Extend ExportSpecList
exportList (ExportSpecList _ exports) = do
  write "("
  if length exports <= maxSingleLineExports
    then do
      inter (write ", ") $ map pretty exports
      write ")"
    else indented indentSpaces' $ do
      -- First export
      let first:rest = exports
      newline
      pretty first
      write ","

      forM_ (zip rest exports) $ \(cur, prev) -> do
        replicateM_ (max 1 $ lineDelta cur prev) newline
        pretty cur
        write ","
      newline
      write ")"

  where
    indentSpaces' = 2 * indentSpaces

lineDelta :: (Annotated ast1, Annotated ast2) => ast1 NodeInfo -> ast2 NodeInfo -> Int
lineDelta cur prev = emptyLines
  where
    prevLine = srcSpanEndLine . srcInfoSpan . nodeInfoSpan . ann $ prev
    curLine = astStartLine cur
    emptyLines = curLine - prevLine

fieldUpdate :: Extend FieldUpdate
fieldUpdate (FieldUpdate _ name val) = do
  pretty name
  write " = "
  pretty val
fieldUpdate upd = prettyNoExt upd
