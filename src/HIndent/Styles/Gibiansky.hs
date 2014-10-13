{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards, RankNTypes #-}

module HIndent.Styles.Gibiansky (gibiansky) where

import Data.Foldable

import HIndent.Pretty
import HIndent.Types

import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.SrcLoc
import Prelude hiding (exp, all, mapM_)

-- | Empty state.
data State = State

-- | The printer style.
gibiansky :: Style
gibiansky =
  Style { styleName = "gibiansky"
        , styleAuthor = "Andrew Gibiansky"
        , styleDescription = "Andrew Gibiansky's style"
        , styleInitialState = State
        , styleExtenders = [ Extender imp
                           , Extender context
                           , Extender derivings
                           , Extender typ
                           , Extender exprs
                           , Extender rhss
                           , Extender decls
                           , Extender condecls
                           ]
        , styleDefConfig =
           Config { configMaxColumns = 100
                  , configIndentSpaces = 2
                  }
        }

--------------------------------------------------------------------------------
-- Extenders

type Extend f = forall t. t -> f NodeInfo -> Printer ()


-- | Format import statements.
imp :: Extend ImportDecl
imp _ ImportDecl{..} = do
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
context _ (CxTuple _ asserts) =
  parens $ inter (comma >> space) $ map pretty asserts
context _ ctx = prettyNoExt ctx

-- | Format deriving clauses with spaces and commas between class constraints.
derivings :: Extend Deriving
derivings _ (Deriving _ instHeads) = do
  write "deriving "
  go instHeads

  where
    go insts | length insts == 1
             = pretty $ head insts
             | otherwise
             = parens $ inter (comma >> space) $ map pretty insts

-- | Format function type declarations.
typ :: Extend Type

-- For contexts, check whether the context and all following function types
-- are on the same line. If they are, print them on the same line; otherwise
-- print the context and each argument to the function on separate lines.
typ _ (TyForall _ _ (Just ctx) rest) =
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
      write  "=> "
      indented 3 $ pretty rest

typ _ ty@(TyFun _ from to) =
  -- If the function argument types are on the same line,
  -- put the entire function type on the same line.
  if all (sameLine from) $ collectTypes ty
  then do
    pretty from
    write " -> "
    pretty to
  -- If the function argument types are on different lines,
  -- write one argument type per line.
  else do
    col <- getColumn
    pretty from
    column (col - 3) $ do
      newline
      write "-> "
      indented 3 $ pretty to
typ _ t = prettyNoExt t

sameLine :: (Annotated ast, Annotated ast') => ast NodeInfo -> ast' NodeInfo -> Bool
sameLine x y = line x == line y
  where
    line :: Annotated ast => ast NodeInfo -> Int 
    line = startLine . nodeInfoSpan . ann

collectTypes :: Type l -> [Type l]
collectTypes (TyFun _ from to) = from : collectTypes to
collectTypes ty = [ty]

exprs :: Extend Exp
exprs _ exp@Let{} = letStmt exp
exprs _ exp = prettyNoExt exp

letStmt :: Exp NodeInfo -> Printer ()
letStmt (Let _ binds result) = do
  cols <- depend (write "let ") $ do
    col <- getColumn
    pretty binds
    return $ col - 4
  column cols $ do
    newline
    write "in "
    pretty result
letStmt _ = error "Not a let"

rhss :: Extend Rhs
rhss _ (UnGuardedRhs _ exp) = do
  write " = "
  pretty exp
rhss _ rhs = prettyNoExt rhs

decls :: Extend Decl
decls _ (DataDecl _ dataOrNew Nothing declHead constructors mayDeriving) = do
  pretty dataOrNew
  write " "
  pretty declHead
  case constructors of
    []  -> return ()
    [x] -> do
      write " = "
      pretty x
    (x:xs) ->
      depend (write " ") $ do
        write "= "
        pretty x
        forM_ xs $ \constructor -> do
          newline
          write "| "
          pretty constructor

  forM_ mayDeriving $ \deriv -> do
    newline
    indented 2 $ pretty deriv
decls _ decl = prettyNoExt decl

condecls :: Extend ConDecl
condecls _ (ConDecl _ name bangty) =
  depend (pretty name) $
    forM_ bangty $ \ty -> space >> pretty ty
condecls _ (RecDecl _ name fields) =
  depend (pretty name >> space) $ do
    write "{ "
    case fields of
      []         -> return ()
      [x]        -> pretty x >> space
      first:rest -> do
        pretty first
        newline
        forM_ rest $ \field -> do
          comma
          space
          pretty field
          newline
    write "}"
condecls _ other = prettyNoExt other
