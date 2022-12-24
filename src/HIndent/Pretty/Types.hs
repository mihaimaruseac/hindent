{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Types to pretty-print certain parts of Haskell codes.
--
-- We define new types to pretty-print AST nodes rather than define
-- functions to print comments easily using the 'Pretty' implementation of
-- 'GenLocated'.
module HIndent.Pretty.Types
  ( InfixExpr(..)
  , InfixOp(..)
  , PrefixOp(..)
  , InfixApp(..)
  , GRHSsExpr(..)
  , GRHSExpr(..)
  , GRHSProc(..)
  , RecConPat(..)
  , RecConField(..)
  , HsSigType'(..)
  , pattern HsSigTypeInsideInstDecl
  , pattern HsSigTypeInsideVerticalFuncSig
  , pattern HsSigTypeInsideDeclSig
  , HsType'(..)
  , pattern HsTypeInsideVerticalFuncSig
  , pattern HsTypeInsideDeclSig
  , pattern HsTypeInsideInstDecl
  , StmtLRInsideVerticalList(..)
  , ParStmtBlockInsideVerticalList(..)
  , DeclSig(..)
  , TopLevelTyFamInstDecl(..)
  , Context(..)
  , HorizontalContext(..)
  , VerticalContext(..)
  , ModuleNameWithPrefix(..)
  , PatInsidePatDecl(..)
  , LambdaCase(..)
  , ModuleDeprecatedPragma(..)
  , ListComprehension(..)
  , DoExpression(..)
  , DoOrMdo(..)
  , LetIn(..)
  , NodeComments(..)
  , GRHSExprType(..)
  , GRHSProcType(..)
  , HsTypeFor(..)
  , HsTypeDir(..)
  , CaseOrCases(..)
  ) where

import           GHC.Hs
import           GHC.Types.Name.Reader
import           GHC.Unit
import           GHC.Unit.Module.Warnings

-- | `LHsExpr` used as a infix operator
newtype InfixExpr =
  InfixExpr (LHsExpr GhcPs)

newtype InfixOp =
  InfixOp RdrName

-- | A wrapper type for printing an identifier as a prefix operator.
--
-- Printing a `PrefixOp` value containing a symbol operator wraps it with
-- parentheses.
newtype PrefixOp =
  PrefixOp RdrName

-- | An infix operator application.
--
-- `immediatelyAfterDo` is `True` if an application is next to a `do`
-- keyword. It needs an extra indent in such cases because
--
-- > do a
-- > * b
--
-- is not a valid Haskell code.
data InfixApp =
  InfixApp
    { lhs                :: LHsExpr GhcPs
    , op                 :: LHsExpr GhcPs
    , rhs                :: LHsExpr GhcPs
    , immediatelyAfterDo :: Bool
    }

-- | `GRHSs` with a label indicating in which context the RHS is located
-- in.
data GRHSsExpr =
  GRHSsExpr
    { grhssExprType :: GRHSExprType
    , grhssExpr     :: GRHSs GhcPs (LHsExpr GhcPs)
    }

-- | 'GRHS' for a normal binding.
data GRHSExpr =
  GRHSExpr
    { grhsExprType :: GRHSExprType
    , grhsExpr     :: GRHS GhcPs (LHsExpr GhcPs)
    }

-- | 'GRHS' for a @proc@ binding.
newtype GRHSProc =
  GRHSProc (GRHS GhcPs (LHsCmd GhcPs))

-- | A pattern match against a record.
newtype RecConPat =
  RecConPat (HsRecFields GhcPs (LPat GhcPs))
#if MIN_VERSION_ghc_lib_parser(9,4,1)
-- | A record field in a pattern match.
newtype RecConField =
  RecConField (HsFieldBind (LFieldOcc GhcPs) (LPat GhcPs))
#else
-- | A record field in a pattern match.
newtype RecConField =
  RecConField (HsRecField' (FieldOcc GhcPs) (LPat GhcPs))
#endif
-- | A wrapper for `HsSigType`.
data HsSigType' =
  HsSigType'
    { hsSigTypeFor :: HsTypeFor -- ^ In which context a `HsSigType` is located in.
    , hsSigTypeDir :: HsTypeDir -- ^ How a `HsSigType` should be printed;
                                -- either horizontally or vertically.
    , hsSigType    :: HsSigType GhcPs -- ^ The actual signature.
    }

-- | `HsSigType'` for instance declarations.
pattern HsSigTypeInsideInstDecl :: HsSigType GhcPs -> HsSigType'

pattern HsSigTypeInsideInstDecl x = HsSigType' HsTypeForInstDecl HsTypeNoDir x

-- | `HsSigType'` for function declarations; printed horizontally.
pattern HsSigTypeInsideVerticalFuncSig :: HsSigType GhcPs -> HsSigType'

pattern HsSigTypeInsideVerticalFuncSig x = HsSigType' HsTypeForFuncSig HsTypeVertical x

-- | `HsSigType'` for a top-level function signature.
pattern HsSigTypeInsideDeclSig :: HsSigType GhcPs -> HsSigType'

pattern HsSigTypeInsideDeclSig x = HsSigType' HsTypeForDeclSig HsTypeNoDir x

-- | A wrapper for `HsType`.
data HsType' =
  HsType'
    { hsTypeFor :: HsTypeFor -- ^ In which context a `HsType` is located in.
    , hsTypeDir :: HsTypeDir -- ^ How a function signature is printed;
                             -- either horizontally or vertically.
    , hsType    :: HsType GhcPs -- ^ The actual type.
    }

-- | `HsType'` inside a function signature declaration; printed horizontally.
pattern HsTypeInsideVerticalFuncSig :: HsType GhcPs -> HsType'

pattern HsTypeInsideVerticalFuncSig x = HsType' HsTypeForFuncSig HsTypeVertical x

-- | `HsType'` inside a top-level function signature declaration.
pattern HsTypeInsideDeclSig :: HsType GhcPs -> HsType'

pattern HsTypeInsideDeclSig x = HsType' HsTypeForDeclSig HsTypeNoDir x

-- | `HsType'` inside a instance signature declaration.
pattern HsTypeInsideInstDecl :: HsType GhcPs -> HsType'

pattern HsTypeInsideInstDecl x = HsType' HsTypeForInstDecl HsTypeNoDir x

-- | `StmtLR` inside a vertically printed list.
newtype StmtLRInsideVerticalList =
  StmtLRInsideVerticalList (StmtLR GhcPs GhcPs (LHsExpr GhcPs))

-- | `ParStmtBlock` inside a vertically printed list.
newtype ParStmtBlockInsideVerticalList =
  ParStmtBlockInsideVerticalList (ParStmtBlock GhcPs GhcPs)

-- | A top-level function signature.
newtype DeclSig =
  DeclSig (Sig GhcPs)

-- | A top-level type family instance declaration.
newtype TopLevelTyFamInstDecl =
  TopLevelTyFamInstDecl (TyFamInstDecl GhcPs)
#if MIN_VERSION_ghc_lib_parser(9,4,1)
-- | A wrapper type for type class constraints; e.g., (Eq a, Ord a) of (Eq
-- a, Ord a) => [a] -> [a]. Either 'HorizontalContext' or 'VerticalContext'
-- is used internally.
newtype Context =
  Context (LHsContext GhcPs)

-- | A wrapper type for printing a context horizontally.
newtype HorizontalContext =
  HorizontalContext (LHsContext GhcPs)

-- | A wrapper type for printing a context vertically.
newtype VerticalContext =
  VerticalContext (LHsContext GhcPs)
#else
-- | A wrapper type for type class constraints; e.g., (Eq a, Ord a) of (Eq
-- a, Ord a) => [a] -> [a]. Either 'HorizontalContext' or 'VerticalContext'
-- is used internally.
newtype Context =
  Context (Maybe (LHsContext GhcPs))

-- | A wrapper type for printing a context horizontally.
newtype HorizontalContext =
  HorizontalContext (Maybe (LHsContext GhcPs))

-- | A wrapper type for printing a context vertically.
newtype VerticalContext =
  VerticalContext (Maybe (LHsContext GhcPs))
#endif
-- | A wrapper type for pretty-printing a value of @ModuleName@ with the
-- @module @ prefix.
--
-- Pretty-printing it via @(string "module " >> pretty (name ::
-- ModuleName))@ locates comments before @name@ in the same line as @module
-- @ and the name will be in the next line. This type is to avoid the
-- problem.
newtype ModuleNameWithPrefix =
  ModuleNameWithPrefix ModuleName

-- | A wrapper for 'LPat' inside a pattern declaration. Here, all infix
-- patterns have extra spaces around the operators, like x : xs.
newtype PatInsidePatDecl =
  PatInsidePatDecl (Pat GhcPs)

-- | Lambda case.
data LambdaCase =
  LambdaCase
    { lamCaseGroup :: MatchGroup GhcPs (LHsExpr GhcPs)
    , caseOrCases  :: CaseOrCases
    }
#if MIN_VERSION_ghc_lib_parser(9,4,1)
-- | A deprecation pragma for a module.
newtype ModuleDeprecatedPragma =
  ModuleDeprecatedPragma (WarningTxt GhcPs)
#else
-- | A deprecation pragma for a module.
newtype ModuleDeprecatedPragma =
  ModuleDeprecatedPragma WarningTxt
#endif
-- | Use this type to pretty-print a list comprehension.
data ListComprehension =
  ListComprehension
    { listCompLhs :: ExprLStmt GhcPs -- ^ @f x@ of @[f x| x <- xs]@.
    , listCompRhs :: [ExprLStmt GhcPs] -- ^ @x <- xs@ of @[f x| x <- xs]@.
    }

-- | Use this type to pretty-print a do expression.
data DoExpression =
  DoExpression
    { doStmts :: [ExprLStmt GhcPs]
    , doOrMdo :: DoOrMdo
    }

-- | Use this type to pretty-print a @let ... in ...@ expression.
data LetIn =
  LetIn
    { letBinds :: HsLocalBinds GhcPs
    , inExpr   :: LHsExpr GhcPs
    }

-- | Comments belonging to an AST node.
data NodeComments =
  NodeComments
    { commentsBefore     :: [LEpaComment]
    , commentsOnSameLine :: [LEpaComment]
    , commentsAfter      :: [LEpaComment]
    }

-- | Values indicating whether `do` or `mdo` is used.
data DoOrMdo
  = Do
  | Mdo

-- | Values indicating in which context a RHS is located.
data GRHSExprType
  = GRHSExprNormal
  | GRHSExprCase
  | GRHSExprMultiWayIf
  | GRHSExprLambda
  deriving (Eq)

-- | Values indicating in which context a RHS in a proc expression is located.
data GRHSProcType
  = GRHSProcCase
  | GRHSProcLambda

-- | Values indicating in which context a `HsType` is located.
data HsTypeFor
  = HsTypeForNormalDecl
  | HsTypeForInstDecl
  | HsTypeForFuncSig
  | HsTypeForDeclSig

-- | Values indicating how a node should be printed; either horizontally or
-- vertically.
data HsTypeDir
  = HsTypeNoDir
  | HsTypeVertical

-- | Values indicating whether `case` or `cases` is used.
data CaseOrCases
  = Case
  | Cases
