{-# LANGUAGE CPP #-}
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
  , DataFamInstDecl'(..)
  , pattern DataFamInstDeclTopLevel
  , pattern DataFamInstDeclInsideClassInst
  , FamEqn'(..)
  , pattern FamEqnTopLevel
  , pattern FamEqnInsideClassInst
  , StmtLRInsideVerticalList(..)
  , ParStmtBlockInsideVerticalList(..)
  , TopLevelTyFamInstDecl(..)
  , Context(..)
  , HorizontalContext(..)
  , VerticalContext(..)
  , LambdaCase(..)
  , ListComprehension(..)
  , DoExpression(..)
  , DoOrMdo(..)
  , QualifiedDo(..)
  , LetIn(..)
  , GRHSExprType(..)
  , GRHSProcType(..)
  , HsTypeFor(..)
  , HsTypeDir(..)
  , CaseOrCases(..)
  , DataFamInstDeclFor(..)
  ) where

import Data.List.NonEmpty
import GHC.Hs
import GHC.Types.Name.Reader
import {-# SOURCE #-} qualified HIndent.Ast.Module.Name as HIndent
#if !MIN_VERSION_ghc_lib_parser(9,6,1)
import GHC.Unit
#endif
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
data InfixApp = InfixApp
  { lhs :: LHsExpr GhcPs
  , op :: LHsExpr GhcPs
  , rhs :: LHsExpr GhcPs
  }

-- | `GRHSs` with a label indicating in which context the RHS is located
-- in.
data GRHSsExpr = GRHSsExpr
  { grhssExprType :: GRHSExprType
  , grhssExpr :: GRHSs GhcPs (LHsExpr GhcPs)
  }

-- | 'GRHS' for a normal binding.
data GRHSExpr = GRHSExpr
  { grhsExprType :: GRHSExprType
  , grhsExpr :: GRHS GhcPs (LHsExpr GhcPs)
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
data HsSigType' = HsSigType'
  { hsSigTypeFor :: HsTypeFor -- ^ In which context a `HsSigType` is located in.
  , hsSigTypeDir :: HsTypeDir -- ^ How a `HsSigType` should be printed;
                                -- either horizontally or vertically.
  , hsSigType :: HsSigType GhcPs -- ^ The actual signature.
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

-- | A wrapper of `DataFamInstDecl`.
data DataFamInstDecl' = DataFamInstDecl'
  { dataFamInstDeclFor :: DataFamInstDeclFor -- ^ Where a data family instance is declared.
  , dataFamInstDecl :: DataFamInstDecl GhcPs -- ^ The actual value.
  }

-- | `DataFamInstDecl'` wrapping a `DataFamInstDecl` representing
-- a top-level data family instance.
pattern DataFamInstDeclTopLevel :: DataFamInstDecl GhcPs -> DataFamInstDecl'
pattern DataFamInstDeclTopLevel x = DataFamInstDecl' DataFamInstDeclForTopLevel x

-- | `DataFamInstDecl'` wrapping a `DataFamInstDecl` representing a data
-- family instance inside a class instance.
pattern DataFamInstDeclInsideClassInst :: DataFamInstDecl GhcPs -> DataFamInstDecl'
pattern DataFamInstDeclInsideClassInst x = DataFamInstDecl' DataFamInstDeclForInsideClassInst x

-- | A wrapper for `FamEqn`.
data FamEqn' = FamEqn'
  { famEqnFor :: DataFamInstDeclFor -- ^ Where a data family instance is declared.
  , famEqn :: FamEqn GhcPs (HsDataDefn GhcPs)
  }

-- | `FamEqn'` wrapping a `FamEqn` representing a top-level data family
-- instance.
pattern FamEqnTopLevel :: FamEqn GhcPs (HsDataDefn GhcPs) -> FamEqn'
pattern FamEqnTopLevel x = FamEqn' DataFamInstDeclForTopLevel x

-- | `FamEqn'` wrapping a `FamEqn` representing a data family instance
-- inside a class instance.
pattern FamEqnInsideClassInst :: FamEqn GhcPs (HsDataDefn GhcPs) -> FamEqn'
pattern FamEqnInsideClassInst x = FamEqn' DataFamInstDeclForInsideClassInst x

-- | `StmtLR` inside a vertically printed list.
newtype StmtLRInsideVerticalList =
  StmtLRInsideVerticalList (StmtLR GhcPs GhcPs (LHsExpr GhcPs))

-- | `ParStmtBlock` inside a vertically printed list.
newtype ParStmtBlockInsideVerticalList =
  ParStmtBlockInsideVerticalList (ParStmtBlock GhcPs GhcPs)

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
-- | Lambda case.
data LambdaCase = LambdaCase
  { lamCaseGroup :: MatchGroup GhcPs (LHsExpr GhcPs)
  , caseOrCases :: CaseOrCases
  }

-- | Use this type to pretty-print a list comprehension.
data ListComprehension = ListComprehension
  { listCompLhs :: ExprLStmt GhcPs -- ^ @f x@ of @[f x| x <- xs]@.
  , listCompRhs :: NonEmpty (ExprLStmt GhcPs) -- ^ @x <- xs@ of @[f x| x <- xs]@.
  }

-- | Use this type to pretty-print a do expression.
data DoExpression = DoExpression
  { doStmts :: [ExprLStmt GhcPs]
  , qualifiedDo :: QualifiedDo
  }

-- | Use this type to pretty-print a @let ... in ...@ expression.
data LetIn = LetIn
  { letBinds :: HsLocalBinds GhcPs
  , inExpr :: LHsExpr GhcPs
  }

-- | Values indicating whether `do` or `mdo` is used.
data DoOrMdo
  = Do
  | Mdo

-- | Values indicating whether the `do` is qualified with a module name (and
-- whether `do` or `mdo` is used)
data QualifiedDo =
  QualifiedDo (Maybe HIndent.ModuleName) DoOrMdo

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
  | HsTypeForVerticalAppTy

-- | Values indicating how a node should be printed; either horizontally or
-- vertically.
data HsTypeDir
  = HsTypeNoDir
  | HsTypeVertical

-- | Values indicating whether `case` or `cases` is used.
data CaseOrCases
  = Case
  | Cases

-- | Values indicating where a data family instance is declared.
data DataFamInstDeclFor
  = DataFamInstDeclForTopLevel
  | DataFamInstDeclForInsideClassInst
