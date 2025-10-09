{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Types to pretty-print certain parts of Haskell codes.
--
-- We define new types to pretty-print AST nodes rather than define
-- functions to print comments easily using the 'Pretty' implementation of
-- 'GenLocated'.
module HIndent.Pretty.Types
  ( DataFamInstDecl'(..)
  , pattern DataFamInstDeclTopLevel
  , pattern DataFamInstDeclInsideClassInst
  , FamEqn'(..)
  , pattern FamEqnTopLevel
  , pattern FamEqnInsideClassInst
  , TopLevelTyFamInstDecl(..)
  , Context(..)
  , HorizontalContext(..)
  , VerticalContext(..)
  , DoOrMdo(..)
  , QualifiedDo(..)
  , DataFamInstDeclFor(..)
  ) where

import GHC.Hs
import {-# SOURCE #-} qualified HIndent.Ast.Module.Name as HIndent
#if !MIN_VERSION_ghc_lib_parser(9,6,1)
import GHC.Unit
#endif
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
-- | Values indicating whether `do` or `mdo` is used.
data DoOrMdo
  = Do
  | Mdo

-- | Values indicating whether the `do` is qualified with a module name (and
-- whether `do` or `mdo` is used)
data QualifiedDo =
  QualifiedDo (Maybe HIndent.ModuleName) DoOrMdo

-- | Values indicating where a data family instance is declared.
data DataFamInstDeclFor
  = DataFamInstDeclForTopLevel
  | DataFamInstDeclForInsideClassInst
