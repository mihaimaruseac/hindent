-- | This module defines the AST for Haskell code.
--
-- GHC provides its AST for Haskell code, but the structure it offers may change
-- with version updates. In other words, when directly using GHC's AST as the
-- AST for pretty-printing, updates in GHC require direct modifications to the
-- pretty-printing functions. On the other hand, when there is a need to change
-- the pretty-printing style, corresponding modifications to the functions are
-- also necessary. The presence of these two reasons for modification leads to a
-- suboptimal design state.
--
-- Therefore, this module defines a custom AST for HIndent, allowing flexibility
-- to adapt to changes in GHC's AST across different versions.
--
module HIndent.Ast
  ( mkModule
  ) where

import HIndent.Ast.Module
