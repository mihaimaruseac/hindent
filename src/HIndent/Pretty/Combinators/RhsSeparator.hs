-- | Separator-related things (e.g., '=' and '->').
module HIndent.Pretty.Combinators.RhsSeparator
  ( rhsSeparator
  ) where

import           HIndent.Pretty.Combinators.String
import           HIndent.Pretty.Types
import           HIndent.Types

-- | Returns a separator between a LHS and a RHS according to the type of
-- the RHS.
rhsSeparator :: GRHSExprType -> Printer ()
rhsSeparator GRHSExprNormal     = string "="
rhsSeparator GRHSExprCase       = string "->"
rhsSeparator GRHSExprMultiWayIf = string "->"
rhsSeparator GRHSExprLambda     = string "->"
