-- | Printer combinators related to operators.
module HIndent.Pretty.Combinators.Op
  ( unlessSpecialOp
  ) where

import           Control.Monad
import           GHC.Types.Name
import           GHC.Types.Name.Reader
import           HIndent.Types

-- | Runs the printer unless HIndent needs to treat the operator specially.
unlessSpecialOp :: RdrName -> Printer () -> Printer ()
unlessSpecialOp name = unless (isSpecialOp name)

-- | Returns if HIndent needs special treatment for the operator.
isSpecialOp :: RdrName -> Bool
isSpecialOp (Unqual name) = isSpecialOpString $ occNameString name
isSpecialOp Qual {}       = False
isSpecialOp Orig {}       = error "This node is never used in the parsed stage."
isSpecialOp (Exact name)  = isSpecialOpString $ occNameString $ nameOccName name

-- | Returns if HIndent needs special treatment for the operator.
isSpecialOpString :: String -> Bool
isSpecialOpString name = name `elem` ["()", "[]", "->", ":"]
