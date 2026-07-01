{-# LANGUAGE CPP #-}

module HIndent.Ast.Type.Bang
  ( Bang
  , mkBang
  ) where

import HIndent.Applicative (whenJust)
import HIndent.Ast.Type.Strictness
import HIndent.Ast.Type.Unpackedness
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data Bang =
  Bang (Maybe Unpackedness) (Maybe Strictness)

instance Pretty Bang where
  pretty (Bang unpack strictness) = do
    whenJust unpack $ \x -> pretty x >> space
    whenJust strictness pretty
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkBang :: GHC.HsSrcBang -> Bang
mkBang (GHC.HsSrcBang _ unpack strictness) =
  Bang (mkUnpackedness unpack) (mkStrictness strictness)
#elif MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkBang :: GHC.HsBang -> Bang
mkBang (GHC.HsBang unpack strictness) =
  Bang (mkUnpackedness unpack) (mkStrictness strictness)
#else
mkBang :: GHC.HsSrcBang -> Bang
mkBang (GHC.HsSrcBang _ unpack strictness) =
  Bang (mkUnpackedness unpack) (mkStrictness strictness)
#endif
