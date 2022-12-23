-- | Types related to language extensions
module HIndent.LanguageExtension.Types
  ( Extension(..)
  ) where

import qualified GHC.LanguageExtensions as GLP

-- | Language Extension. Either enabled or disabled.
--
-- The `Cabal` package also has an `Extension` type that can be used to
-- indicate whether an extension is enabled or disabled, but Cabal's one
-- should be avoided as much as possible. The `KnownExtension` of `Cabal`
-- may not have the latest extensions, and if such extensions are used,
-- there will be cases where GHC can build, but HIndent cannot format.
data Extension
  = EnableExtension GLP.Extension
  | DisableExtension GLP.Extension
  deriving (Eq)
