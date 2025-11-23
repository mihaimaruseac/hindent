module HIndent.Ast.Declaration.Class.AssociatedThings
  ( ClassAssociatedThings
  , mkAssociatedThings
  , nullAssociatedThings
  , mkClassAssociatedThings
  ) where

import HIndent.Ast.Declaration.Class.AssociatedThing
  ( ClassAssociatedThings(..)
  , mkClassAssociatedThings
  )
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC

mkAssociatedThings ::
     [GHC.LSig GHC.GhcPs]
  -> [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
  -> [GHC.LFamilyDecl GHC.GhcPs]
  -> [GHC.LTyFamInstDecl GHC.GhcPs]
  -> ClassAssociatedThings
mkAssociatedThings = mkClassAssociatedThings

nullAssociatedThings :: ClassAssociatedThings -> Bool
nullAssociatedThings (ClassAssociatedThings items) = null items
