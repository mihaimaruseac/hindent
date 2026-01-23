module HIndent.Ast.Declaration.Instance.Class.Members
  ( ClassInstanceMembers
  , mkClassInstanceMembers
  , classInstanceMembers
  ) where

import HIndent.Ast.Declaration.Instance.Class.Member
  ( ClassInstanceMember
  , mkClassInstanceMember
  )
import HIndent.Ast.GhcOrdered.InstanceMember
  ( InstanceMember
  , mkSortedInstanceMembers
  )
import HIndent.Ast.WithComments (WithComments, fromGenLocated)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC

newtype ClassInstanceMembers =
  ClassInstanceMembers [WithComments ClassInstanceMember]

mkClassInstanceMembers ::
     [GHC.LSig GHC.GhcPs]
  -> [GHC.LHsBindLR GHC.GhcPs GHC.GhcPs]
  -> [GHC.LTyFamInstDecl GHC.GhcPs]
  -> [GHC.LDataFamInstDecl GHC.GhcPs]
  -> ClassInstanceMembers
mkClassInstanceMembers sigs binds tyInsts dataInsts =
  ClassInstanceMembers
    $ fmap
        toClassInstanceMember
        (mkSortedInstanceMembers sigs binds tyInsts dataInsts)

classInstanceMembers ::
     ClassInstanceMembers -> [WithComments ClassInstanceMember]
classInstanceMembers (ClassInstanceMembers items) = items

toClassInstanceMember ::
     GHC.LocatedA InstanceMember -> WithComments ClassInstanceMember
toClassInstanceMember item = mkClassInstanceMember <$> fromGenLocated item
