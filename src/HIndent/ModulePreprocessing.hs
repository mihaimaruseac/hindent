{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Module preprocessing before pretty-printing.
module HIndent.ModulePreprocessing
  ( modifyASTForPrettyPrinting
  ) where

import Data.Function
import Data.List
import GHC.Hs
import GHC.Types.SrcLoc
import Generics.SYB hiding (GT, typeOf, typeRep)
import HIndent.Fixity
import HIndent.GhcLibParserWrapper.GHC.Hs
import HIndent.ModulePreprocessing.CommentRelocation
import Language.Haskell.GhclibParserEx.Fixity
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
import qualified GHC.Data.Strict as Strict
import HIndent.GhcLibParserWrapper.GHC.Parser.Annotation
#else
import Control.Applicative
import Data.Maybe
import Type.Reflection
#endif
-- | This function modifies the given module AST for pretty-printing.
--
-- Pretty-printing a module without calling this function for it before may
-- raise an error or not print it correctly.
modifyASTForPrettyPrinting :: HsModule' -> HsModule'
modifyASTForPrettyPrinting m = relocateComments (beforeRelocation m) allComments
  where
    beforeRelocation =
      resetListCompRange
        . resetLGRHSEndPositionInModule
        . removeAllDocDs
        . closeEpAnnOfHsFunTy
        . closeEpAnnOfMatchMExt
        . closePlaceHolderEpAnns
        . closeEpAnnOfFunBindFunId
        . resetModuleNameColumn
        . replaceAllNotUsedAnns
        . removeComments
        . sortExprLStmt
        . fixFixities
    allComments = listify (not . isEofComment . ac_tok . unLoc) m

-- | This function modifies the given module AST to apply fixities of infix
-- operators defined in the 'base' package.
fixFixities :: HsModule' -> HsModule'
fixFixities = applyFixities fixities

-- | This function modifies the range of `HsDo` with `ListComp` so that it
-- includes the whole list comprehension.
--
-- This function is necessary for `ghc-lib-parser>=9.10.1<9.12.1` because `HsDo`
-- no longer includes brackets of list comprehensions in its range.
resetListCompRange :: HsModule' -> HsModule'
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
resetListCompRange = everywhere (mkT resetListCompRange')
  where
    resetListCompRange' :: HsExpr GhcPs -> HsExpr GhcPs
    resetListCompRange' (HsDo al@AnnList {al_brackets = ListSquare (EpTok (EpaSpan (RealSrcSpan open _))) (EpTok (EpaSpan (RealSrcSpan close _)))} ListComp (L EpAnn {..} xs)) =
      HsDo
        al
        ListComp
        (L EpAnn
             { entry =
                 EpaSpan
                   $ RealSrcSpan
                       (mkRealSrcSpan
                          (realSrcSpanStart open)
                          (realSrcSpanEnd close))
                       Strict.Nothing
             , ..
             }
           xs)
    resetListCompRange' x = x
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
resetListCompRange = everywhere (mkT resetListCompRange')
  where
    resetListCompRange' :: HsExpr GhcPs -> HsExpr GhcPs
    resetListCompRange' (HsDo al@AnnList { al_open = Just (AddEpAnn _ (EpaSpan (RealSrcSpan open _)))
                                         , al_close = Just (AddEpAnn _ (EpaSpan (RealSrcSpan close _)))
                                         } ListComp (L EpAnn {..} xs)) =
      HsDo
        al
        ListComp
        (L EpAnn
             { entry =
                 EpaSpan
                   $ RealSrcSpan
                       (mkRealSrcSpan
                          (realSrcSpanStart open)
                          (realSrcSpanEnd close))
                       Strict.Nothing
             , ..
             }
           xs)
    resetListCompRange' x = x
#else
resetListCompRange = id
#endif
-- | This function sets an 'LGRHS's end position to the end position of the
-- last RHS in the 'grhssGRHSs'.
--
-- The source span of an 'L?GRHS' contains the 'where' keyword, which
-- locates comments in the wrong position in the process of comment
-- relocation. This function prevents it by fixing the 'L?GRHS''s source
-- span.
resetLGRHSEndPositionInModule :: HsModule' -> HsModule'
resetLGRHSEndPositionInModule = everywhere (mkT resetLGRHSEndPosition)

-- | This function sorts lists of statements in order their positions.
--
-- For example, the last element of 'HsDo' of 'HsExpr' is the element
-- before a bar, and the elements are not sorted by their locations. This
-- function fixes the orderings.
sortExprLStmt :: HsModule' -> HsModule'
sortExprLStmt m@HsModule {hsmodDecls = xs} = m {hsmodDecls = sorted}
  where
    sorted = everywhere (mkT sortByLoc) xs
    sortByLoc :: [ExprLStmt GhcPs] -> [ExprLStmt GhcPs]
    sortByLoc = sortBy (compare `on` srcSpanToRealSrcSpan . locA . getLoc)

-- | This function removes all comments from the given module not to
-- duplicate them on comment relocation.
removeComments :: HsModule' -> HsModule'
removeComments = everywhere (mkT $ const emptyComments)

-- | This function replaces all 'EpAnnNotUsed's in 'SrcSpanAnn''s with
-- 'EpAnn's to make it possible to locate comments on them.
replaceAllNotUsedAnns :: HsModule' -> HsModule'
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
-- 'EpAnnNotUsed' is not used since 9.10.1.
replaceAllNotUsedAnns = id
#else
replaceAllNotUsedAnns = everywhere app
  where
    app ::
         forall a. Data a
      => (a -> a)
    app sp
      | App g (App y z) <- typeRep @a
      , Just HRefl <- eqTypeRep g (typeRep @SrcSpanAnn')
      , Just HRefl <- eqTypeRep y (typeRep @EpAnn) =
        fromMaybe sp $ do
          let try :: Typeable b => b -> Maybe a
              try ann = do
                HRefl <- eqTypeRep (typeOf ann) z
                pure sp {ann = EpAnn (spanAsAnchor $ locA sp) ann emptyComments}
          try emptyListItem
            <|> try emptyList
            <|> try emptyPragma
            <|> try emptyContext
            <|> try emptyNameAnn
            <|> try NoEpAnns
    app x = x
    emptyListItem = AnnListItem []
    emptyList = AnnList Nothing Nothing Nothing [] []
    emptyPragma = AnnPragma emptyAddEpAnn emptyAddEpAnn []
    emptyContext = AnnContext Nothing [] []
    emptyNameAnn = NameAnnTrailing []
    emptyAddEpAnn = AddEpAnn AnnAnyclass emptyEpaLocation
    emptyEpaLocation = EpaDelta (SameLine 0) []
#endif
-- | This function sets the start column of 'hsmodName' of the given
-- 'HsModule' to 1 to correctly locate comments above the module name.
resetModuleNameColumn :: HsModule' -> HsModule'
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
resetModuleNameColumn m@HsModule {hsmodName = Just (L epa@EpAnn {..} name)} =
  m {hsmodName = Just (L newAnn name)}
  where
    newAnn = epa {entry = realSpanAsAnchor newSpan}
    newSpan =
      mkRealSrcSpan
        (mkRealSrcLoc (srcSpanFile anc) (srcSpanStartLine anc) 1)
        (realSrcSpanEnd anc)
    anc =
      case entry of
        EpaSpan (RealSrcSpan a _) -> a
        _ -> error "resetModuleNameColumn: not a RealSrcSpan"
#else
resetModuleNameColumn m@HsModule {hsmodName = Just (L (SrcSpanAnn epa@EpAnn {..} sp) name)} =
  m {hsmodName = Just (L (SrcSpanAnn newAnn sp) name)}
  where
    newAnn = epa {entry = realSpanAsAnchor newSpan}
    newSpan =
      mkRealSrcSpan
        (mkRealSrcLoc (srcSpanFile anc) (srcSpanStartLine anc) 1)
        (realSrcSpanEnd anc)
    anc = anchor entry
#endif
resetModuleNameColumn m = m

-- | This function replaces the 'EpAnn' of 'fun_id' in 'FunBind' with
-- 'EpAnnNotUsed'.
--
-- The 'fun_id' contains the function's name. However, 'FunRhs' of 'Match'
-- also contains the name, and we use the latter one. This function
-- prevents comments from being located in 'fun_id'.
closeEpAnnOfFunBindFunId :: HsModule' -> HsModule'
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
-- TODO: 'EpAnnNotUsed' is not used since 9.10.1. We need to find another
-- way to close 'EpAnn's.
closeEpAnnOfFunBindFunId = id
#else
closeEpAnnOfFunBindFunId = everywhere (mkT closeEpAnn)
  where
    closeEpAnn :: HsBind GhcPs -> HsBind GhcPs
    closeEpAnn bind@FunBind {fun_id = (L (SrcSpanAnn _ l) name)} =
      bind {fun_id = L (SrcSpanAnn EpAnnNotUsed l) name}
    closeEpAnn x = x
#endif
-- | This function replaces the 'EpAnn' of 'm_ext' in 'Match' with
-- 'EpAnnNotUsed.
--
-- The field contains the annotation of the match LHS. However, the same
-- information is also stored inside the 'Match'. This function removes the
-- duplication not to locate comments on a wrong point.
closeEpAnnOfMatchMExt :: HsModule' -> HsModule'
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
-- TODO: 'EpAnnNotUsed' is not used since 9.10.1. We need to find another
-- way to close 'EpAnn's.
closeEpAnnOfMatchMExt = id
#else
closeEpAnnOfMatchMExt = everywhere closeEpAnn
  where
    closeEpAnn ::
         forall a. Typeable a
      => a
      -> a
    closeEpAnn x
      | App (App g h) _ <- typeRep @a
      , Just HRefl <- eqTypeRep g (typeRep @Match)
      , Just HRefl <- eqTypeRep h (typeRep @GhcPs) = x {m_ext = EpAnnNotUsed}
      | otherwise = x
#endif
-- | This function replaces the 'EpAnn' of the first argument of 'HsFunTy'
-- of 'HsType'.
--
-- 'HsFunTy' should not have any comments. Instead, its LHS and RHS should
-- have them.
closeEpAnnOfHsFunTy :: HsModule' -> HsModule'
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
-- TODO: 'EpAnnNotUsed' is not used since 9.10.1. We need to find another
-- way to close 'EpAnn's.
closeEpAnnOfHsFunTy = id
#else
closeEpAnnOfHsFunTy = everywhere (mkT closeEpAnn)
  where
    closeEpAnn :: HsType GhcPs -> HsType GhcPs
    closeEpAnn (HsFunTy _ p l r) = HsFunTy EpAnnNotUsed p l r
    closeEpAnn x = x
#endif
-- | This function replaces all 'EpAnn's that contain placeholder anchors
-- to locate comments correctly. A placeholder anchor is an anchor pointing
-- on (-1, -1).
closePlaceHolderEpAnns :: HsModule' -> HsModule'
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
-- TODO: 'EpAnnNotUsed' is not used since 9.10.1. We need to find another
-- way to close 'EpAnn's.
closePlaceHolderEpAnns = id
#else
closePlaceHolderEpAnns = everywhere closeEpAnn
  where
    closeEpAnn ::
         forall a. Typeable a
      => a
      -> a
    closeEpAnn x
      | App g _ <- typeRep @a
      , Just HRefl <- eqTypeRep g (typeRep @EpAnn)
      , (EpAnn (Anchor sp _) _ _) <- x
      , srcSpanEndLine sp == -1 && srcSpanEndCol sp == -1 = EpAnnNotUsed
      | otherwise = x
#endif
-- | This function removes all 'DocD's from the given module. They have
-- haddocks, but the same information is stored in 'EpaCommentTok's. Thus,
-- we need to remove the duplication.
removeAllDocDs :: HsModule' -> HsModule'
removeAllDocDs x@HsModule {hsmodDecls = decls} =
  x {hsmodDecls = filter (not . isDocD . unLoc) decls}
  where
    isDocD DocD {} = True
    isDocD _ = False

-- | This function sets the position of the given 'LGRHS' to the end
-- position of the last RHS in it.
--
-- See the documentation of 'resetLGRHSEndPositionInModule' for the reason.
resetLGRHSEndPosition ::
     LGRHS GhcPs (LHsExpr GhcPs) -> LGRHS GhcPs (LHsExpr GhcPs)
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
resetLGRHSEndPosition (L locAnn (GRHS ext@EpAnn {..} stmt body)) =
  let lastPosition =
        maximum
          $ realSrcSpanEnd . epaLocationToRealSrcSpan
              <$> listify collectEpaLocation' body
      newSpan =
        mkRealSrcSpan
          (realSrcSpanStart $ epaLocationToRealSrcSpan entry)
          lastPosition
      newLocAnn = locAnn {entry = realSpanAsAnchor newSpan}
      newAnn = ext {entry = realSpanAsAnchor newSpan}
   in L newLocAnn (GRHS newAnn stmt body)
  where
    collectEpaLocation' :: EpaLocation -> Bool
    collectEpaLocation' (EpaSpan RealSrcSpan {}) = True
    collectEpaLocation' _ = False
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
resetLGRHSEndPosition (L (SrcSpanAnn locAnn@EpAnn {} sp) (GRHS ext@EpAnn {..} stmt body)) =
  let lastPosition =
        maximum $ realSrcSpanEnd . anchor <$> listify collectAnchor body
      newSpan = mkRealSrcSpan (realSrcSpanStart $ anchor entry) lastPosition
      newLocAnn = locAnn {entry = realSpanAsAnchor newSpan}
      newAnn = ext {entry = realSpanAsAnchor newSpan}
   in L (SrcSpanAnn newLocAnn sp) (GRHS newAnn stmt body)
  where
    collectAnchor :: Anchor -> Bool
    collectAnchor _ = True
resetLGRHSEndPosition x = x
#else
resetLGRHSEndPosition (L _ (GRHS ext@EpAnn {..} stmt body)) =
  let lastPosition =
        maximum $ realSrcSpanEnd . anchor <$> listify collectAnchor body
      newSpan = mkRealSrcSpan (realSrcSpanStart $ anchor entry) lastPosition
      newLoc = RealSrcSpan newSpan Nothing
      newAnn = ext {entry = realSpanAsAnchor newSpan}
   in L newLoc (GRHS newAnn stmt body)
  where
    collectAnchor :: Anchor -> Bool
    collectAnchor _ = True
resetLGRHSEndPosition x = x
#endif
isEofComment :: EpaCommentTok -> Bool
#if !MIN_VERSION_ghc_lib_parser(9, 10, 1)
isEofComment EpaEofComment = True
#endif
isEofComment _ = False
