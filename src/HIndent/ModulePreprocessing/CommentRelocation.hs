{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs,CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Comment relocation for pretty-printing comments correctly.
--
-- HIndent gathers all comments above a function, an import, a module
-- declaration, etc. For example, HIndent formats the following code
--
-- > f :: Int
-- > f = 1
-- >
-- > -- A comment between f and g
-- >
-- > -- Another comment between f and g
-- >
-- > g :: Int
-- > g = 2
--
-- to
--
-- > f :: Int
-- > f = 1
-- >
-- > -- A comment between f and g
-- > -- Another comment between f and g
-- > g :: Int
-- > g = 2
--
-- AST nodes must have the information of which comments are above, on the
-- same line, and below. However, AST nodes generated by a parser of
-- 'ghc-lib-parser' only contain comments after them. 'relocateComments' is
-- defined to solve the problem.
module HIndent.ModulePreprocessing.CommentRelocation
  ( relocateComments
  ) where

import Control.Exception
import Control.Monad.State
import Data.Foldable
import Data.Function
import Data.List
import GHC.Data.Bag
import GHC.Hs
import GHC.Types.SrcLoc
import Generics.SYB hiding (GT, typeOf, typeRep)
import HIndent.Pretty.Pragma
import HIndent.Pretty.SigBindFamily
import Type.Reflection
#if MIN_VERSION_GLASGOW_HASKELL(9,6,0,0)
import Control.Monad
#endif

-- | A wrapper type used in everywhereMEpAnnsBackwards' to collect all
-- 'EpAnn's to apply a function with them in order their positions.
data Wrapper =
  forall a. Typeable (EpAnn a) =>
            Wrapper (EpAnn a)

-- | 'State' with comments.
type WithComments = State [LEpaComment]

-- | This function collects all comments from the passed 'HsModule', and
-- modifies all 'EpAnn's so that all 'EpAnn's have 'EpaCommentsBalanced's.
#if MIN_VERSION_ghc_lib_parser(9,6,1)
relocateComments :: HsModule GhcPs -> [LEpaComment] -> HsModule GhcPs
#else
relocateComments :: HsModule -> [LEpaComment] -> HsModule
#endif
relocateComments = evalState . relocate
  where
    relocate =
      relocatePragmas >=>
      relocateCommentsBeforePragmas >=>
      relocateCommentsInExportList >=>
      relocateCommentsBeforeTopLevelDecls >=>
      relocateCommentsSameLine >=>
      relocateCommentsTopLevelWhereClause >=>
      relocateCommentsAfter >=> assertAllCommentsAreConsumed
    assertAllCommentsAreConsumed x = do
      cs <- get
      assert (null cs) (pure x)

-- | This function locates pragmas to the module's EPA.
#if MIN_VERSION_ghc_lib_parser(9,6,1)
relocatePragmas :: HsModule GhcPs -> WithComments (HsModule  GhcPs)
relocatePragmas m@HsModule {hsmodExt=xmod@XModulePs{hsmodAnn = epa@EpAnn {}}} = do
  newAnn <- insertComments (isPragma . ac_tok . unLoc) insertPriorComments epa
  return m {hsmodExt = xmod{hsmodAnn=newAnn}}
#else
relocatePragmas :: HsModule -> WithComments HsModule
relocatePragmas m@HsModule {hsmodAnn = epa@EpAnn {}} = do
  newAnn <- insertComments (isPragma . ac_tok . unLoc) insertPriorComments epa
  return m {hsmodAnn = newAnn}
#endif
relocatePragmas m = pure m

-- | This function locates comments that are located before pragmas to the
-- module's EPA.
#if MIN_VERSION_ghc_lib_parser(9,6,1)
relocateCommentsBeforePragmas :: HsModule GhcPs -> WithComments (HsModule GhcPs)
relocateCommentsBeforePragmas m@HsModule {hsmodExt=xmod@XModulePs{hsmodAnn = ann}}
  | pragmaExists m = do
    newAnn <- insertCommentsByPos (< startPosOfPragmas) insertPriorComments ann
    pure m {hsmodExt=xmod{hsmodAnn = newAnn}}
  | otherwise = pure m
  where
    startPosOfPragmas = anchor $ getLoc $ head $ priorComments $ comments ann
#else 
relocateCommentsBeforePragmas :: HsModule -> WithComments HsModule
relocateCommentsBeforePragmas m@HsModule {hsmodAnn = ann}
  | pragmaExists m = do
    newAnn <- insertCommentsByPos (< startPosOfPragmas) insertPriorComments ann
    pure m {hsmodAnn = newAnn}
  | otherwise = pure m
  where
    startPosOfPragmas = anchor $ getLoc $ head $ priorComments $ comments ann
#endif

-- | This function locates comments that are located before each element of
-- an export list.
#if MIN_VERSION_ghc_lib_parser(9,6,1)
relocateCommentsInExportList :: HsModule GhcPs -> WithComments (HsModule GhcPs)
#else
relocateCommentsInExportList :: HsModule -> WithComments HsModule
#endif
relocateCommentsInExportList m@HsModule {hsmodExports = Just (L listSp@SrcSpanAnn {ann = EpAnn {entry = listAnn}} xs)} = do
  newExports <- mapM insertCommentsBeforeElement xs
  pure m {hsmodExports = Just (L listSp newExports)}
  where
    insertCommentsBeforeElement (L sp@SrcSpanAnn {ann = entryAnn@EpAnn {}} x) = do
      newEpa <-
        insertCommentsByPos
          (isBefore $ anchor $ entry entryAnn)
          insertPriorComments
          entryAnn
      pure $ L sp {ann = newEpa} x
    insertCommentsBeforeElement x = pure x
    isBefore anc comAnc =
      srcSpanStartLine comAnc < srcSpanStartLine anc &&
      realSrcSpanStart (anchor listAnn) < realSrcSpanStart comAnc
relocateCommentsInExportList x = pure x

-- | This function locates comments located before top-level declarations.
#if MIN_VERSION_ghc_lib_parser(9,6,1)
relocateCommentsBeforeTopLevelDecls :: HsModule GhcPs -> WithComments (HsModule GhcPs)
#else
relocateCommentsBeforeTopLevelDecls :: HsModule -> WithComments HsModule
#endif
relocateCommentsBeforeTopLevelDecls = everywhereM (applyM f)
  where
    f epa@EpAnn {..} =
      insertCommentsByPos (isBefore $ anchor entry) insertPriorComments epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isBefore anc comAnc =
      srcSpanStartCol anc == 1 &&
      srcSpanStartCol comAnc == 1 &&
      srcSpanStartLine comAnc < srcSpanStartLine anc

-- | This function scans the given AST from bottom to top and locates
-- comments that are on the same line as the node.  Comments are stored in
-- the 'followingComments' of 'EpaCommentsBalanced'.
#if MIN_VERSION_ghc_lib_parser(9,6,1)
relocateCommentsSameLine :: HsModule GhcPs -> WithComments (HsModule GhcPs)
#else
relocateCommentsSameLine :: HsModule -> WithComments HsModule
#endif
relocateCommentsSameLine = everywhereMEpAnnsBackwards f
  where
    f epa@EpAnn {..} =
      insertCommentsByPos
        (isOnSameLine $ anchor entry)
        insertFollowingComments
        epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isOnSameLine anc comAnc =
      srcSpanStartLine comAnc == srcSpanStartLine anc &&
      srcSpanStartLine comAnc == srcSpanEndLine anc

-- | This function locates comments above the top-level declarations in
-- a 'where' clause in the topmost declaration.
#if MIN_VERSION_ghc_lib_parser(9,6,1)
relocateCommentsTopLevelWhereClause :: HsModule GhcPs -> WithComments (HsModule GhcPs)
#else
relocateCommentsTopLevelWhereClause :: HsModule -> WithComments HsModule
#endif
relocateCommentsTopLevelWhereClause m@HsModule {..} = do
  hsmodDecls' <- mapM relocateCommentsDeclWhereClause hsmodDecls
  pure m {hsmodDecls = hsmodDecls'}
  where
    relocateCommentsDeclWhereClause (L l (ValD ext fb@(FunBind {fun_matches = MG {..}}))) = do
      mg_alts' <- mapM (mapM relocateCommentsMatch) mg_alts
      pure $ L l (ValD ext fb {fun_matches = MG {mg_alts = mg_alts', ..}})
    relocateCommentsDeclWhereClause x = pure x
    relocateCommentsMatch (L l match@Match {m_grhss = gs@GRHSs {grhssLocalBinds = (HsValBinds ext (ValBinds ext' binds sigs))}}) = do
      (binds', sigs') <- relocateCommentsBindsSigs binds sigs
      let localBinds = HsValBinds ext (ValBinds ext' binds' sigs')
      pure $ L l match {m_grhss = gs {grhssLocalBinds = localBinds}}
    relocateCommentsMatch x = pure x
    relocateCommentsBindsSigs ::
         LHsBindsLR GhcPs GhcPs
      -> [LSig GhcPs]
      -> WithComments (LHsBindsLR GhcPs GhcPs, [LSig GhcPs])
    relocateCommentsBindsSigs binds sigs = do
      bindsSigs' <- mapM addCommentsBeforeEpAnn bindsSigs
      pure (listToBag $ filterLBind bindsSigs', filterLSig bindsSigs')
      where
        bindsSigs = mkSortedLSigBindFamilyList sigs (bagToList binds) [] [] []
    addCommentsBeforeEpAnn (L (SrcSpanAnn epa@EpAnn {..} sp) x) = do
      cs <- get
      let (notAbove, above) =
            partitionAboveNotAbove (sortCommentsByLocation cs) entry
          epa' = epa {comments = insertPriorComments comments above}
      put notAbove
      pure $ L (SrcSpanAnn epa' sp) x
    addCommentsBeforeEpAnn x = pure x
    partitionAboveNotAbove cs sp =
      fst $
      foldr'
        (\c@(L l _) ((ls, rs), lastSpan) ->
           if anchor l `isAbove` anchor lastSpan
             then ((ls, c : rs), l)
             else ((c : ls, rs), lastSpan))
        (([], []), sp)
        cs
    isAbove comAnc anc =
      srcSpanStartCol comAnc == srcSpanStartCol anc &&
      srcSpanEndLine comAnc + 1 == srcSpanStartLine anc

-- | This function scans the given AST from bottom to top and locates
-- comments in the comment pool after each node on it.
#if MIN_VERSION_ghc_lib_parser(9,6,1)
relocateCommentsAfter :: HsModule GhcPs -> WithComments (HsModule GhcPs)
#else
relocateCommentsAfter :: HsModule -> WithComments HsModule
#endif
relocateCommentsAfter = everywhereMEpAnnsBackwards f
  where
    f epa@EpAnn {..} =
      insertCommentsByPos (isAfter $ anchor entry) insertFollowingComments epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isAfter anc comAnc = srcSpanEndLine anc <= srcSpanStartLine comAnc

-- | This function applies the given function to all 'EpAnn's.
applyM ::
     forall a. Typeable a
  => (forall b. EpAnn b -> WithComments (EpAnn b))
  -> (a -> WithComments a)
applyM f
  | App g _ <- typeRep @a
  , Just HRefl <- eqTypeRep g (typeRep @EpAnn) = f
  | otherwise = pure

-- | This function drains comments whose positions satisfy the given
-- predicate and inserts them to the given node using the given inserter.
insertCommentsByPos ::
     (RealSrcSpan -> Bool)
  -> (EpAnnComments -> [LEpaComment] -> EpAnnComments)
  -> EpAnn a
  -> WithComments (EpAnn a)
insertCommentsByPos cond = insertComments (cond . anchor . getLoc)

-- | This function drains comments that satisfy the given predicate and
-- inserts them to the given node using the given inserter.
insertComments ::
     (LEpaComment -> Bool)
  -> (EpAnnComments -> [LEpaComment] -> EpAnnComments)
  -> EpAnn a
  -> WithComments (EpAnn a)
insertComments cond inserter epa@EpAnn {..} = do
  coms <- drainComments cond
  pure $ epa {comments = inserter comments coms}
insertComments _ _ EpAnnNotUsed = pure EpAnnNotUsed

-- | This function inserts comments to `priorComments`.
insertPriorComments :: EpAnnComments -> [LEpaComment] -> EpAnnComments
insertPriorComments (EpaComments prior) cs =
  EpaComments (sortCommentsByLocation $ prior ++ cs)
insertPriorComments (EpaCommentsBalanced prior following) cs =
  EpaCommentsBalanced (sortCommentsByLocation $ prior ++ cs) following

-- | This function inserts comments to `followingComments`.
insertFollowingComments :: EpAnnComments -> [LEpaComment] -> EpAnnComments
insertFollowingComments (EpaComments prior) cs = EpaCommentsBalanced prior cs
insertFollowingComments (EpaCommentsBalanced prior following) cs =
  EpaCommentsBalanced prior (sortCommentsByLocation $ following ++ cs)

-- | This function drains comments that satisfy the given predicate.
drainComments :: (LEpaComment -> Bool) -> WithComments [LEpaComment]
drainComments cond = do
  coms <- get
  let (xs, others) = partition cond coms
  put others
  return xs

-- | 'everywhereM' but applies the given function to EPAs in order their
-- positions from backwards.
everywhereMEpAnnsBackwards ::
     Data a
  => (forall b. EpAnn b -> WithComments (EpAnn b))
  -> a
  -> WithComments a
everywhereMEpAnnsBackwards =
  everywhereMEpAnnsInOrder (flip compareEpaByEndPosition)

-- | 'everywhereM' but applies the given function to EPAs in order
-- specified by the given ordering function.
everywhereMEpAnnsInOrder ::
     Data a
  => (forall b c. EpAnn b -> EpAnn c -> Ordering)
  -> (forall b. EpAnn b -> WithComments (EpAnn b))
  -> a
  -> WithComments a
everywhereMEpAnnsInOrder cmp f hm =
  collectEpAnnsInOrderEverywhereMTraverses >>=
  applyFunctionInOrderEpAnnEndPositions >>=
  putModifiedEpAnnsToModule
  where
    collectEpAnnsInOrderEverywhereMTraverses
      -- This function uses 'everywhereM' to collect 'EpAnn's because they
      -- should be collected in the same order as 'putModifiedEpAnnsToModule'
      -- puts them to the AST.
     = reverse <$> execStateT (everywhereM collectEpAnnsST hm) []
      where
        collectEpAnnsST x = do
          modify $ collectEpAnns x
          pure x
        collectEpAnns ::
             forall a. Typeable a
          => a
          -> ([Wrapper] -> [Wrapper])
        collectEpAnns x
          -- If 'a' is 'EpAnn b' ('b' can be any type), wrap 'x' with a 'Wrapper'.
          | App g _ <- typeRep @a
          , Just HRefl <- eqTypeRep g (typeRep @EpAnn) = (Wrapper x :)
          | otherwise = id
    applyFunctionInOrderEpAnnEndPositions ::
         [Wrapper]
      -> WithComments [(Int, Wrapper)] -- ^ The first element of the tuple
                                       -- indicates how many 'Wrapper's were there before 'everywhereM'
                                       -- accessed the second element.
    applyFunctionInOrderEpAnnEndPositions anns =
      forM sorted $ \(i, Wrapper x) -> do
        x' <- f x
        pure (i, Wrapper x')
      where
        indexed = zip [0 :: Int ..] anns
        sorted = sortBy (\(_, Wrapper a) (_, Wrapper b) -> cmp a b) indexed
    putModifiedEpAnnsToModule anns = evalStateT (everywhereM setEpAnn hm) [0 ..]
      where
        setEpAnn ::
             forall a. Typeable a
          => a
          -> StateT [Int] WithComments a
        setEpAnn x
          -- This guard arm checks if 'a' is 'EpAnn b' ('b' can be any type).
          | App g g' <- typeRep @a
          , Just HRefl <- eqTypeRep g (typeRep @EpAnn) = do
            i <- gets head
            modify tail
            case lookup i anns of
              Just (Wrapper y)
                | App _ h <- typeOf y
                , Just HRefl <- eqTypeRep g' h -> pure y
              _ -> error "Unmatches"
          | otherwise = pure x

-- | This function sorts comments by its location.
sortCommentsByLocation :: [LEpaComment] -> [LEpaComment]
sortCommentsByLocation = sortBy (compare `on` anchor . getLoc)

-- | This function compares given EPAs by their end positions.
compareEpaByEndPosition :: EpAnn a -> EpAnn b -> Ordering
compareEpaByEndPosition (EpAnn a _ _) (EpAnn b _ _) =
  on compare (realSrcSpanEnd . anchor) a b
compareEpaByEndPosition EpAnnNotUsed EpAnnNotUsed = EQ
compareEpaByEndPosition _ EpAnnNotUsed = GT
compareEpaByEndPosition EpAnnNotUsed _ = LT
