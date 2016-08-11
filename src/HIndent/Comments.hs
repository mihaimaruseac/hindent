{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables #-}

-- | Comment handling.

module HIndent.Comments where

import Control.Arrow (first, second)
import Control.Monad.State.Strict
import Data.Data
import qualified Data.Map.Strict as M
import HIndent.Types
import Language.Haskell.Exts hiding (Style,prettyPrint,Pretty,style,parse)

-- Order by start of span, larger spans before smaller spans.
newtype OrderByStart =
  OrderByStart SrcSpan
  deriving (Eq)

instance Ord OrderByStart where
  compare (OrderByStart l) (OrderByStart r) =
    compare (srcSpanStartLine l)
            (srcSpanStartLine r) `mappend`
    compare (srcSpanStartColumn l)
            (srcSpanStartColumn r) `mappend`
    compare (srcSpanEndLine r)
            (srcSpanEndLine l) `mappend`
    compare (srcSpanEndColumn r)
            (srcSpanEndColumn l)

-- Order by end of span, smaller spans before larger spans.
newtype OrderByEnd =
  OrderByEnd SrcSpan
  deriving (Eq)

instance Ord OrderByEnd where
  compare (OrderByEnd l) (OrderByEnd r) =
    compare (srcSpanEndLine l)
            (srcSpanEndLine r) `mappend`
    compare (srcSpanEndColumn l)
            (srcSpanEndColumn r) `mappend`
    compare (srcSpanStartLine r)
            (srcSpanStartLine l) `mappend`
    compare (srcSpanStartColumn r)
            (srcSpanStartColumn l)

-- | Annotate the AST with comments.
annotateComments :: forall ast. (Data (ast NodeInfo),Traversable ast,Annotated ast,Show (ast NodeInfo))
                 => ast SrcSpanInfo -> [Comment] -> ([ComInfo],ast NodeInfo)
annotateComments src comments =
  evalState (do _ <- traverse assignComment comments
                cis <- gets fst
                ast <- traverse transferComments src
                return (cis,ast))
            ([],nodeinfos)
  where
    nodeinfos :: M.Map SrcSpanInfo NodeInfo
    nodeinfos = foldr (\ssi -> M.insert ssi (NodeInfo ssi [])) M.empty src

    -- Assign a single comment to the right AST node
    assignComment :: Comment -> State ([ComInfo],M.Map SrcSpanInfo NodeInfo) ()
    assignComment comment@(Comment _ cspan _) =
      -- Find the biggest AST node directly in front of this comment.
      case nodeBefore comment of
        -- Comments before any AST node are handled separately.
        Nothing -> modify $ first $ (:) (ComInfo comment Nothing)

        Just ssi ->
          -- Comments on the same line as the AST node belong to this node.
          if sameline (srcInfoSpan ssi) cspan
             then insertComment After ssi
             else do nodeinfo <- gets ((M.! ssi) . snd)
                     case nodeinfo of
                       -- We've already collected comments for this
                       -- node and this comment is a continuation.
                       NodeInfo _ ((ComInfo c' _):_)
                         | aligned c' comment -> insertComment After ssi

                       -- The comment does not belong to this node.
                       -- If there is a node following this comment,
                       -- assign it to that node, else keep it here,
                       -- anyway.
                       _ ->
                         case nodeAfter comment of
                           Nothing -> insertComment After ssi
                           Just ssi' -> insertComment Before ssi'
      where
        sameline :: SrcSpan -> SrcSpan -> Bool
        sameline before after = srcSpanEndLine before == srcSpanStartLine after

        aligned :: Comment -> Comment -> Bool
        aligned (Comment _ before _) (Comment _ after _) =
          srcSpanEndLine before == srcSpanStartLine after - 1 &&
          srcSpanStartColumn before == srcSpanStartColumn after

        insertComment :: ComInfoLocation -> SrcSpanInfo -> State ([ComInfo],M.Map SrcSpanInfo NodeInfo) ()
        insertComment l ssi = modify $ second $ M.adjust (addComment (ComInfo comment (Just l))) ssi

        addComment :: ComInfo -> NodeInfo -> NodeInfo
        addComment x (NodeInfo s xs) = NodeInfo s (x : xs)

    -- Transfer collected comments into the AST.
    transferComments :: SrcSpanInfo -> State ([ComInfo],M.Map SrcSpanInfo NodeInfo) NodeInfo
    transferComments ssi =
      do ni <- gets ((M.! ssi) . snd)
         -- Sometimes, there are multiple AST nodes with the same
         -- SrcSpan.  Make sure we assign comments to only one of
         -- them.
         modify $ second $ M.adjust (\(NodeInfo s _) -> NodeInfo s []) ssi
         return ni { nodeInfoComments = reverse $ nodeInfoComments ni }

    nodeBefore (Comment _ ss _) = fmap snd $ (OrderByEnd ss) `M.lookupLT` spansByEnd
    nodeAfter (Comment _ ss _) = fmap snd $ (OrderByStart ss) `M.lookupGT` spansByStart

    spansByStart = foldr (\ssi -> M.insert (OrderByStart $ srcInfoSpan ssi) ssi) M.empty src
    spansByEnd = foldr (\ssi -> M.insert (OrderByEnd $ srcInfoSpan ssi) ssi) M.empty src
