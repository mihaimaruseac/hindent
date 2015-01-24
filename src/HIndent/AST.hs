{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

-- | AST Computations.

module HIndent.AST where

import           HIndent.Types

import           Control.Monad.State.Strict
import           Data.Data
import           Data.Function
import           Data.Traversable
import           Language.Haskell.Exts.Annotated
import Data.Monoid

data Attachment = HighestLayer | LowestLayer deriving (Eq)

data Location = Before' | After' | Inside | Wrapping | Overlapping | Same deriving (Eq)


okDirection :: Location -> Direction -> Bool
okDirection l d = (l == Before' && d == Before) ||
                  (l == After' && d == After)

-- | Where is a comment relative to a node?
commentLocation :: Comment -> NodeInfo -> Location
commentLocation (Comment _ c _) (NodeInfo (SrcSpanInfo n _) _) =
  location c n

-- | the location of the first span in terms of the second
location :: SrcSpan -> SrcSpan -> Location
location first second
  | end first < start second = Before'
  | start first > end second = After'
  | start first == start second && end first == end second = Same
  | start first >= start second && end first <= end second = Inside
  | start first <= start second && end first >= end second = Wrapping
  | otherwise = Overlapping

start :: SrcSpan -> (Int,Int)
start s = (srcSpanStartLine s, srcSpanStartColumn s)

end :: SrcSpan -> (Int,Int)
end s = (srcSpanEndLine s, srcSpanEndColumn s)

instance (Num a,Num b) => Num (a, b) where
   (a,b) - (c,d) = (a-c,b-d)

-- | starting with a reference span, is the first span better than the second
-- for attaching a comment (closer, the right direction and the right shape)?
better :: Attachment -> Direction -> SrcSpan -> SrcSpan -> SrcSpan -> Bool
better att dir ref first second
  | not (okDirection (location ref first) dir) = False
  | not (okDirection (location ref second) dir) = True
  | start first - end ref < start second - end ref = True
  | start first - end ref > start second - end ref = False
  | otherwise = case location first second of
                 Inside -> att == LowestLayer
                 Wrapping -> att == HighestLayer
                 _ -> True

addId :: (Traversable ast) => ast a -> ast (a, Int)
addId ast =
  evalState (traverse id' ast) 1
  where
    id' x = do
      n <- get
      modify (+1)
      return (x,n)

removeId :: (Traversable ast) => ast (a,b) -> ast a
removeId ast = fmap fst ast

findBest :: (Traversable ast) => Attachment -> Direction -> Comment -> ast (NodeInfo,Int) -> Maybe (NodeInfo,Int)
findBest att dir (Comment _ c _) ast =
  execState (traverse (\x' -> modify (\x -> maybeBetter x x')) ast) Nothing
  where
    maybeBetter Nothing x = Just x
    maybeBetter
      (Just x0@(NodeInfo (SrcSpanInfo n0 _) _,_))
            x1@(NodeInfo (SrcSpanInfo n1 _) _,_)
      = Just (if better att dir c n0 n1 then x0 else x1)


insertById :: (Traversable ast) => ComInfo -> Int -> ast (NodeInfo,Int) -> ast (NodeInfo, Int)
insertById comment n ast =
  fmap
  (\a@(NodeInfo s comments,x) ->
    if x==n
    then (NodeInfo s (comments<>[comment]),x)
    else a)
  ast

insertComment :: Traversable ast => Attachment-> Direction-> Comment-> ([ComInfo],ast NodeInfo) -> ([ComInfo],ast NodeInfo)
insertComment att dir comment (cs,ast) =
  let ast' = addId ast
      best = findBest att dir comment ast'
  in
  case best of
    Nothing     -> (cs<>[ComInfo comment Nothing],ast)
    Just (_,b)  -> (cs,removeId (insertById (ComInfo comment (Just dir)) b ast'))

placeComments :: forall ast. (Data (ast NodeInfo),Traversable ast,Annotated ast)
                 => Attachment -> ast SrcSpanInfo -> [Comment] -> ([ComInfo],ast NodeInfo)
placeComments att ast comments =
  let ast' = fmap (\n -> NodeInfo n []) ast
      -- Make sure to process comments top to bottom.
      reversed = reverse comments
      -- Add all comments to the ast.
      (cominfos, ast'') = foldr processComment ([], ast') reversed

  in -- Reverse order of comments at each node.
    (cominfos, fmap (\(NodeInfo n cs) -> NodeInfo n $ reverse cs) ast'')

  where processComment :: Comment
                       -> ([ComInfo],ast NodeInfo)
                       -> ([ComInfo],ast NodeInfo)
        -- Add in a single comment to the ast.
        processComment c@(Comment _ cspan _) (cs,ast) =
          -- Try to find the best previous node
          case findBest att After c (addId ast) of
            Nothing -> insertComment att Before c (cs,ast)
            -- We found the node that this comment follows.
            -- Check whether the node is on the same line.
            Just ((NodeInfo n coms),_)
              -- If it's on a different line than the node, but the node has an
              -- EOL comment, and the EOL comment and this comment are aligned,
              -- attach this comment to the preceding node.
              | ownLine && alignedWithPrevious ->
                  insertComment att After c (cs,ast)
              -- otherwise attach to the next node
              | otherwise -> insertComment att Before c (cs,ast)

              where
                ownLine = srcSpanStartLine cspan /= srcSpanEndLine (srcInfoSpan n)
                alignedWithPrevious
                  | null coms = False
                  | otherwise = case last coms of
                      -- Require single line comment after the node.
                      ComInfo (Comment False prevSpan _) (Just After) ->
                        srcSpanStartLine prevSpan == srcSpanStartLine cspan - 1 &&
                        srcSpanStartColumn prevSpan == srcSpanStartColumn cspan
                      _       -> False


-- | Annotate the AST with comments.
annotateComments :: forall ast. (Data (ast NodeInfo),Traversable ast,Annotated ast)
                 => ast SrcSpanInfo -> [Comment] -> ([ComInfo],ast NodeInfo)
annotateComments src comments =
  let
      -- Make sure to process comments top to bottom.
      reversed = reverse comments

      -- Replace source spans with node infos in the AST.
      src' = fmap (\n -> NodeInfo n []) src

      -- Add all comments to the ast.
      (cominfos, src'') = foldr processComment ([], src') reversed

  in -- Reverse order of comments at each node.
    (cominfos, fmap (\(NodeInfo n cs) -> NodeInfo n $ reverse cs) src'')

  where processComment :: Comment
                       -> ([ComInfo],ast NodeInfo)
                       -> ([ComInfo],ast NodeInfo)
        -- Add in a single comment to the ast.
        processComment c@(Comment _ cspan _) (cs,ast) =
          -- Try to find the node after which this comment lies.
          case execState (traverse (collect After c) ast) Nothing of
            -- When no node is found, the comment is a leading comment of the ast
            -- try and find the next node
            Nothing -> case execState (traverse (collect Before c) ast) Nothing of

              Nothing -> (ComInfo c Nothing : cs, ast)
              Just (NodeInfo l _) ->
                (cs, evalState (traverse (insert l (ComInfo c $ Just Before)) ast) False)


            -- We found the node that this comment follows.
            -- Check whether the node is on the same line.
            Just (NodeInfo l coms)
              -- If it's on a different line than the node, but the node has an
              -- EOL comment, and the EOL comment and this comment are aligned,
              -- attach this comment to the preceding node.
              | ownLine && alignedWithPrevious -> insertedBefore

              -- If it's on a different line than the node, look for the following node to attach it to.
              | ownLine ->
                  case execState (traverse (collect Before c) ast) Nothing of
                    -- If we don't find a node after the comment, leave it with the previous node.
                    Nothing   -> insertedBefore
                    Just (NodeInfo node _) ->
                      (cs, evalState (traverse (insert node (ComInfo c $ Just Before)) ast) False)

              -- If it's on the same line, insert this comment into that node.
              | otherwise -> insertedBefore
              where
                ownLine = srcSpanStartLine cspan /= srcSpanEndLine (srcInfoSpan l)
                insertedBefore = (cs, evalState (traverse (insert l (ComInfo c $ Just After)) ast) False)
                alignedWithPrevious
                  | null coms = False
                  | otherwise = case last coms of
                      -- Require single line comment after the node.
                      ComInfo (Comment False prevSpan _) (Just After) ->
                        srcSpanStartLine prevSpan == srcSpanStartLine cspan - 1 &&
                        srcSpanStartColumn prevSpan == srcSpanStartColumn cspan
                      _       -> False

        -- For a comment, check whether the comment is after the node.
        -- If it is, store it in the state; otherwise do nothing.
        -- The location specifies where the comment should lie relative to the node.
        collect :: Direction -> Comment -> NodeInfo -> State (Maybe NodeInfo) NodeInfo
        collect loc' c ni@(NodeInfo newL _) =
          do when (commentLocated loc' ni c)
                  (modify (maybe (Just ni)
                                 (\oldni@(NodeInfo oldL _) ->
                                    Just (if (spanTest loc' `on` srcInfoSpan) oldL newL
                                             then ni
                                             else oldni))))
             return ni

        -- Insert the comment into the ast. Find the right node and add it to the
        -- comments of that node. Do nothing afterwards.
        insert :: SrcSpanInfo -> ComInfo -> NodeInfo -> State Bool NodeInfo
        insert al c ni@(NodeInfo bl cs) =
          do done <- get
             if not done && al == bl
                then do put True
                        return (ni {nodeInfoComments = c : cs})
                else return ni

-- | Is the comment before/after the node?
commentLocated :: Direction -> NodeInfo -> Comment -> Bool
commentLocated loc' (NodeInfo (SrcSpanInfo n _) _) (Comment _ c _) =
  spanTest loc' n c

-- | For @After@, does the first span end before the second starts?
-- For @Before@, does the first span start after the second ends?
spanTest :: Direction -> SrcSpan -> SrcSpan -> Bool
spanTest loc' first second =
  
  (srcSpanStartLine after > srcSpanEndLine before) ||
  ((srcSpanStartLine after == srcSpanEndLine before) &&
   (srcSpanStartColumn after > srcSpanEndColumn before))
  where (before,after) =
          case loc' of
            After -> (first,second)
            Before -> (second,first)

spanTestNode :: Direction -> SrcSpan -> SrcSpan -> Bool
spanTestNode loc' first second =
  (srcSpanStartLine after > srcSpanStartLine before) ||
  ((srcSpanStartLine after == srcSpanStartLine before) &&
   (srcSpanEndColumn after > srcSpanEndColumn before))
  where (before,after) =
          case loc' of
            After -> (first,second)
            Before -> (second,first)

