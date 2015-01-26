{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module TestParsing where

import Data.Traversable
import           Control.Applicative
import           Data.Either
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.IO as Text
import Language.Haskell.HLint3
import           HIndent
import HIndent.AST
import           HIndent.Merge
import           HIndent.Pretty
import           HIndent.Styles.TonyDay hiding (State)
import           HIndent.Types
import           Language.Haskell.Exts.Annotated hiding (Style,prettyPrint,Pretty,style,parse)
import           Control.Monad.State.Strict
import Data.Typeable
import Text.Show.Pretty hiding (Con)
import Text.PrettyPrint.HughesPJ (Doc)
import Data.Functor.Identity

t1 :: String
t1 = "test/tony-day/test1.hs"

tc1 :: Text.Text
tc1 = Text.pack "{- multi -}\nx=1\n{- multi -}\nx'=2\n\n{- multi -}"

tc2 :: Text.Text
tc2 = Text.pack "-- single\nx=1\n-- single\nx'=2\n\n-- single"

file :: FilePath -> IO Text.Text
file = Text.readFile

showText :: Text.Text -> IO ()
showText t = 
  Text.putStr t

show' :: (Show a) => a -> Doc
show' = ppDoc 

getParse :: Text.Text -> (Module SrcSpanInfo, [Comment])
getParse t = fromParseResult (parseModuleWithComments parseMode (Text.unpack t))

getAst :: Text.Text -> ([ComInfo], Module NodeInfo)
getAst = uncurry (placeComments HighestLayer) . getParse

prettyAst :: Pretty ast => Style -> ([ComInfo], ast NodeInfo) -> Text.Builder
prettyAst style (topcomments, ast) = prettyPrint style $ do
  mapM_ (printComment Nothing) (reverse topcomments)
  pretty ast

getPretty :: Style -> Text.Text -> Text.Text
getPretty style = Text.toLazyText . prettyAst style . getAst  

getDecl :: Module NodeInfo -> Decl NodeInfo
getDecl (Module _ _ _ _ decls) = head decls

showT :: (Traversable t, Show a) => t a -> [String]
showT ast = execState (traverse (\x -> modify (\xs -> show x:xs)) ast) []

-- idT :: Num s => StateT s Identity a -> s
idT :: Module SrcSpanInfo -> Module Int
idT ast = execState (traverse (\ann' ->
                                (inc ann')) ast) 0
  where
    inc :: t -> State Int Int
    inc _ = do
      modify (+1)
      get
      
-- posnT :: (Traversable t, Typeable a) => t a -> Int
posnT :: Module SrcSpanInfo -> Module ((Int, Int), (Int, Int))
posnT ast = fmap (\(SrcSpanInfo (SrcSpan _ l0 c0 l1 c1) _)
                      -> ((l0,c0),(l1,c1)) ) ast

posnT' :: Functor f => f NodeInfo -> f ((Int, Int), (Int, Int), [ComInfo])
posnT' ast = fmap (\(NodeInfo (SrcSpanInfo (SrcSpan _ l0 c0 l1 c1) _) cs)
                      -> ((l0,c0),(l1,c1),cs) ) ast



-- show' $ posnT $ fst $ getParse "x1 = putStrLn . show"
-- show' $ posnT' $ snd $ getAst "x1 = putStrLn . show {- end -}"

-- hlint
getDeclsIdea :: Text.Text -> IO [Idea]
getDeclsIdea t = do
  (flags, classify, hint) <- autoSettings
  let (m@(Module _ _ _ _ decls),c) = getParse t
      scope = scopeCreate m
  return $ concat $ hintDecl hint scope m <$> decls





{-
testMerge :: FilePath -> FilePath -> IO ()
testMerge f f' = do
  t <- Text.readFile f
  t' <- Text.readFile f'
  let ast1 = testAst t
      ast2 = testAst t'
      ast = case (ast1,ast2) of
        (Left e1, Left e2) -> Left (e1 ++ e2)
        (Left e1, Right _) -> Left e1
        (Right _, Left e2) -> Left e2
        (Right (cominfo1,m1), Right (cominfo2,m2)) -> Right
          (cominfo1 ++ cominfo2, m1 `mergeModule` m2)
  case ast of
   Left _ -> return ()
   Right (cominfo, m) ->
     let p = prettyPrint tonyDay
             (do mapM_ (printComment Nothing) (reverse cominfo)
                 pretty m)
     in
     Text.putStr $ Text.toLazyText p


order :: (Traversable t, Num s) => t a -> (t s, s)
order ast =
  runState
  (traverse
   (\x -> do
     c <- get
     modify (\c -> (c+1))
     return c
   )
   ast)
  0
-}

-- show' $ posnT $ fst $ getParse "Proxy a a' b b' m"


isProxy :: Exp t -> Bool
isProxy (Con _ (UnQual _ (Ident _ i))) = i == "Proxy"
isProxy _ = False

isX (UnQual _ (Ident _ i)) = i == "X"
isX _ = False

isUnit (Special _ (UnitCon _)) = True
isUnit _ = False

ident (Var _ (UnQual _ (Ident _ i))) = Just i
ident _ = Nothing

data S = S { foundProxy :: Bool }

{-
lookForProxy ast
  | isProxy = modify (\x -> x {foundProxy=True})
  | isUnit =
-}
