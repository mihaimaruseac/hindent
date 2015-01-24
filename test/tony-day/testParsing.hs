{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module TestParsing where

import Data.Traversable
import           Control.Applicative
import           Data.Either
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.IO as Text
import           HIndent
import HIndent.AST
import           HIndent.Merge
import           HIndent.Pretty
import           HIndent.Styles.TonyDay hiding (State)
import           HIndent.Types
import           Language.Haskell.Exts.Annotated hiding (Style,prettyPrint,Pretty,style,parse)
import           Control.Monad.State.Strict
import Data.Typeable

t1 :: String
t1 = "test/tony-day/test1.hs"

tc1 :: Text.Text
tc1 = Text.pack "{- multi -}\nx=1\n{- multi -}\nx'=2\n\n{- multi -}"

tc2 :: Text.Text
tc2 = Text.pack "-- single\nx=1\n-- single\nx'=2\n\n-- single"

testf :: Style -> FilePath -> IO ()
testf style f = do
  tf1 <- Text.readFile f
  let ast1 = testAst tf1
  case ast1 of
   Left _ -> return ()
   Right (cominfo, m) ->
     let p = prettyPrint style
             (do
                 mapM_ (printComment Nothing) (reverse cominfo)
                 pretty m)
     in
     Text.putStr $ Text.toLazyText p

getAst :: FilePath -> IO [Module ()]
getAst f = do
  tf1 <- Text.readFile f
  return $ (bare . snd) <$> rights [testAst tf1]

getAstC :: FilePath -> IO ([ComInfo], Module NodeInfo)
getAstC f = do
  t <- Text.readFile f
  let (m, c) = fromParseResult (parseModuleWithComments parseMode (Text.unpack t))
      (x, ast) = annotateComments m c
  return (x, ast)

getAstC' :: Text.Text -> IO ([ComInfo], Module NodeInfo)
getAstC' t = do
  let (m, c) = fromParseResult (parseModuleWithComments parseMode (Text.unpack t))
      (x, ast) = annotateComments m c
  return (x, ast)

getParse :: Text.Text -> (Module SrcSpanInfo, [Comment])
getParse t = fromParseResult (parseModuleWithComments parseMode (Text.unpack t))

getAnn :: Text.Text -> ([ComInfo], Module NodeInfo)
getAnn = uncurry (placeComments HighestLayer) . getParse

testt :: Text.Text -> IO ()
testt t =
  let (cominfo,m) = getAnn t
      p = prettyPrint tonyDay $ do
        mapM_ (printComment Nothing) (reverse cominfo)
        pretty m
  in
  Text.putStr $ Text.toLazyText p

testt' :: Text.Text -> IO ()
testt' t =
  let (cominfo,m) = getAnn t
      p = prettyPrint fundamental $ do
        mapM_ (printComment Nothing) (reverse cominfo)
        pretty m
  in
  Text.putStr $ Text.toLazyText p

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

getDecl :: Module NodeInfo -> Decl NodeInfo
getDecl (Module _ _ _ _ decls) = head decls



t2 :: (Traversable t, Show a) => t a -> [String]
t2 ast = execState (traverse (\x -> modify (\xs -> (show x):xs)) ast) []

tCount :: (Traversable t, Typeable a) => t a -> Int
tCount ast = execState (traverse (\x -> modify (\xs -> xs+1)) ast) 0


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


