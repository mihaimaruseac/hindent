{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module HLindent where

import           Control.Applicative
import           Control.Monad.State.Strict
import           Data.Default
import           Data.List
import           Data.Monoid
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.IO as Text
import           Data.Traversable
import           HIndent
import           HIndent.AST
import           HIndent.Pretty
import qualified HIndent.Styles.Exact as Exact
import           HIndent.Types
import           Language.Haskell.Exts.Annotated hiding (Style,prettyPrint,Pretty,style,parse)
import qualified Language.Haskell.Exts.Annotated.ExactPrint as Exact
import           Language.Haskell.HLint3
import           Prelude
import           Text.PrettyPrint.HughesPJ (Doc)
import           Text.Show.Pretty hiding (Con)

-- combos & helpers
getAst :: Text.Text -> Module NodeInfo
getAst = uncurry (placeComments LowestLayer) . getParse

getAst' :: Attachment -> Text.Text -> Module NodeInfo
getAst' layer = uncurry (placeComments layer) . getParse

getAstOld :: Text.Text -> ([ComInfo], Module NodeInfo)
getAstOld = uncurry annotateComments . getParse

getComments :: Functor f => f NodeInfo -> f [ComInfo]
getComments ast = fmap (\(NodeInfo _ cs) -> cs) ast

getPretty :: Style -> Text.Text -> Text.Text
getPretty style = Text.toLazyText . prettyAst style . getAst  

getPretty' :: Attachment -> Style -> Text.Text -> Text.Text
getPretty' att style = Text.toLazyText . prettyAst style . getAst' att 

prettyAst :: Pretty ast => Style -> ast NodeInfo -> Text.Builder
prettyAst style ast = prettyPrint style (pretty ast)

toList :: Traversable t =>  t t1 -> [t1]
toList = reverse . flip execState [] . traverse (\x -> modify (\xs -> x:xs))

posn :: Functor f => f NodeInfo -> f ((Int, Int), (Int, Int), [ComInfo])
posn ast = fmap (\(NodeInfo (SrcSpanInfo (SrcSpan _ l0 c0 l1 c1) _) cs)
                      -> ((l0,c0),(l1,c1),cs) ) ast

show' :: (Show a) => a -> Doc
show' = ppDoc 

-- testing
commentLoc :: Text.Text -> [Int]
commentLoc = findIndices (not . null) . toList . getComments . getAst

commentLoc' :: Attachment -> Text.Text -> [Int]
commentLoc' att = findIndices (not . null) . toList . getComments . getAst' att

-- tests
t1 :: Text.Text
t1 = "x1 = putStrLn . show --end\n"

t1CommentLoc :: Bool
t1CommentLoc = commentLoc' LowestLayer t1 == [14] && commentLoc' HighestLayer t1 == [0]

exactOk' :: Text.Text -> Bool
exactOk' t = t == getPretty' LowestLayer exact t && 
            t == getPretty' HighestLayer exact t

exactOk :: Text.Text -> Bool
exactOk t = t == getPretty exact t

t1Ok :: Bool
t1Ok = exactOk' t1

t2 :: Text
t2 = "x1 = putStrLn {-1-} . {-2-} show {-3-}\n"

t2Ok :: Bool
t2Ok = exactOk t2

{- 
difficulties with using HighestLayer

Processes either pretty print the comments, then pretty print the ast element, or vice versa.  No concept exists of printing comments inside an ast element.

eg
λ> getPretty exact LowestLayer "x1 = putStrLn {-1-} . {-2-} show {-3-}\n"
"x1 = putStrLn {-1-} . {-2-} show {-3-}\n"
λ> getPretty exact HighestLayer "x1 = putStrLn {-1-} . {-2-} show {-3-}\n"
"x1 = putStrLn       .       show {-3-}{-1-}   {-2-}\n"

-}


{-
comment-only tests
λ> show' $ getComments $ getAst ""
Module [] Nothing [] [] []
λ> show' $ getComments $ getAst "\n"
Module [] Nothing [] [] []
λ> show' $ getComments $ getAst "-- 1"
Module
  [ ComInfo
      { comInfoComment =
          Comment False (SrcSpan "<unknown>.hs" 1 1 1 5) " 1"
      , comInfoLocation = Just After
      }
  ]
  Nothing
  []
  []
  []


λ> show' $ getComments $ getAst ""
Module [] Nothing [] [] []
λ> show' $ getComments $ getAst "\n"
Module [] Nothing [] [] []
λ> show' $ getComments $ getAst "-- 1"
Module
  [ ComInfo
      { comInfoComment =
          Comment False (SrcSpan "<unknown>.hs" 1 1 1 5) " 1"
      , comInfoLocation = Just After
      }
  ]
  Nothing
  []
  []
  []
λ> show' $ getComments $ getAst "-- 1\n-- 2"
Module
  [ ComInfo
      { comInfoComment =
          Comment False (SrcSpan "<unknown>.hs" 1 1 1 5) " 1"
      , comInfoLocation = Just Before
      }
  , ComInfo
      { comInfoComment =
          Comment False (SrcSpan "<unknown>.hs" 2 1 2 5) " 2"
      , comInfoLocation = Just After
      }
  ]
  Nothing
  []
  []
  []
λ> show' $ getComments $ getAst "{-1-}{-2-}"
Module
  [ ComInfo
      { comInfoComment =
          Comment True (SrcSpan "<unknown>.hs" 1 1 1 6) "1"
      , comInfoLocation = Just After
      }
  , ComInfo
      { comInfoComment =
          Comment True (SrcSpan "<unknown>.hs" 1 6 1 11) "2"
      , comInfoLocation = Just After
      }
  ]
  Nothing
  []
  []
  []
λ> 
-}

{-
comment tests

λ> show' $ getComments $ getAst "x -- 1"
Module
  []
  Nothing
  []
  []
  [ SpliceDecl
      []
      (Var
         []
         (UnQual
            []
            (Ident
               [ ComInfo
                   { comInfoComment =
                       Comment False (SrcSpan "<unknown>.hs" 1 3 1 7) " 1"
                   , comInfoLocation = Just After
                   }
               ]
               "x")))
  ]


Without the '\n', the Module happens to be the same srcSpan as any of the other options, so that the first comment attaches to the module rather than the decl

λ> show' $ posnT' $ getAst "{-1-}x{-2-}\n"
Module
  ( ( 1 , 6 ) , ( 2 , 0 ) , [] )
  Nothing
  []
  []
  [ SpliceDecl
      ( ( 1 , 6 ) , ( 1 , 7 ) , [] )
      (Var
         ( ( 1 , 6 ) , ( 1 , 7 ) , [] )
         (UnQual
            ( ( 1 , 6 ) , ( 1 , 7 ) , [] )
            (Ident
               ( ( 1 , 6 )
               , ( 1 , 7 )
               , [ ComInfo
                     { comInfoComment =
                         Comment True (SrcSpan "<unknown>.hs" 1 1 1 6) "1"
                     , comInfoLocation = Just Before
                     }
                 , ComInfo
                     { comInfoComment =
                         Comment True (SrcSpan "<unknown>.hs" 1 7 1 12) "2"
                     , comInfoLocation = Just After
                     }
                 ]
               )
               "x")))
  ]
λ> 

-}

t3 :: Text.Text
t3 = "{-1-} x1 = putStrLn {-2-} . show {-3-}\n"

testt3Ok :: Bool
testt3Ok = exactOk t3


t = "x = putStrLn {-1-} . show -- the end"


testEP :: (ExactP t, Traversable t) => t NodeInfo -> String
testEP a = Exact.exactPrint (fmap nodeInfoSpan a) (mconcat $ toList $ fmap (fmap comInfoComment . nodeInfoComments) a)

defaultPrintState :: PrintState Exact.State
defaultPrintState = PrintState 0 mempty False 0 1 Exact.State [] def False False

expPutStrLn' :: Exp NodeInfo
expPutStrLn' = Var
        NodeInfo
          { nodeInfoSpan =
              SrcSpanInfo
                { srcInfoSpan = SrcSpan "<unknown>.hs" 1 18 1 26
                , srcInfoPoints = []
                }
          , nodeInfoComments = []
          }
        (UnQual
           NodeInfo
             { nodeInfoSpan =
                 SrcSpanInfo
                   { srcInfoSpan = SrcSpan "<unknown>.hs" 1 18 1 26
                   , srcInfoPoints = []
                   }
             , nodeInfoComments = []
             }
           (Ident
              NodeInfo
                { nodeInfoSpan =
                    SrcSpanInfo
                      { srcInfoSpan = SrcSpan "<unknown>.hs" 1 18 1 26
                      , srcInfoPoints = []
                      }
                , nodeInfoComments =
                    [ ComInfo
                        { comInfoComment =
                            Comment True (SrcSpan "<unknown>.hs" 1 27 1 39) " inside "
                        , comInfoLocation = Just After
                        }
                    ]
                }
              "putStrLn"))

expShow' :: Exp NodeInfo
expShow' = Var
        NodeInfo
          { nodeInfoSpan =
              SrcSpanInfo
                { srcInfoSpan = SrcSpan "<unknown>.hs" 1 42 1 46
                , srcInfoPoints = []
                }
          , nodeInfoComments = []
          }
        (UnQual
           NodeInfo
             { nodeInfoSpan =
                 SrcSpanInfo
                   { srcInfoSpan = SrcSpan "<unknown>.hs" 1 42 1 46
                   , srcInfoPoints = []
                   }
             , nodeInfoComments = []
             }
           (Ident
              NodeInfo
                { nodeInfoSpan =
                    SrcSpanInfo
                      { srcInfoSpan = SrcSpan "<unknown>.hs" 1 42 1 46
                      , srcInfoPoints = []
                      }
                , nodeInfoComments =
                    [ ComInfo
                        { comInfoComment =
                            Comment False (SrcSpan "<unknown>.hs" 1 47 1 52) "end"
                        , comInfoLocation = Just After
                        }
                    ]
                }
              "show"))

dot' :: QOp NodeInfo
dot' = QVarOp
        NodeInfo
          { nodeInfoSpan =
              SrcSpanInfo
                { srcInfoSpan = SrcSpan "<unknown>.hs" 1 40 1 41
                , srcInfoPoints = []
                }
          , nodeInfoComments = []
          }
        (UnQual
           NodeInfo
             { nodeInfoSpan =
                 SrcSpanInfo
                   { srcInfoSpan = SrcSpan "<unknown>.hs" 1 40 1 41
                   , srcInfoPoints = []
                   }
             , nodeInfoComments = []
             }
           (Symbol
              NodeInfo
                { nodeInfoSpan =
                    SrcSpanInfo
                      { srcInfoSpan = SrcSpan "<unknown>.hs" 1 40 1 41
                      , srcInfoPoints = []
                      }
                , nodeInfoComments = []
                }
              "."))

app' :: Exp NodeInfo -> Exp NodeInfo -> QOp NodeInfo -> Exp NodeInfo
app' x1 x2 op1 = InfixApp
     NodeInfo
       { nodeInfoSpan =
           SrcSpanInfo
             { srcInfoSpan = SrcSpan "<unknown>.hs" 1 18 1 46
             , srcInfoPoints = []
             }
       , nodeInfoComments = []
       }
       x1
       op1
       x2


-- var' :: SExp NodeInfo
var' place att = 
  let cloc = if place == Before then SrcSpan "" 1 1 1 6 else SrcSpan "" 1 2 1 7
      sloc = if place == Before then SrcSpan "" 1 6 1 7 else SrcSpan "" 1 1 1 2
      c = [ ComInfo
          { comInfoComment =
               Comment True cloc "1"
          , comInfoLocation = Just place
          }
        ]
  in
  Var
        NodeInfo
          { nodeInfoSpan =
              SrcSpanInfo
                { srcInfoSpan = sloc
                , srcInfoPoints = []
                }
          , nodeInfoComments = if att == "Var" then c else []
          }
        (UnQual
           NodeInfo
             { nodeInfoSpan =
                 SrcSpanInfo
                   { srcInfoSpan = sloc
                   , srcInfoPoints = []
                   }
             , nodeInfoComments = if att == "UnQual" then c else []
             }
           (Ident
              NodeInfo
                { nodeInfoSpan =
                    SrcSpanInfo
                      { srcInfoSpan = sloc
                      , srcInfoPoints = []
                      }
                , nodeInfoComments = if att == "Ident" then c else []
                }
              "x"))


-- testAll' :: Text -> IO ()
testAll' i =
  forM_ styles
        (\style ->
           do Text.putStrLn ("-- " <> (Text.fromStrict $ styleName style) <> ":")
              Text.putStrLn $ Text.toLazyText $ prettyAst style $ getAst i
              Text.putStrLn "")


-- the rest
file :: FilePath -> IO Text.Text
file = Text.readFile

showText :: Text.Text -> IO ()
showText t = 
  Text.putStr t

getParse :: Text.Text -> (Module SrcSpanInfo, [Comment])
getParse t = fromParseResult (parseModuleWithComments parseMode (Text.unpack t))


getDecl :: Module NodeInfo -> Decl NodeInfo
getDecl (Module _ _ _ _ decls) = head decls

getSrcSpan :: NodeInfo -> SrcSpan
getSrcSpan (NodeInfo (SrcSpanInfo s _) _) = s

showT :: (Traversable t, Show a) => t a -> [String]
showT ast = execState (traverse (\x -> modify (\xs -> show x:xs)) ast) []



incT :: Module a -> Module Int
incT ast = flip evalState 0 $ traverse inc ast

inc :: t -> State Int Int
inc _ = do
      modify (+1)
      get
      
-- show' $ posnT $ fst $ getParse "x1 = putStrLn . show"
-- show' $ posnT' $ snd $ getAst "x1 = putStrLn . show {- end -}"

-- hlint
getDeclsIdea :: Text.Text -> IO [Idea]
getDeclsIdea t = do
  (flags, classify, hint) <- autoSettings
  let (m@(Module _ _ _ _ decls),c) = getParse t
      scope = scopeCreate m
  return $ concat $ hintDecl hint scope m <$> decls


getIdeas :: Text.Text -> IO [Idea]
getIdeas t = do
  (flags, classify, hint) <- autoSettings
  let (m,c) = getParse t
  return $ applyHints classify hint [(m,c)]

