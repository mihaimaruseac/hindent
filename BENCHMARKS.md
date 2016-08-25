# Large inputs

Bunch of declarations

``` haskell
listPrinters =
  [(''[]
   ,\(typeVariable:_) _automaticPrinter ->
      (let presentVar = varE (presentVarName typeVariable)
       in lamE [varP (presentVarName typeVariable)]
               [|(let typeString = "[" ++ fst $(presentVar) ++ "]"
                  in (typeString
                     ,\xs ->
                        case fst $(presentVar) of
                          "GHC.Types.Char" ->
                            ChoicePresentation
                              "String"
                              [("String",undefined)
                              ,("List of characters",undefined)]
                          _ ->
                            ListPresentation typeString
                                             (map (snd $(presentVar)) xs)))|]))]
printComments loc' ast = do
  let correctLocation comment = comInfoLocation comment == Just loc'
      commentsWithLocation = filter correctLocation (nodeInfoComments info)
  comments <- return $ map comInfoComment commentsWithLocation

  forM_ comments $ \comment -> do
    -- Preceeding comments must have a newline before them.
    hasNewline <- gets psNewline
    when (not hasNewline && loc' == Before) newline

    printComment (Just $ srcInfoSpan $ nodeInfoSpan info) comment
  where info = ann ast
exp' (App _ op a) =
  do (fits,st) <-
       fitsOnOneLine (spaced (map pretty (f : args)))
     if fits
        then put st
        else do pretty f
                newline
                spaces <- getIndentSpaces
                indented spaces (lined (map pretty args))
  where (f,args) = flatten op [a]
        flatten :: Exp NodeInfo
                -> [Exp NodeInfo]
                -> (Exp NodeInfo,[Exp NodeInfo])
        flatten (App _ f' a') b =
          flatten f' (a' : b)
        flatten f' as = (f',as)
infixApp :: Exp NodeInfo
         -> Exp NodeInfo
         -> QOp NodeInfo
         -> Exp NodeInfo
         -> Maybe Int64
         -> Printer ()
```

Bunch of declarations - sans comments

``` haskell
listPrinters =
  [(''[]
   ,\(typeVariable:_) _automaticPrinter ->
      (let presentVar = varE (presentVarName typeVariable)
       in lamE [varP (presentVarName typeVariable)]
               [|(let typeString = "[" ++ fst $(presentVar) ++ "]"
                  in (typeString
                     ,\xs ->
                        case fst $(presentVar) of
                          "GHC.Types.Char" ->
                            ChoicePresentation
                              "String"
                              [("String",undefined)
                              ,("List of characters",undefined)]
                          _ ->
                            ListPresentation typeString
                                             (map (snd $(presentVar)) xs)))|]))]
printComments loc' ast = do
  let correctLocation comment = comInfoLocation comment == Just loc'
      commentsWithLocation = filter correctLocation (nodeInfoComments info)
  comments <- return $ map comInfoComment commentsWithLocation

  forM_ comments $ \comment -> do
    hasNewline <- gets psNewline
    when (not hasNewline && loc' == Before) newline

    printComment (Just $ srcInfoSpan $ nodeInfoSpan info) comment
  where info = ann ast
exp' (App _ op a) =
  do (fits,st) <-
       fitsOnOneLine (spaced (map pretty (f : args)))
     if fits
        then put st
        else do pretty f
                newline
                spaces <- getIndentSpaces
                indented spaces (lined (map pretty args))
  where (f,args) = flatten op [a]
        flatten :: Exp NodeInfo
                -> [Exp NodeInfo]
                -> (Exp NodeInfo,[Exp NodeInfo])
        flatten (App _ f' a') b =
          flatten f' (a' : b)
        flatten f' as = (f',as)
infixApp :: Exp NodeInfo
         -> Exp NodeInfo
         -> QOp NodeInfo
         -> Exp NodeInfo
         -> Maybe Int64
         -> Printer ()
```

# Complex inputs

Quasi-quotes with nested lets and operators

``` haskell
quasiQuotes =
  [(''[]
   ,\(typeVariable:_) _automaticPrinter ->
      (let presentVar = varE (presentVarName typeVariable)
       in lamE [varP (presentVarName typeVariable)]
               [|(let typeString = "[" ++ fst $(presentVar) ++ "]"
                  in (typeString
                     ,\xs ->
                        case fst $(presentVar) of
                          "GHC.Types.Char" ->
                            ChoicePresentation
                              "String"
                              [("String"
                               ,StringPresentation "String"
                                                   (concatMap getCh (map (snd $(presentVar)) xs)))
                              ,("List of characters"
                               ,ListPresentation typeString
                                                 (map (snd $(presentVar)) xs))]
                            where getCh (CharPresentation "GHC.Types.Char" ch) =
                                    ch
                                  getCh (ChoicePresentation _ ((_,CharPresentation _ ch):_)) =
                                    ch
                                  getCh _ = ""
                          _ ->
                            ListPresentation typeString
                                             (map (snd $(presentVar)) xs)))|]))]
```

Lots of comments and operators

``` haskell
bob -- after bob
 =
    foo -- next to foo
    -- line after foo
        (bar
             foo -- next to bar foo
             bar -- next to bar
         ) -- next to the end paren of (bar)
        -- line after (bar)
        mu -- next to mu
        -- line after mu
        -- another line after mu
        zot -- next to zot
        -- line after zot
        (case casey -- after casey
               of
             Just -- after Just
              -> do
                 justice -- after justice
                  *
                     foo
                         (blah * blah + z + 2 / 4 + a - -- before a line break
                          2 * -- inside this mess
                          z /
                          2 /
                          2 /
                          aooooo /
                          aaaaa -- bob comment
                          ) +
                     (sdfsdfsd fsdfsdf) -- blah comment
                 putStrLn "")
        [1, 2, 3]
        [ 1 -- foo
        , ( 2 -- bar
          , 2.5 -- mu
           )
        , 3]

foo = 1 -- after foo
```
