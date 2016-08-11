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
