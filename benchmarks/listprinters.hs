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
