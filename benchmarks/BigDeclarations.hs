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
