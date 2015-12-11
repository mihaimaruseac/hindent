((haskell-mode
  . ((haskell-indent-spaces . 2)
     (hindent-style . "chris-done")
     (haskell-process-type . ghci)
     (haskell-process-path-ghci . "stack")
     (haskell-process-args-ghci . ("ghci" "--with-ghc" "ghci-ng"))))
 (haskell-cabal-mode
  . ((haskell-process-type . ghci)
     (haskell-process-path-ghci . "stack")
     (haskell-process-args-ghci . ("ghci" "--with-ghc" "ghci-ng")))))
