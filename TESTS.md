# Introduction

This file is a test suite. Each section maps to an HSpec test, and
each line that is followed by a Haskell code fence is tested to make
sure re-formatting that code snippet produces the same result.

You can browse through this document to see what HIndent's style is
like, or contribute additional sections to it, or regression tests.

# Modules

Module header

``` haskell
module X where
```

Exports

``` haskell
module X
  (x
  ,y
  ,Z
  ,P(x, z))
  where
```

## Imports

Import lists

``` haskell
import Data.Text
import Data.Text
import qualified Data.Text as T
import qualified Data.Text (a, b, c)
import Data.Text (a, b, c)
import Data.Text hiding (a, b, c)
```

# Declarations

Type declaration

``` haskell
type EventSource a = (AddHandler a, a -> IO ())
```

# Expressions

Lazy patterns in a lambda

``` haskell
f = \ ~a -> undefined -- \~a yields parse error on input ‘\~’
```

Bang patterns in a lambda

``` haskell
f = \ !a -> undefined -- \!a yields parse error on input ‘\!’
```

List comprehensions

``` haskell
defaultExtensions =
  [ e
  | e@EnableExtension {} <- knownExtensions ] \\
  map EnableExtension badExtensions
```

Record indentation

``` haskell
getGitProvider :: EventProvider GitRecord ()
getGitProvider =
  EventProvider
  { getModuleName = "Git"
  , getEvents = getRepoCommits
  }
```

Records again

``` haskell
commitToEvent :: FolderPath -> TimeZone -> Commit -> Event.Event
commitToEvent gitFolderPath timezone commit =
  Event.Event
  { pluginName = getModuleName getGitProvider
  , eventIcon = "glyphicon-cog"
  , eventDate = localTimeToUTC timezone (commitDate commit)
  }
```

Cases

``` haskell
strToMonth :: String -> Int
strToMonth month =
  case month of
    "Jan" -> 1
    "Feb" -> 2
    _ -> error $ "Unknown month " ++ month
```

Operators

``` haskell
x =
  Value <$> thing <*> secondThing <*> thirdThing <*> fourthThing <*>
  Just thisissolong <*>
  Just stilllonger
```

# Type signatures

Class constraints

``` haskell
fun
  :: (Class a, Class b)
  => a -> b -> c
```

Tuples

``` haskell
fun :: (a, b, c) -> (a, b)
```

# Function declarations

Where clause

``` haskell
sayHello :: IO ()
sayHello = do
  name <- getLine
  putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"
```

Guards and pattern guards

``` haskell
f :: Int
f x
  | x <- Just x
  , x <- Just x =
    case x of
      Just x -> e
  | otherwise = do e
  where
    x = y
```

Case inside a `where` and `do`

``` haskell
g x =
  case x of
    a -> x
  where
    foo =
      case x of
        _ -> do
          launchMissiles
      where
        y = 2
```

Let inside a `where`

``` haskell
g x =
  let x = 1
  in x
  where
    foo =
      let y = 2
          z = 3
      in y
```

# Behaviour checks

Unicode

``` haskell
α = γ * "ω" -- υ
```

Empty module

``` haskell
```
