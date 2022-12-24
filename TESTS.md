# HIndent test codes

This file is a test suite. Each section maps to an HSpec test, and
each line that is followed by a Haskell code fence is tested to make
sure re-formatting that code snippet produces the same result.

You can browse through this document to see what HIndent's style is
like, or contribute additional sections to it, or regression tests.

## Shebangs

No newlines after a shebang

```haskell given
#!/usr/bin/env stack

-- stack runghc
main =
 pure ()
-- https://github.com/mihaimaruseac/hindent/issues/208
```

```haskell expect
#!/usr/bin/env stack
-- stack runghc
main = pure ()
-- https://github.com/mihaimaruseac/hindent/issues/208
```

Double shebangs

```haskell
#!/usr/bin/env stack
#!/usr/bin/env stack
main = pure ()
```

## Modules

Empty module

```haskell
```

### Module headers

Without an export list

```haskell
module X where

x = 1
```

With an export list

```haskell
module X
  ( x
  , y
  , Z
  , P(x, z)
  , module Foo
  ) where
```

With an export list; indentation 4

```haskell 4
module X
    ( x
    , y
    , Z
    , P(x, z)
    ) where
```

### Module-level pragmas

A `WARNING` for a module without an export list.

```haskell
module Foo {-# WARNING "Debug purpose only." #-} where
```

A `DEPRECATED` for a module with an export list.

```haskell
module Foo {-# DEPRECATED "Use Bar." #-}
  ( x
  , y
  , z
  ) where
```

A pragma's name is converted to the SHOUT_CASE.

```haskell given
{-# lAnGuAgE CPP #-}
```

```haskell expect
{-# LANGUAGE CPP #-}
```

Pragmas, GHC options, and haddock options.

```haskell
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Foo where
```

Accept pragmas via `OPTIONS -XFOO`

```haskell
{-# OPTIONS -XPatternSynonyms #-}

import Foo (pattern Bar)
```

Accept pragmas via `OPTIONS_GHC -XFOO`

```haskell
{-# OPTIONS_GHC -XPatternSynonyms #-}

import Foo (pattern Bar)
```

A pragma's length is adjusted automatically

```haskell given
{-#         LANGUAGE OverloadedStrings          #-}
```

```haskell expect
{-# LANGUAGE OverloadedStrings #-}
```

Collect multiple extensions correctly

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

import Language.C.Types (pattern TypeName)
```

Collect multiple extensions separated by commas correctly

```haskell given
{-# LANGUAGE TypeApplications,
    PatternSynonyms #-}

import Foo (pattern Bar)

foo = bar @Int 3
```

```haskell expect
{-# LANGUAGE TypeApplications, PatternSynonyms #-}

import Foo (pattern Bar)

foo = bar @Int 3
```

Do not collect pragma-like comments

```haskell
-- {-# LANGUAGE StaticPointers #-}
{-

{-# LANGUAGE StaticPointers #-}

-}
-- @static@ is no longer a valid identifier
-- once `StaticPointers` is enabled.
static = 3
```

## Imports, foreign imports, and foreign exports

Import lists

```haskell
import Control.Lens (_2, _Just)
import Data.Text
import Data.Text
import qualified Data.Text as T
import qualified Data.Text (a, b, c)
import Data.Text (a, b, c)
import Data.Text hiding (a, b, c)
```

Shorter identifiers come first

```haskell
import Foo ((!), (!!))
```

Import with `ExplicitNamespaces`.

```haskell
{-# LANGUAGE ExplicitNamespaces #-}

import Prlude (type FilePath)
```

Import a pattern

```haskell
{-# LANGUAGE PatternSynonyms #-}

import Foo (pattern Bar)
```

Sorted

```haskell given
import B
import A
```

```haskell expect
import A
import B
```

Explicit imports - capitals first (typeclasses/types), then operators, then identifiers

```haskell given
import qualified MegaModule as M ((>>>), MonadBaseControl, void, MaybeT(..), join, Maybe(Nothing, Just), liftIO, Either, (<<<), Monad(return, (>>=), (>>)))
```

```haskell expect
import qualified MegaModule as M
  ( Either
  , Maybe(Just, Nothing)
  , MaybeT(..)
  , Monad((>>), (>>=), return)
  , MonadBaseControl
  , (<<<)
  , (>>>)
  , join
  , liftIO
  , void
  )
```

Pretty import specification

```haskell
import A hiding
  ( foobarbazqux
  , foobarbazqux
  , foobarbazqux
  , foobarbazqux
  , foobarbazqux
  , foobarbazqux
  , foobarbazqux
  )

import Name hiding ()

import {-# SOURCE #-} safe qualified Module as M hiding (a, b, c, d, e, f)
```

An import declaration importing lots of data constructors

```haskell
import Direction
  ( Direction(East, North, NorthEast, NorthWest, South, SouthEast,
          SouthWest, West)
  , allDirections
  )
```

Preserve newlines between import groups

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/200
import GHC.Monad

import CommentAfter -- Comment here shouldn't affect newlines
import HelloWorld

import CommentAfter -- Comment here shouldn't affect newlines

-- Comment here shouldn't affect newlines
import CommentAfter
```

`PackageImports`

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/480
{-# LANGUAGE PackageImports #-}

import qualified "base" Prelude as P
```

### Foreign imports and exports

A `ccall` foreign export

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

foreign export ccall "test" test :: IO ()
```

A `ccall` unsafe foreign import

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall unsafe "test" test :: IO ()
```

A `capi` foreign import

```haskell
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

foreign import capi safe "foo" test :: IO Int
```

A `stdcall` foreign import

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

foreign import stdcall safe "test" bar :: IO ()
```

A `prim` foreign import

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

foreign import prim safe "test" test :: IO ()
```

A `javascript` foreign import

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

foreign import javascript safe "test" test :: IO ()
```

## Declarations

Data family

```haskell
data family Foo a
```

`StandaloneKindSignatures`

```haskell
{-# LANGUAGE StandaloneKindSignatures #-}

type Foo :: Type -> Type -> Type
```

Default declaration

```haskell
default (Integer, Double)
```

### `ANN` pragmas

Value annotation.

```haskell
{-# ANN foo "annotation" #-}
```

Type annotation.

```haskell
{-# ANN type Foo "annotation" #-}
```

Module annotation.

```haskell
{-# ANN module "annotation" #-}
```

### Class declarations

Default signatures

```haskell
-- https://github.com/chrisdone/hindent/issues/283
class Foo a where
  bar :: a -> a -> a
  default bar :: Monoid a =>
    a -> a -> a
  bar = mappend
```

`TypeOperators` and `MultiParamTypeClasses`

```haskell
-- https://github.com/chrisdone/hindent/issues/277
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class (a :< b) c
```

#### Class constraints

Empty

```haskell
class () =>
      Foo a
```

Long

```haskell
class ( Foo a
      , Bar a
      , Baz a
      , Hoge a
      , Fuga a
      , Piyo a
      , Hogera a
      , Hogehoge a
      , Spam a
      , Ham a
      ) =>
      Quux a
```

#### Class methods

With class constraints

```haskell
class Foo f where
  myEq :: (Eq a) => f a -> f a -> Bool
```

Long signatures

```haskell
class Foo a where
  fooBarBazQuuxHogeFuga ::
       a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a
```

#### Associated type synonyms

Associated type synonyms

```haskell
class Foo a where
  type Bar b
```

Associated type synonyms annotated with injectivity information

```haskell
-- https://github.com/commercialhaskell/hindent/issues/528
class C a where
  type F a = b | b -> a
```

### Class instance declarations

Without methods

```haskell
instance C a
```

With methods

```haskell
instance C a where
  foobar = do
    x y
    k p
```

With type operators

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/342
instance Foo (->)

instance Foo (^>)

instance Foo (T.<^)
```

With a type alias

```haskell
instance Foo a where
  type Bar a = Int
```

A `where` clause between instance functions

```haskell
instance Pretty HsModule where
  pretty' = undefined
    where
      a = b
  commentsBefore = Nothing
```

With a `SPECIALISE` pragma

```haskell
instance (Show a) => Show (Foo a) where
  {-# SPECIALISE instance Show (Foo String) #-}
  show = undefined
```

#### With overlapping pragmas

`OVERLAPPING`

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/386
instance {-# OVERLAPPING #-} Arbitrary (Set Int) where
  arbitrary = undefined
```

`OVERLAPPABLE`

```haskell
instance {-# OVERLAPPABLE #-} Arbitrary Int where
  arbitrary = undefined
```

`OVERLAPS`

```haskell
instance {-# OVERLAPS #-} Arbitrary String where
  arbitrary = undefined
```

`INCOHERENT`

```haskell
instance {-# INCOHERENT #-} Arbitrary String where
  arbitrary = undefined
```

#### With class constraints

Short name

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/244
instance Num a => C a
```

Long name

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/244
instance Nuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuum a =>
         C a where
  f = undefined
```

#### Explicit foralls

Without class constraints

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/218
instance forall x. C
```

With class constraints

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/218
instance forall x. Show x => C x
```

#### Symbol class constructor

Infix

```haskell
instance Bool :?: Bool
```

Prefix

```haskell
instance (:?:) Int Bool
```

### Data declarations

Data declaration with underscore

```haskell
data Stanza =
  MkStanza
    { _stanzaBuildInfo :: BuildInfo
    , stanzaIsSourceFilePath :: FilePath -> Bool
    }
```

A data declaration with typeclass constraints

```haskell
data Ord a =>
     Foo =
  Foo a
```

Multiple constructors at once

```haskell
data Foo =
  Foo
    { foo, bar, baz, qux, quux :: Int
    }
```

No fields

```haskell
data Foo
```

Single field

```haskell
data Foo =
  Foo
```

Multiple unnamed fields

```haskell
data HttpException
  = InvalidStatusCode Int
  | MissingContentHeader
```

A lot of unnamed fields in a constructor

```haskell
data Foo =
  Foo
    String
    String
    String
    String
    String
    String
    String
    String
    String
    String
    String
```

A banged field

```haskell
data Foo =
  Foo !Int
```

A record constructor with a field

```haskell
data Foo =
  Foo
    { foo :: Int
    }
```

Multiple constructors with fields

```haskell
data Expression a
  = VariableExpression
      { id :: Id Expression
      , label :: a
      }
  | FunctionExpression
      { var :: Id Expression
      , body :: Expression a
      , label :: a
      }
  | ApplyExpression
      { func :: Expression a
      , arg :: Expression a
      , label :: a
      }
  | ConstructorExpression
      { id :: Id Constructor
      , label :: a
      }
```

A mixture of constructors with unnamed fields and record constructors

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/393
data X
  = X
      { x :: Int
      , x' :: Int
      }
  | X'
```

An infix data constructor

```haskell
data Foo =
  Int :--> Int
```

An `UNPACK`ed field.

```haskell
data Foo =
  Foo
    { x :: {-# UNPACK #-} Int
    }
```

An `NOUNPACK`ed field.

```haskell
data Foo =
  Foo
    { x :: {-# NOUNPACK #-} !Int
    }
```

A lazy field.

```haskell
data Foo =
  Foo
    { x :: ~Int
    }
```

#### Fields with `forall` constraints

Single

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/278
data Link c1 c2 a c =
  forall b. (c1 a b, c2 b c) =>
            Link (Proxy b)
```

Multiple

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/443
{-# LANGUAGE ExistentialQuantification #-}

data D =
  forall a b c. D a b c
```

#### Derivings

With a single constructor

```haskell
data Simple =
  Simple
  deriving (Show)
```

With multiple constructors

```haskell
data Stuffs
  = Things
  | This
  | That
  deriving (Show)
```

With a record constructor

```haskell
-- From https://github.com/mihaimaruseac/hindent/issues/167
data Person =
  Person
    { firstName :: !String -- ^ First name
    , lastName :: !String -- ^ Last name
    , age :: !Int -- ^ Age
    }
  deriving (Eq, Show)
```

Multiple derivings

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/289
newtype Foo =
  Foo Proxy
  deriving ( Functor
           , Applicative
           , Monad
           , Semigroup
           , Monoid
           , Alternative
           , MonadPlus
           , Foldable
           , Traversable
           )
```

Various deriving strategies

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/503
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Foo where

import Data.Typeable
import GHC.Generics

newtype Number a =
  Number a
  deriving (Generic)
  deriving stock (Ord)
  deriving newtype (Eq)
  deriving anyclass (Typeable)
  deriving (Show) via a
```

`StandaloneDeriving`

```haskell
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

data Foo =
  Foo

deriving instance Eq Foo

deriving stock instance Ord Foo

deriving via (Foo a) instance Show (Bar a)
```

#### GADT declarations

With a kind signature

```haskell
data Ty :: (* -> *) where
  TCon
    :: { field1 :: Int
       , field2 :: Bool}
    -> Ty Bool
  TCon' :: (a :: *) -> a -> Ty a
```

Without a kind signature

```haskell
data Foo where
  Foo
    :: forall v. Ord v
    => v
    -> v
    -> Foo
```

With a `forall` but no contexts

```haskell
data Foo where
  Foo :: forall v. v -> v -> Foo
```

With a context but no `forall`s

```haskell
data Foo where
  Foo :: (Ord v) => v -> v -> Foo
```

### Data instance declarations

Without type applications

```haskell
data instance Foo Int =
  FInt
```

With type applications

```haskell
data instance Foo @k a =
  FString
```

### Function declarations

Case inside `do` and lambda

```haskell
foo =
  \x -> do
    case x of
      Just _ -> 1
      Nothing -> 2
```

A `case` inside a `let`.

```haskell
f = do
  let (x, xs) =
        case gs of
          [] -> undefined
          (x':xs') -> (x', xs')
  undefined
```

A `do` inside a lambda.

```haskell
printCommentsAfter =
  case commentsAfter p of
    xs -> do
      forM_ xs $ \(L loc c) -> do
        eolCommentsArePrinted
```

Case with natural pattern (See NPat of https://hackage.haskell.org/package/ghc-lib-parser-9.2.3.20220527/docs/Language-Haskell-Syntax-Pat.html#t:Pat)

```haskell
foo =
  case x of
    0 -> pure ()
    _ -> undefined
```

```haskell
s8_stripPrefix bs1@(S.PS _ _ l1) bs2
  | bs1 `S.isPrefixOf` bs2 = Just (S.unsafeDrop l1 bs2)
  | otherwise = Nothing
```

A `do` inside a guard arm

```haskell
f
  | x == 1 = do
    a
    b
```

`if` having a long condition

```haskell
foo =
  if fooooooo ||
     baaaaaaaaaaaaaaaaaaaaa || apsdgiuhasdpfgiuahdfpgiuah || bazzzzzzzzzzzzz
    then a
    else b
```

A long signature inside a where clause

```haskell
cppSplitBlocks :: ByteString -> [CodeBlock]
cppSplitBlocks inp = undefined
  where
    spanCPPLines ::
         [(Int, ByteString)] -> ([(Int, ByteString)], [(Int, ByteString)])
    spanCPPLines = undefined
```

A `forall` type inside a where clause

```haskell
replaceAllNotUsedAnns :: HsModule -> HsModule
replaceAllNotUsedAnns = everywhere app
  where
    app ::
         forall a. Data a
      => (a -> a)
    app = undefined

f :: a
f = undefined
  where
    ggg ::
         forall a. Typeable a
      => a
      -> a
    ggg = undefined
```

Prefix notation for operators

```haskell
(+) a b = a
```

Guards and pattern guards

```haskell
f x
  | x <- Just x
  , x <- Just x =
    case x of
      Just x -> e
  | otherwise = do e
  where
    x = y
```

Where clause

```haskell
sayHello = do
  name <- getLine
  putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"
```

An empty line is inserted after an empty `where`

```haskell given
f = evalState
    -- A comment
  where
```

```haskell expect
f = evalState
    -- A comment
  where

```

Multiple function declarations with an empty `where`

```haskell
f = undefined
  where


g = undefined
```

Let inside a `where`

```haskell
g x =
  let x = 1
   in x
  where
    foo =
      let y = 2
          z = 3
       in y
```

The indent after a top-level `where` has always 2 spaces.

```haskell 4
f = undefined
  where
    g = undefined
```

The indent after a `where` inside a `case` depends on the indent space setting

```haskell 4
f =
    case x of
        x -> undefined
            where y = undefined
```

#### Pattern matchings

View pattern

```haskell
foo (f -> Just x) = print x
foo _ = Nothing
```

Match against a list

```haskell
head [] = undefined
head [x] = x
head xs = head $ init xs

foo [Coord _ _, Coord _ _] = undefined
```

Multiple matchings

```haskell
head' [] = Nothing
head' (x:_) = Just x
```

n+k patterns

```haskell
f (n+5) = 0
```

Binary symbol data constructor in pattern

```haskell
f (x :| _) = x

f' ((:|) x _) = x

f'' ((Data.List.NonEmpty.:|) x _) = x

g (x:xs) = x

g' ((:) x _) = x
```

Infix constructor pattern

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/424
from $ \(author `InnerJoin` post) -> pure ()
```

Unboxed sum pattern matching.

```haskell
{-# LANGUAGE UnboxedSums #-}

f (# x | | | #) = undefined
```

Pattern matching against a infix constructor with a module name prefix

```haskell
foo (a FOO.:@: b) = undefined
```

##### Pattern matchings against record

Short

```haskell
fun Rec {alpha = beta, gamma = delta, epsilon = zeta, eta = theta, iota = kappa} = do
  beta + delta + zeta + theta + kappa
```

Long

```haskell
fun Rec { alpha = beta
        , gamma = delta
        , epsilon = zeta
        , eta = theta
        , iota = kappa
        , lambda = mu
        } =
  beta + delta + zeta + theta + kappa + mu + beta + delta + zeta + theta + kappa
```

Another long one

```haskell
resetModuleStartLine m@HsModule { hsmodAnn = epa@EpAnn {..}
                                , hsmodName = Just (L (SrcSpanAnn _ (RealSrcSpan sp _)) _)
                                } = undefined
```

Symbol constructor, short

```haskell
fun ((:..?) {}) = undefined
```

Symbol constructor, long

```haskell
fun (:..?) { alpha = beta
           , gamma = delta
           , epsilon = zeta
           , eta = theta
           , iota = kappa
           , lambda = mu
           } =
  beta + delta + zeta + theta + kappa + mu + beta + delta + zeta + theta + kappa
```

Symbol field

```haskell
f (X {(..?) = x}) = x
```

Punned symbol field

```haskell
f' (X {(..?)}) = (..?)
```

`RecordWileCards`

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/274
foo (bar@Bar {..}) = Bar {..}

resetModuleNameColumn m@HsModule {hsmodName = Just (L (SrcSpanAnn epa@EpAnn {..} sp) name)} =
  m

bar Bar {baz = before, ..} = Bar {baz = after, ..}
```

As pattern

```haskell
f all@(x:xs) = all
```

### Infix declarations

infixl

```haskell
infixl 1 ^-^
```

infixr

```haskell
infixr 1 ^-^
```

infix

```haskell
infix 1 ^-^
```

### Pattern synonym declarations

Unidirectional with a pattern type signature

```haskell
{-# LANGUAGE PatternSynonyms #-}

pattern Foo :: Int -> Int -> [Int]

pattern Foo x y <- [x, y]
```

Bidirectional record pattern

```haskell
{-# LANGUAGE PatternSynonyms #-}

pattern Pair {x, y} = (x, y)
```

#### Explicit bidirectional

With a prefix constructor

```haskell
{-# LANGUAGE PatternSynonyms #-}

pattern Fst x <- (x, x)
  where Fst x = (x, 0)
```

With an infix constructor

```haskell
{-# LANGUAGE PatternSynonyms #-}

pattern x :| xs <- x : xs
  where a :| b = a : b
```

### Pragma declarations

`INLINE`

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/255
{-# INLINE f #-}
f :: Int -> Int
f n = n
```

`NOINLINE` with an operator enclosed by parentheses

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/415
{-# NOINLINE (<>) #-}
```

`INLINABLE`

```haskell
{-# INLINABLE f #-}
f :: a
f = undefined
```

`OPAQUE`

```haskell since 9.4.0
{-# OPAQUE f #-}
f :: a
f = undefined
```

`INLINE` with levels

```haskell
{-# INLINE [0] f #-}
{-# INLINE [~1] g #-}
```

A `DEPRECATED`.

```haskell
{-# DEPRECATED
giveUp "Never give up."
 #-}

giveUp = undefined
```

A `WARNING`.

```haskell
{-# WARNING
debugCode "The use of 'debugCode'"
 #-}
```

A `COMPLETE`

```haskell
{-# COMPLETE Single, Anylist #-}
```

Top-level `SPECIALISE`

```haskell
{-# SPECIALISE lookup :: [(Int, Int)] -> Int -> Maybe Int #-}
```

A `SCC`

```haskell
{-# SCC bar #-}
```

#### Rule declarations

Without `forall`s

```haskell
{-# RULES
"foo/bar" foo = bar
 #-}
```

With `forall` but no type signatures

```haskell
{-# RULES
"piyo/pochi" forall a. piyo a = pochi a a
 #-}
```

With `forall` and type signatures

```haskell
{-# RULES
"hoge/fuga" forall (a :: Int). hoge a = fuga a a
 #-}
```

### Role annotation declarations

`normal`

```haskell
{-# LANGUAGE RoleAnnotations #-}

type role Foo nominal
```

`representational`

```haskell
{-# LANGUAGE RoleAnnotations #-}

type role Bar representational
```

`phantom`

```haskell
{-# LANGUAGE RoleAnnotations #-}

type role Baz phantom
```

### Type family declarations

Without annotations

```haskell
type family Id a
```

With annotations

```haskell
type family Id a :: *
```

With injectivity annotations

```haskell
type family Id a = r | r -> a
```

Closed type families

```haskell
type family Closed (a :: k) :: Bool where
  Closed (x @Int) = 'Int
  Closed x = 'True
```

### Type family instance declarations

Without holes

```haskell
type instance Id Int = Int
```

With a hole

```haskell
type instance Id _ = String
```

### Type signature declarations

Multiple function signatures at once

```haskell
a, b, c :: Int
```

Type using a numeric value

```haskell
f :: Foo 0
```

Type using a character value

```haskell
f :: Foo 'a'
```

Type using a unicode string value

```haskell
f :: Foo "あ"
```

A dot not enclosed by spaces is printed correctly if `OverloadedRecordDot` is not enabled.

```haskell given
f :: forall a.(Data a, Typeable a) => a
```

```haskell expect
f :: forall a. (Data a, Typeable a)
  => a
```

Short

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/390
fun :: Short
fun = undefined
```

Always break after `::` on overlong signatures

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/390
someFunctionSignature ::
     Wiiiiiiiiiiiiiiiiith
  -> Enough
  -> (Arguments -> To ())
  -> Overflow (The Line Limit)
```

A long type is broken into lines

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/359
thing ::
     ( ResB.BomEx
     , Maybe [( Entity BomSnapshot
              , ( [ResBS.OrderSubstituteAggr]
                , ( Maybe (Entity BomSnapshotHistory)
                  , Maybe (Entity BomSnapshotHistory))))])
  -> [(ResB.BomEx, Maybe ResBS.BomSnapshotAggr)]
```

Long parameter list with a `forall`

```haskell
fooooooooo ::
     forall a.
     Fooooooooooooooo a
  -> Fooooooooooooooo a
  -> Fooooooooooooooo a
  -> Fooooooooooooooo a
```

Implicit parameters

```haskell
f :: (?x :: Int) => Int
```

Quasiquotes in types

```haskell
fun :: [a|bc|]
```

Implicit parameters

```haskell
f :: (?x :: Int) => Int
```

Tuples

```haskell
fun :: (a, b, c) -> (a, b)
```

Infix operator

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/301
(+) :: ()
```

With a record

```haskell
url :: r {url :: String} => r -> Integer
```

`forall` type

```haskell
f :: (forall a. Data a =>
                  a -> a)
  -> (forall a b. Data a =>
                    a -> b)
g :: forall a b. a -> b
```


An infix operator containing `#`

```haskell
(#!) :: Int -> Int -> Int
```

Multiple line function signature inside a `where`

```haskell 4
foo = undefined
  where
    go :: Fooooooooooooooooooooooo
       -> Fooooooooooooooooooooooo
       -> Fooooooooooooooooooooooo
       -> Fooooooooooooooooooooooo
    go = undefined
```

`UnboxedSums`

```haskell
{-# LANGUAGE UnboxedSums #-}

f :: (# Int | Bool | String #)
```

#### Promoted types

Promoted lists

```haskell
fun1 :: Def ('[ Ref s (Stored Uint32), IBool] T.:-> IBool)
fun1 = undefined

fun2 :: Def ('[ Ref s (Stored Uint32), IBool] :-> IBool)
fun2 = undefined
```

Promoted list (issue #348)

```haskell
a :: A '[ 'True]
-- nested promoted list with multiple elements.
b :: A '[ '[ 'True, 'False], '[ 'False, 'True]]
```

Class constraints should leave `::` on same line

``` haskell
-- see https://github.com/chrisdone/hindent/pull/266#issuecomment-244182805
fun ::
     (Class a, Class b)
  => fooooooooooo bar mu zot
  -> fooooooooooo bar mu zot
  -> c
```

`forall` type

```haskell
f :: (forall a. Data a =>
                  a -> a)
  -> (forall a b. Data a =>
                    a -> b)
g :: forall a b. a -> b
```

An infix operator containing `#`

```haskell
(#!) :: Int -> Int -> Int
```

Prefix promoted symbol type constructor

```haskell
a :: '(T.:->) 'True 'False
b :: (T.:->) 'True 'False
c :: '(:->) 'True 'False
d :: (:->) 'True 'False
```

#### Symbol type constructors

Infix

```haskell
f :: a :?: b
```

Prefix

```haskell
f' :: (:?:) a b
```

#### Type signature with class constraints

Single

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/244
x :: Num a => a
x = undefined
```

Multiple

```haskell
fun :: (Class a, Class b) => a -> b -> c
```

Long constraints

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/222
foo ::
     ( Foooooooooooooooooooooooooooooooooooooooooo
     , Foooooooooooooooooooooooooooooooooooooooooo
     )
  => A
```

Class constraints should leave `::` on same line

```haskell
-- see https://github.com/mihaimaruseac/hindent/pull/266#issuecomment-244182805
fun ::
     (Class a, Class b)
  => fooooooooooo bar mu zot
  -> fooooooooooo bar mu zot
  -> c
```

Symbol class constructor in class constraint

```haskell
f :: (a :?: b) => (a, b)
f' :: ((:?:) a b) => (a, b)
```

### Type synonym declarations

Short

```haskell
type EventSource a = (AddHandler a, a -> IO ())
```

Long

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/290
type MyContext m
   = ( MonadState Int m
     , MonadReader Int m
     , MonadError Text m
     , MonadMask m
     , Monoid m
     , Functor m)
```

Infix type constructor

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/417
type API = api1 :<|> api2
```

Type with a string

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/451
type Y = X "abc\n\n\ndef"
```

`TypeOperators`

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/277
{-# LANGUAGE TypeOperators #-}

type m ~> n = ()
```

#### Functional dependencies

Short

```haskell
-- https://github.com/commercialhaskell/hindent/issues/323
class Foo a b | a -> b where
  f :: a -> b
```

Long

```haskell
-- https://github.com/commercialhaskell/hindent/issues/323
class Foo a b c d e f
  | a b c d e -> f
  , a b c d f -> e
  , a b c e f -> d
  , a b d e f -> c
  , a c d e f -> b
  , b c d e f -> a
  where
  foo :: a -> b -> c -> d -> e -> f
```

#### With class constraints

Single

```haskell
-- https://github.com/commercialhaskell/hindent/issues/459
class Class1 a =>
      Class2 a
  where
  f :: a -> Int
```

Multiple

```haskell
-- https://github.com/commercialhaskell/hindent/issues/459
class (Eq a, Show a) =>
      Num a
  where
  (+), (-), (*) :: a -> a -> a
  negate :: a -> a
  abs, signum :: a -> a
  fromInteger :: Integer -> a
```

#### MINIMAL pragmas

Monad example

```haskell
class A where
  {-# MINIMAL return, ((>>=) | (join, fmap)) #-}
```

Very long names #310

```haskell
class A where
  {-# MINIMAL averylongnamewithnoparticularmeaning
            | ananotherverylongnamewithnomoremeaning #-}
```

## Expressions

A minus sign

```haskell
f = -(3 + 5)
```

Lists

```haskell
exceptions = [InvalidStatusCode, MissingContentHeader, InternalServerError]

exceptions =
  [ InvalidStatusCode
  , MissingContentHeader
  , InternalServerError
  , InvalidStatusCode
  , MissingContentHeader
  , InternalServerError
  ]
```

Multi-way if

```haskell
x =
  if | x <- Just x
     , x <- Just x ->
       case x of
         Just x -> e
         Nothing -> p
     | otherwise -> e
```

Type application

```haskell
{-# LANGUAGE TypeApplications #-}

fun @Int 12
```

An expression with a SCC pragma

```haskell
foo = {-# SCC foo #-} undefined
```

A hole

```haskell
foo = 3 + _
```

Implicit value

```haskell
foo = ?undefined
```

`UnboxedSums`

```haskell
{-# LANGUAGE UnboxedSums #-}

f = (# | Bool #)
```

`StaticPointers`

```haskell
{-# LANGUAGE StaticPointers #-}

f = static 1
```

### Arrows

`-<`

```haskell
{-# LANGUAGE Arrows #-}

f =
  proc foo -> do
    bar -< baz
    aaa >- bbb
```

`-<<`

```haskell
{-# LANGUAGE Arrows #-}

f =
  proc foo -> do
    g bar -<< baz
    aaaaa >>- h bbb
```

`(| ... |)`

```haskell
{-# LANGUAGE Arrows #-}

f = proc g -> (|foo (bar -< g) (baz -< g)|) zz
```

Lambda equation.

```haskell
{-# LANGUAGE Arrows #-}

f = proc g -> \x -> x -< g
```

Case expression.

```haskell
{-# LANGUAGE Arrows #-}

f =
  proc g ->
    case h of
      [] -> i -< ()
      (_:_) -> j -< ()
```

Lambda case

```haskell
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

f =
  proc g ->
    \case
      _ -> h -< ()
```

`if ... then ... else`

```haskell
{-# LANGUAGE Arrows #-}

f =
  proc g ->
    if x
      then h -< g
      else t -< g
```

`let ... in`

```haskell
{-# LANGUAGE Arrows #-}

f =
  proc g ->
    let x = undefined
        y = undefined
     in returnA -< g
```

### Case expressions

Normal case

```haskell
strToMonth :: String -> Int
strToMonth month =
  case month of
    "Jan" -> 1
    "Feb" -> 2
    _ -> error $ "Unknown month " ++ month
```

Inside a `where` and `do`

```haskell
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

Empty case

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/414
{-# LANGUAGE EmptyCase #-}

f1 = case () of {}
```

Empty lambda case

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/414
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}

f2 = \case {}
```

A guard in a case

```haskell
f =
  case g of
    []
      | even h -> Nothing
    _ -> undefined
```

cases

```haskell since 9.4.1
foo =
  \cases
    1 1 -> 1
    _ _ -> 2
```

### `do` expressions

Long function applications

```haskell
test = do
  alphaBetaGamma deltaEpsilonZeta etaThetaIota kappaLambdaMu nuXiOmicron piRh79
  alphaBetaGamma deltaEpsilonZeta etaThetaIota kappaLambdaMu nuXiOmicron piRho80
  alphaBetaGamma
    deltaEpsilonZeta
    etaThetaIota
    kappaLambdaMu
    nuXiOmicron
    piRhoS81
```

Do as a left-hand side of an infix operation

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/238
-- https://github.com/mihaimaruseac/hindent/issues/296
block =
  do ds <- inBraces $ inWhiteSpace declarations
     return $ Block ds
     <?> "block"
```

#### Bindings

Short

```haskell
foo = do
  mcp <- findCabalFiles (takeDirectory abssrcpath) (takeFileName abssrcpath)
  print mcp
```

Large

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/221
x = do
  config <- execParser options
  comments <-
    case config of
      Diff False args -> commentsFromDiff args
      Diff True args -> commentsFromDiff ("--cached" : args)
      Files args -> commentsFromFiles args
  mapM_ (putStrLn . Fixme.formatTodo) (concatMap Fixme.getTodos comments)
```

#### `let` bindings

With type signatures but no class constraints

```haskell
f = do
  let g :: Int
      g = 3
  print g
```

With both type signatures and class constraints

```haskell
f = do
  let try :: Typeable b => b
      try = undefined
  undefined
```

#### `RecursiveDo`

`rec`

```haskell
{-# LANGUAGE RecursiveDo #-}

f = do
  a <- foo
  rec b <- a c
      c <- a b
  return $ b + c
```

`mdo`

```haskell
{-# LANGUAGE RecursiveDo #-}

g = mdo
  foo
  bar
```

### Function applications

Long line, tuple

```haskell
test
  (alphaBetaGamma, deltaEpsilonZeta, etaThetaIota, kappaLambdaMu, nuXiOmicro79)
  (alphaBetaGamma, deltaEpsilonZeta, etaThetaIota, kappaLambdaMu, nuXiOmicron80)
  ( alphaBetaGamma
  , deltaEpsilonZeta
  , etaThetaIota
  , kappaLambdaMu
  , nuXiOmicronP81)
```

Long line, tuple section

```haskell
test
  (, alphaBetaGamma, , deltaEpsilonZeta, , etaThetaIota, kappaLambdaMu, nu79, )
  (, alphaBetaGamma, , deltaEpsilonZeta, , etaThetaIota, kappaLambdaMu, , n80, )
  (
  , alphaBetaGamma
  ,
  , deltaEpsilonZeta
  ,
  , etaThetaIota
  , kappaLambdaMu
  ,
  , nu81
  ,)
```

Linebreaks after very short names if the total line length goes over the limit

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/405
t =
  f "this is a very loooooooooooooooooooooooooooong string that goes over the line length"
    argx
    argy
    argz

t =
  function
    "this is a very loooooooooooooooooooooooooooong string that goes over the line length"
    argx
    argy
    argz
```

### Lambda expressions

Lazy patterns

```haskell
f = \ ~a -> undefined
-- \~a yields parse error on input ‘\~’
```

Bang patterns

```haskell
f = \ !a -> undefined
-- \!a yields parse error on input ‘\!’
```

An infix operator with a lambda expression

```haskell
for xs $ \x -> do
  left x
  right x
```

Nested lambdas

```haskell
foo :: IO ()
foo =
  alloca 10 $ \a ->
    alloca 20 $ \b ->
      cFunction fooo barrr muuu (fooo barrr muuu) (fooo barrr muuu)
```

In a `case`

```haskell
f x =
  case filter (\y -> isHappy y x) of
    [] -> Nothing
    (z:_) -> Just (\a b -> makeSmile z a b)
```

### Let ... in expressions

With bang parameters

```haskell
f =
  let !x = 3
   in x
```

With implicit parameters

```haskell
f =
  let ?x = 42
   in f
```

inside a `do`

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/467
main :: IO ()
main = do
  let x = 5
   in when (x > 0) (return ())
```

### List comprehensions

Short

```haskell
map f xs = [f x | x <- xs]
```

Long

```haskell
defaultExtensions =
  [ e
  | EnableExtension {extensionField1 = extensionField1} <-
      knownExtensions knownExtensions
  , let a = b
    -- comment
  , let c = d
    -- comment
  ]
```

Another long one

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/357
foo =
  [ (x, y)
  | x <- [1 .. 10]
  , y <- [11 .. 20]
  , even x
  , even x
  , even x
  , even x
  , even x
  , odd y
  ]
```

With operators

```haskell
defaultExtensions =
  [e | e@EnableExtension {} <- knownExtensions] \\
  map EnableExtension badExtensions
```

Transform list comprehensions

```haskell
list =
  [ (x, y, map the v)
  | x <- [1 .. 10]
  , y <- [1 .. 10]
  , let v = x + y
  , then group by v using groupWith
  , then take 10
  , then group using permutations
  , t <- concat v
  , then takeWhile by t < 3
  ]
```

#### Parallel list comprehensions

Short

```haskell
zip xs ys = [(x, y) | x <- xs | y <- ys]
```

Long

```haskell
fun xs ys =
  [ (alphaBetaGamma, deltaEpsilonZeta)
  | x <- xs
  , z <- zs
  | y <- ys
  , cond
  , let t = t
  ]
```

### Operators

Bad

```haskell
x =
  Value <$> thing <*> secondThing <*> thirdThing <*> fourthThing <*>
  Just thisissolong <*>
  Just stilllonger <*>
  evenlonger
```

Good

```haskell pending
x =
  Value <$> thing <*> secondThing <*> thirdThing <*> fourthThing <*>
  Just thisissolong <*> Just stilllonger <*> evenlonger
```

With `do`

```haskell
for xs $ do
  left x
  right x
```

With lambda-case

```haskell
for xs $ \case
  Left x -> x
```

`$` chain

```haskell
f =
  Right $
  S.lazyByteStrings $ addPrefix prefix $ S.toLazyByteString $ prettyPrint m
```

Qualified operator as an argument

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/273
foo = foldr1 (V.++) [V.empty, V.empty]
```

Apply an infix operator in prefix style

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/273
ys = (++) [] []
```

Qualified operator

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/273
xs = V.empty V.++ V.empty
```

In parentheses

```haskell
cat = (++)
```

Qualified operator in parentheses

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/273
cons = (V.++)
```

A list constructor enclosed by parentheses

```haskell
cons = (:)
```

A data constructor enclosed by parentheses

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/422
data T a =
  a :@ a

test = (:@)
```

Force indent and print RHS in a top-level expression

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/473
template $
  haskell
    [ SomeVeryLongName
    , AnotherLongNameEvenLongToBreakTheLine
    , LastLongNameInList
    ]
```

### Primitive type values

`Char`

```haskell
a = 'a'
```

`\n` as a `Char`

```haskell
a = '\n'
```

`String` with a `\n`

```haskell
a = "bcd\nefgh"
```

Multiple line string

```haskell
foo =
  "hoge \
 \ fuga"
  where
    bar =
      "foo \
     \ bar"
```

Hex integers

```haskell
a = 0xa5
```

Unboxed integers

```haskell
a = 0#
```

Unboxed floating point numbers

```haskell
a = 3.3#
```

Unboxed `Char`

```haskell
a = 'c'#
```

Unboxed `String`

```haskell
a = "Foo"#
```

### Quasi-quotes

Body has multiple lines.

```haskell
{-# LANGUAGE QuasiQuotes #-}

f =
  [s|First line
Second line|]
```

Body has a top-level declaration.

```haskell
{-# LANGUAGE QuasiQuotes #-}

f =
  [d| f :: Int -> Int
      f = undefined |]
```

Typed quote.

```haskell
f = [|| a ||]
```

Preserve the trailing newline.

```haskell
{-# LANGUAGE QuasiQuotes #-}

f =
  [s|foo
|]
```
### Ranges

from

```haskell
a = [1 ..]
```

from to

```haskell
a = [1 .. 9]
```

from then

```haskell
b = [1,3 ..]
```

from then to

```haskell
c = [1,3 .. 9]
```

### Records

No fields

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/366
foo = Nothing {}
```

Short

```haskell
getGitProvider :: EventProvider GitRecord ()
getGitProvider =
  EventProvider {getModuleName = "Git", getEvents = getRepoCommits}
```

Medium

```haskell
commitToEvent :: FolderPath -> TimeZone -> Commit -> Event.Event
commitToEvent gitFolderPath timezone commit =
  Event.Event
    {pluginName = getModuleName getGitProvider, eventIcon = "glyphicon-cog"}
```

Long

```haskell
commitToEvent :: FolderPath -> TimeZone -> Commit -> Event.Event
commitToEvent gitFolderPath timezone commit =
  Event.Event
    { pluginName = getModuleName getGitProvider
    , eventIcon = "glyphicon-cog"
    , eventDate = localTimeToUTC timezone (commitDate commit)
    }
```

Another long one

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/358
foo =
  assert
    sanityCheck
    BomSnapshotAggr
      { snapshot = Just bs
      , previousId = M.bomSnapshotHistoryPreviousId . entityVal <$> bsp
      , nextId = M.bomSnapshotHistoryNextId . entityVal <$> bsn
      , bomEx = bx''
      , orderSubstitutes =
          S.fromList . map OrderSubstituteAggrByCreatedAtAsc $ subs
      , snapshotSubstitute = msub
      }
```

Record body may be in one line even if a new line is inserted after the variable name.

```haskell
addCommentsToNode mkNodeComment newComments nodeInfo@(NodeInfo (SrcSpanInfo _ _) existingComments) =
  nodeInfo
    {nodeInfoComments = existingComments <> map mkBeforeNodeComment newComments}
```

Symbol constructor

```haskell
f = (:..?) {}
```

Symbol field

```haskell
f x = x {(..?) = wat}

g x = Rec {(..?)}
```

A field updater in a `do` inside a `let ... in`.

```haskell
f = undefined
  where
    g h =
      let x = undefined
       in do foo
             pure
               h
                 { grhssLocalBinds =
                     HsValBinds x (ValBinds (newSigs newSigMethods))
                 }
```

`OverloadedRecordDot`

```haskell since 9.2.2
{-# LANGUAGE OverloadedRecordDot #-}

data Rectangle =
  Rectangle
    { width :: Int
    , height :: Int
    }

area :: Rectangle -> Int
area r = r.width * r.height

foo = (.x.y)
```

`OverloadedRecordUpdate`

```haskell since 9.2.0
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

foo = bar {baz.qux = 1}
```

### Sections

With a LHS

```haskell
double = (2 *)
```

With a RHS

```haskell
halve = (/ 2)
```

With a large RHS

```haskell
foo =
  (`elem` concat
            [ [20, 68, 92, 112, 28, 124, 116, 80]
            , [21, 84, 87, 221, 127, 255, 241, 17]
            ])
```

## Template Haskell

Expression brackets

```haskell
add1 x = [|x + 1|]
```

Pattern brackets

```haskell
mkPat = [p|(x, y)|]
```

Type brackets

```haskell
foo :: $([t|Bool|]) -> a
```

A quoted TH name from a type name

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/412
data (-)

q = ''(-)
```

Quoted list constructors

```haskell
cons = '(:)
```

Pattern splices

```haskell
f $pat = ()

g =
  case x of
    $(mkPat y z) -> True
    _ -> False
```

Typed splice

```haskell
foo = $$bar
```

## Comments

Double comments in a line

```haskell
f = undefined {- Comment 1 -} {- Comment 2 -} -- Comment 3
```

Comments within a declaration

```haskell
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
    , 3
    ]
    -- in the end of the function
  where
    alpha = alpha
    -- between alpha and beta
    beta = beta
    -- after beta

foo = 1 -- after foo

gamma = do
  delta
  epsilon
  -- in the end of a do-block 1

gamma = do
  delta
  epsilon
  -- the very last block is detected differently
```

Doesn't work yet (wrong comment position detection)

```haskell pending
gamma = do
  -- in the beginning of a do-block
  delta
  where
    -- before alpha
    alpha = alpha
```

Comments in a class declaration

```haskell
class Foo a
    -- A comment
  where
  foo :: a -> Int
```

Comments in a class instance

```haskell
instance Pretty MatchForCase
  -- TODO: Do not forget to handle comments!
                                             where
  pretty' = undefined
```

Haddock comments

```haskell
-- | Module comment.
module X where

-- | Main doc.
main :: IO ()
main = return ()

data X
  = X -- ^ X is for xylophone.
  | Y -- ^ Y is for why did I eat that pizza.

data X =
  X
    { field1 :: Int -- ^ Field1 is the first field.
    , field11 :: Char
      -- ^ This field comment is on its own line.
    , field2 :: Int -- ^ Field2 is the second field.
    , field3 :: Char -- ^ This is a long comment which starts next to
      -- the field but continues onto the next line, it aligns exactly
      -- with the field name.
    , field4 :: Char
      -- ^ This is a long comment which starts on the following line
      -- from from the field, lines continue at the sme column.
    }

foo ::
     String -- ^ Reason for eating pizza.
  -> Int -- ^ How many did you eat pizza?
  -> String -- ^ The report.
foo = undefined
```

Module header with haddock comments

```haskell
-- | A module
module HIndent -- Foo
  ( -- * Formatting functions.
    reformat
  , -- * Testing
    test
  ) where
```

Comments around regular declarations

```haskell
-- This is some random comment.
-- | Main entry point.
main = putStrLn "Hello, World!"
-- This is another random comment.
```

Multi-line comments

```haskell
bob {- after bob -}
 =
  foo {- next to foo -}
  {- line after foo -}
    (bar
       foo {- next to bar foo -}
       bar {- next to bar -}
     ) {- next to the end paren of (bar) -}
    {- line after (bar) -}
    mu {- next to mu -}
    {- line after mu -}
    {- another line after mu -}
    zot {- next to zot -}
    {- line after zot -}
    (case casey {- after casey -}
           of
       Just {- after Just -}
        -> do
         justice {- after justice -}
          *
           foo
             (blah * blah + z + 2 / 4 + a - {- before a line break -}
              2 * {- inside this mess -}
              z /
              2 /
              2 /
              aooooo /
              aaaaa {- bob comment -}
              ) +
           (sdfsdfsd fsdfsdf) {- blah comment -}
         putStrLn "")
    [1, 2, 3]
    [ 1 {- foo -}
    , ( 2 {- bar -}
      , 2.5 {- mu -}
       )
    , 3
    ]

foo = 1 {- after foo -}
```

Multi-line comments with multi-line contents

```haskell
{- | This is some random comment.
Here is more docs and such.
Etc.
-}
main = putStrLn "Hello, World!"
{- This is another random comment. -}
```

Comments on functions in where clause

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/540
topLevelFunc1 = f
  where
    -- comment on func in where clause
    -- stays in the where clause
    f = undefined

topLevelFunc2 = f . g
    -- Another comment
  where
    {- multi
       line
       comment -}
    f = undefined -- single line comment
    -- single line comment
        -- Different size of indent
    g :: a
    g = undefined
```

Comments in a 'where' clause

```haskell
foo = undefined
  where
    bar
      -- A comment
     = undefined
      where
        a = b
    baz = undefined
```

Haddocks around data constructors

```haskell
data Foo
    -- | A haddock comment for 'Bar'.
  = Bar
    -- | A haddock comment for 'Baz'.
  | Baz
    -- | A haddock comment for 'Quuz'.
  | Quuz
```

## Identifiers

Unicode

```haskell
α = γ * "ω"
-- υ
```

`rec` and `mdo` are valid identifiers unless `RecursiveDo` is enabled

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/328
rec = undefined

mdo = undefined
```

The first character of an infix operator can be `@` unless `TypeApplications` is enabled.

```haskell
-- https://github.com/mihaimaruseac/hindent/issues/421
a @: b = a + b

main = print (2 @: 2)
```

## Complex input

A complex, slow-to-print decl

```haskell
quasiQuotes =
  [ ( ''[]
    , \(typeVariable:_) _automaticPrinter ->
        (let presentVar = varE (presentVarName typeVariable)
          in lamE
               [varP (presentVarName typeVariable)]
               [|(let typeString = "[" ++ fst $(presentVar) ++ "]"
                   in ( typeString
                      , \xs ->
                          case fst $(presentVar) of
                            "GHC.Types.Char" ->
                              ChoicePresentation
                                "String"
                                [ ( "String"
                                  , StringPresentation
                                      "String"
                                      (concatMap
                                         getCh
                                         (map (snd $(presentVar)) xs)))
                                , ( "List of characters"
                                  , ListPresentation
                                      typeString
                                      (map (snd $(presentVar)) xs))
                                ]
                              where getCh (CharPresentation "GHC.Types.Char" ch) =
                                      ch
                                    getCh (ChoicePresentation _ ((_, CharPresentation _ ch):_)) =
                                      ch
                                    getCh _ = ""
                            _ ->
                              ListPresentation
                                typeString
                                (map (snd $(presentVar)) xs)))|]))
  ]
```

Random snippet from hindent itself

```haskell
exp' (App _ op a) = do
  (fits, st) <- fitsOnOneLine (spaced (map pretty (f : args)))
  if fits
    then put st
    else do
      pretty f
      newline
      spaces <- getIndentSpaces
      indented spaces (lined (map pretty args))
  where
    (f, args) = flatten op [a]
    flatten :: Exp NodeInfo -> [Exp NodeInfo] -> (Exp NodeInfo, [Exp NodeInfo])
    flatten (App _ f' a') b = flatten f' (a' : b)
    flatten f' as = (f', as)
```

Quasi quotes

```haskell
exp = [name|exp|]

f [qq|pattern|] = ()
```

## C preprocessor

Conditionals (`#if`)

```haskell
isDebug :: Bool
#if DEBUG
isDebug = True
#else
isDebug = False
#endif
```

Macro definitions (`#define`)

```haskell
#define STRINGIFY(x) #x
f = STRINGIFY (y)
```

Escaped newlines

```haskell
#define LONG_MACRO_DEFINITION \
  data Pair a b = Pair \
    { first :: a \
    , second :: b \
    }
#define SHORT_MACRO_DEFINITION \
  x
```
