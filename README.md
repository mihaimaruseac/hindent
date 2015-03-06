
# hindent [![Hackage](https://img.shields.io/hackage/v/hindent.svg?style=flat)](https://hackage.haskell.org/package/hindent) [![Build Status](https://travis-ci.org/chrisdone/hindent.png)](https://travis-ci.org/chrisdone/hindent)

Extensible Haskell pretty printer. Both a library and an
executable. Currently a work in progress (see
[FIXME items](https://github.com/chrisdone/hindent/blob/master/src/HIndent/Pretty.hs)).

[Documentation](http://chrisdone.github.io/hindent/)

## Install

    $ cabal install hindent

You might need to `cabal install happy` if haskell-src-exts complains.

## Usage

    $ hindent
    hindent: arguments: --style [fundamental|chris-done|johan-tibell|gibiansky]

## Emacs

In
[elisp/hindent.el](https://github.com/chrisdone/hindent/blob/master/elisp/hindent.el)
there is `hindent-mode`, which provides keybindings to reindent parts of the
buffer:

- `M-q` reformats the current declaration.  When inside a comment, it fills the
  current paragraph instead, like the standard `M-q`.
- `C-M-\` reformats the current region.

To enable it, add the following to your init file:

```lisp
(add-hook 'haskell-mode-hook #'hindent-mode)
```

By default it uses the style called `fundamental`, if you want to use
another, `johan-tibell`, run `M-x customize-variable
hindent-style`. If you want to configure per-project, make a file
called `.dir-locals.el` in the project root directory like this:

``` lisp
((nil . ((hindent-style . "johan-tibell"))))
```

## Vim

Basic support is provided through [vim/hindent.vim](https://github.com/chrisdone/hindent/blob/master/vim/hindent.vim),
which sets hindent as the formatter used by `gq` for haskell files. The formatting style
defaults to `fundamental` but can be configured by setting `g:hindent_style` to the desired style.

Note that unlike in emacs you have to take care of selecting a sensible buffer region as input to
hindent yourself. If that is too much trouble you can try [vim-textobj-haskell](https://github.com/gilligan/vim-textobj-haskell) which provides a text object for top level bindings.

## Contributing your own printer style

This package comes with a basic fundamental pretty printer, which is
probably not desirable to use.

It comes with other styles implemented on top of this fundamental
printer, in the modules in `HIndent.Styles.*`.

Make a module `HIndent.Styles.YourName` in which to place the printer.

To define your own, see
[HIndent.Styles.Fundamental](https://github.com/chrisdone/hindent/blob/master/src/HIndent/Styles/Fundamental.hs)
for a starting point. This module defines a blank style, adds no
additional extensions. Customizations are specified via the
`styleExtenders` property. See
[HIndent.Styles.ChrisDone](https://github.com/chrisdone/hindent/blob/master/src/HIndent/Styles/ChrisDone.hs)
for an example of a non-trivial style.

Useful combinators can be found in
[HIndent.Pretty](https://github.com/chrisdone/hindent/blob/master/src/HIndent/Pretty.hs)
for defining printers. When you want to use a fundamental printer, use
`prettyNoExt` instead of `pretty`. Comments will still be inserted by
`prettyNoExt`.

If you want to contribute it to the package, add it to the list of
styles in
[HIndent](https://github.com/chrisdone/hindent/blob/master/src/HIndent.hs)
and export it, and open a pull request. Use
[the Erlang git commit guide](https://github.com/erlang/otp/wiki/Writing-good-commit-messages)
for your commits.

## Remaining issues

* Add test suite.
* Flesh out more obscure parts of the AST.
* Improve comment re-insertion.
* Possibly: Support formatting whole modules.
* Implement some operator-specific layouts: e.g.

        Foo <$> foo
            <*> bar
            <*> mu

## Example

Input code:

``` haskell
foo = do print "OK, go"; foo (foo bar) -- Yep.
          (if bar then bob else pif) (case mu {- cool -} zot of
            Just x -> return (); Nothing -> do putStrLn "yay"; return 1) bill -- Etc
  where potato Cakes {} = 2 * x foo * bar / 5
```

### Fundamental

This is an intentionally very dumb style that demands extension.

``` haskell
foo =
  do print
       "OK, go"
     foo
       (foo
          bar)
       (if bar
           then bob
           else pif)
       (case mu {- cool -}
               zot of
          Just x ->
            return
              ()
          Nothing ->
            do putStrLn
                 "yay"
               return
                 1)
       bill -- Etc
  where potato Cakes{} =
          2 * x
                foo * bar / 5
```

### Johan Tibell

Documented in
[the style guide](https://github.com/tibbe/haskell-style-guide).
This printer style uses some simple heuristics in deciding when to go
to a new line or not, and custom handling of do, if, case alts, rhs,
etc.

``` haskell
foo = do
    print "OK, go"
    foo
        (foo bar)
        (if bar
             then bob
             else pif)
        (case mu {- cool -} zot of
             Just x ->
                 return ()
             Nothing -> do
                 putStrLn "yay"
                 return 1)
        bill -- Etc
  where
    potato Cakes{} =
        2 * x foo * bar / 5
```

### Chris Done

My style is documented in
[the style guide](https://github.com/chrisdone/haskell-style-guide).
This printer style uses some simple heuristics in deciding when to go
to a new line or not.

``` haskell
foo =
  do print "OK, go"
     foo (foo bar)
         (if bar
             then bob
             else pif)
         (case mu {- cool -} zot of
            Just x -> return ()
            Nothing ->
              do putStrLn "yay"
                 return 1)
         bill -- Etc
  where potato Cakes{} = 2 * x foo * bar / 5
```

### Andrew Gibiansky

``` haskell
foo = do
  print "OK, go"
  foo (foo bar) -- Yep.
   (if bar
       then bob
       else pif) (case mu {- cool -} zot of
                    Just x -> return ()
                    Nothing -> do
                      putStrLn "yay"
                      return 1) bill -- Etc

  where
    potato Cakes{} = 2 * x foo * bar / 5
```
