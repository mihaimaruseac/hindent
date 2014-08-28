hindent
=====

Extensible Haskell pretty printer. Both a library and an
executable. Currently a work in progress (see
[FIXME items](https://github.com/chrisdone/hindent/blob/master/src/HIndent/Instances.hs)).

## Install

    $ cabal install hindent

## Usage

    $ hindent
    hindent: arguments: --style [fundamental|chris-done|michael-snoyman|johan-tibell]

## Example

Input code:

``` haskell
foo = foo (foo bar) (if bar then bob else pif)
   (case mu zot of Just x -> return (); Nothing -> return 1) bill
```

Fundamental style:

``` haskell
foo =
  foo
    (foo
       bar)
    (if bar
        then bob
        else pif)
    (case mu
            zot of
       Just x ->
         return
           ()
       Nothing ->
         return
           1)
    bill
```

Chris Done style (see [style guide](https://github.com/chrisdone/haskell-style-guide)):

``` haskell
foo =
  foo (foo bar)
      (if bar
          then bob
          else pif)
      (case mu zot of
         Just x -> return ()
         Nothing -> return 1)
      bill
```

## Emacs

In
[elisp/hindent.el](https://github.com/chrisdone/hindent/blob/master/elisp/hindent.el),
there is the function `hindent/reformat-decl` which you can run with
`M-x hindent/reformat-decl`. Or alternatively define a keybinding,
e.g.:

``` lisp
(define-key haskell-mode-map (kbd "C-c i") 'hindent/reformat-decl)
```

By default it uses the style called `fundamental`, if you want to use
another, like mine, `chris-done`, run `M-x customize-variable
hindent-style`. If you want to configure per-project, make a file
called `.dir-locals.el` in the project root directory like this:

``` lisp
((nil . ((hindent-style . "chris-done"))))
```

## Vim

No support yet. Patches welcome.

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
[HIndent.Combinators](https://github.com/chrisdone/hindent/blob/master/src/HIndent/Combinators.hs)
for defining printers. When you want to use a fundamental printer, use
`prettyInternal` instead of `pretty`.

If you want to contribute it to the package, add it to the list of
styles in
[HIndent](https://github.com/chrisdone/hindent/blob/master/src/HIndent.hs)
and export it, and open a pull request.
