# hindent [![Hackage](https://img.shields.io/hackage/v/hindent.svg?style=flat)](https://hackage.haskell.org/package/hindent) ![CI (Stack)](https://github.com/mihaimaruseac/hindent/workflows/CI%20(Stack)/badge.svg) ![CI (Cabal)](https://github.com/mihaimaruseac/hindent/workflows/CI%20(Cabal)/badge.svg) [![OpenSSF Scorecard](https://api.securityscorecards.dev/projects/github.com/mihaimaruseac/hindent/badge)](https://api.securityscorecards.dev/projects/github.com/mihaimaruseac/hindent)


Haskell pretty printer

[Examples](https://github.com/mihaimaruseac/hindent/blob/master/TESTS.md)

## Install

    $ stack install hindent

## Usage

    $ hindent --help
    hindent --version --help --style STYLE --line-length <...> --indent-size <...> --no-force-newline [-X<...>]* [<FILENAME>]
    Version 5.1.1
    Default --indent-size is 2. Specify --indent-size 4 if you prefer that.
    -X to pass extensions e.g. -XMagicHash etc.
    The --style option is now ignored, but preserved for backwards-compatibility.
    Johan Tibell is the default and only style.

hindent is used in a pipeline style

    $ cat path/to/sourcefile.hs | hindent

The default indentation size is `2` spaces. Configure indentation size with `--indent-size`:

    $ echo 'example = case x of Just p -> foo bar' | hindent --indent-size 2; echo
    example =
      case x of
        Just p -> foo bar
    $ echo 'example = case x of Just p -> foo bar' | hindent --indent-size 4; echo
    example =
        case x of
            Just p -> foo bar

## Customization

Create a `.hindent.yaml` file in your project directory or in your
`~/` home directory. The following fields are accepted and are the
default:

``` yaml
indent-size: 2
line-length: 80
force-trailing-newline: true
line-breaks: [":>", ":<|>"]
extensions:
  - DataKinds
  - GADTs
  - TypeApplications
```

By default, hindent preserves the newline or lack of newline in your input. With `force-trailing-newline`, it will make sure there is always a trailing newline.

hindent can be forced to insert a newline before specific operators and tokens with `line-breaks`. This is especially useful when utilizing libraries like [`servant`](https://docs.servant.dev/) which use long type aliases.

Using `extensions`, hindent can be made aware of valid syntactic compiler extensions that would normally be considered invalid syntax.

It is also possible to specify which extensions HIndent runs
with in your `.hindent.yaml`:

```yaml
extensions:
  - MagicHash
  - RecursiveDo
```

## Emacs

In
[elisp/hindent.el](https://github.com/mihaimaruseac/hindent/blob/master/elisp/hindent.el)
there is `hindent-mode`, which provides keybindings to reindent parts of the
buffer:

- `M-q` reformats the current declaration.  When inside a comment, it fills the
  current paragraph instead, like the standard `M-q`.
- `C-M-\` reformats the current region.

To enable it, add the following to your init file:

```lisp
(add-to-list 'load-path "/path/to/hindent/elisp")
(require 'hindent)
(add-hook 'haskell-mode-hook #'hindent-mode)
```

## Vim

The `'formatprg'` option lets you use an external program (like
hindent) to format your text. Put the following line into
~/.vim/ftplugin/haskell.vim to set this option for Haskell files:

    setlocal formatprg=hindent

Then you can format with hindent using `gq`. Read `:help gq` and `help
'formatprg'` for more details.

Note that unlike in emacs you have to take care of selecting a
sensible buffer region as input to hindent yourself. If that is too
much trouble you can try
[vim-textobj-haskell](https://github.com/gilligan/vim-textobj-haskell)
which provides a text object for top level bindings.

In order to format an entire source file execute:

    :%!hindent

Alternatively you could use the
[vim-hindent](https://github.com/alx741/vim-hindent) plugin which runs hindent
automatically when a Haskell file is saved.

## Atom

Fortunately, you can use https://atom.io/packages/ide-haskell with the
path to hindent specified instead of that to stylish-haskell. Works
like a charm that way!

## IntelliJ / other JetBrains IDEs
1. Install the "HaskForce" Haskell plugin (this is so we get the language type recognized in the file watcher)
2. Install the "File Watchers" plugin under "Browse Repositories"
3. Add a File Watcher with 
    1. File type: Haskell Language
    2. Program: `/path/to/hindent`
    3. Arguments: `$FilePath$`
    4. Immediate file synchronization: off
    5. Show console: Error
<img src="https://i.imgur.com/gghTjjn.png" width="500">

Now whenever you save a file, `hindent` should autoformat it.
