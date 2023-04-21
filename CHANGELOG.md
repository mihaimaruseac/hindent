# Changelog

## [Unreleased]

### Added

- Support for GHC 9.6 ([#699])

### Changed

- ...

### Fixed

- Fixed module names being removed from uses of qualified `do` ([#696]).
- Misplaced haddocks for class declarations ([#706]).

### Removed

- ...

## [6.0.0] - 2023-02-20

### Added

- The getConfig function is exported.

### Changed

- Switched the parser from [`haskell-src-exts`] to [`ghc-lib-parser`].
  This switch causes changes in the formatting results in some cases.
- Changed how to format a data type with a record constructor to follow
  the [Johan Tibell's Haskell Style Guide]. ([#662]).
- A newline is no longer inserted after a pattern signature ([#663]).
- A type with many type applications are now broken into multiple lines.
  ([#664]).
- A long type-level list is broken into multiple lines. ([#665]).
- Spaces around typed expression brackets are removed ([#666]).
- HIndent no longer breaks short class constraints in function signautres into
  multiple lines ([#669]).

### Fixed

- Fixed the wrong formatting of data family instances inside class instances
  ([#667]).
- Fixed the bug of removing the space before the enclosing parenthesis of a
  record syntax in a signature in a GADT declaration ([#670]).
- Fixed the bug of inserting unnecessary empty lines if a file contains only
  comments ([#672]).

### Removed

- Test functions except `testAst`.
- Atom support ([#671]).

## [5.3.4] - 2022-07-07

This version is accidentally pushlished, and is the same as 5.3.3.

## [5.3.3] - 2022-07-07

### Added

- Support for GHC 9.2.2.
- Test CI with GitHub Actions (WIP).

### Fixed

- Fixed a broken link to Servant by [@mattfbacon] in [#579].
- Fixed a build with Cabal 3.6 by [@uhbif19] in [#584].
- Fixed a compile error for GHC 9.2.2 by [@toku-sa-n] in [#588].

## [5.3.2] - 2022-02-02

### Fixed

- `MonadFix` issues to support newer GHC versions.

## [5.3.1] - 2019-06-28

### Fixed

- Comment relocations in where clauses of top-level function declarations.

## [5.3.0] - 2019-04-29

### Added

- Allow batch processing of multiple files.
- You can now specify default extensions in the configuration file.

### Fixed

- Handle multiple deriving clauses in the `DerivingStrategies` scenario.
- Ignore non-files in `findCabalFiles`.
- Prevent HIndent from trying to open non-files when searching for `.cabal` files.
- Fix the bad output for `[p|Foo|]` pattern quasi-quotes.
- Fix pretty-printing of `'(:)`.
- Fix parsing C preprocessor line continuations.
- Add parentheses around symbols `(:|)` when required.
- Support `$p` pattern splices.
- Fix formatting of associated type families.
- Fix formatting of non-dependent record constructors.

## [5.2.7] - 2019-03-16

### Fixed

- A bug in the `-X` option

## [5.2.6] - 2019-03-16

### Changed

- Switched to [`optparse-applicative`].

## [5.2.5] - 2018-01-04

### Added

- Support getting extensions from a `.cabal` file

### Changed

- Improved the indentation with record constructions and updates
- Updated [`haskell-src-exts`] dependency to version `>= 1.20.0`

### Fixed

- Fix the `let ... in` bug
- Fix formatting top-level lambda expressions in `TemplateHaskell` slices

## [5.2.4] - 2017-10-20

### Added

- Improved pretty-printing unboxed tuples.
- Support the `--validate` option for checking the format without reformatting
- Support parsing `#include`, `#error`, and `#warning` directives.
- Support reading `LANGUAGE` pragmas and parse the declared extensions from sources.
- Support the `EmptyCases` extension
- Optimize pretty-printing for many functional dependencies.

### Changed

- The `TypeApplications` extension is now disabled-by-default due to the `@` symbol

### Fixed

- Fixed pretty-printing imports
- Fixed pretty-printing string literals for `DataKinds`
- Fixed many issues related to infix operators including TH name quotes,
  `INLINE`/`NOINLINE` pragmas, infix type operators, and infix constructors.
- Fix extra linebreaks after short identifiers.

## [5.2.3] - 2017-05-07

### Added

- HIndent now reports where a parse happened.
- Added `--line-breaks` parameter to support custom line breaks.
- Added the `RecStmt` support

### Changed

- Explicit import lists are now sorted.
- Improved pretty-printing long type signatures.
- Improved pretty-printing GADT records.
- Improved pretty-printing data declaration records.
- The `RecursiveDo` and `DoRec` extensions are now disabled-by-default.

## Fixed

- Fixed pretty-printing for infix data constructors.
- Fixed pretty-printing of quasi-quoter names.
- Fixed pretty-printing of complicated type aliases.
- Fixed pretty-printing of complicated type signatures.

## [5.2.2] - 2017-01-16

### Changed

- HIndent now leaves `do`, lambda (`\x->`), and lambda-case (`\case->`) on the previous line of `$`.

### Fixed

- Fixed pretty-printing of parallel list comprehensions.
- Miscellaneous fixes.

## [5.2.1] - 2016-09-01

### Changed

- The `--tab-size` option is renamed to `--indent-size`.
- The `PatternSynonyms` extension is now disabled by default.
- HIndent now puts a newline before the closing bracket on a list.

### Fixed

- Fixed handling of paragraph overhang when using large constraints.
- Fixed pretty-printing of multi-line comments.
- Fixed bug resulting in adding a spurious space for comments at the end of a file.
- Fixed bug which results in adding a trailing white space on `<-`

## [5.2.0] - 2016-08-30

### Added

- Support the `.hindent.yaml` file to specify alternative tab width and max
  column numbers.
- Implement the tab-size support in Emacs Lisp.

### Changed

- The default number of spaces for a tab is changed to 2.

## [5.1.1] - 2016-08-29

### Added

- Add shebang support (Fixes [#208]).
- Output the filename for parse errors (Fixes [#179]).
- Added a document of the `-X` option (Fixes [#212]).

### Changed

- HIndent now preserves spaces between groups of imports (Fixes [#200]).
- HIndent now preserves the last newline if the input ends with one (Fixes [#211]).
- HIndent now puts the last parenthesis of an export list on a new line (Fixes [#227]).

### Fixed

- Fixed pretty-printing explicit `forall`s in instances (Fixes [#218]).

## [5.1.0] - 2016-08-25

### Added

- Add `--tab-size` parameter to control indentation spaces.

### Changed

- Rewrote comment association for more reliability.

### Fixed

- Some miscellaneous bugs.

## [5.0.1] - 2016-08-20

### Added

- Made HIndent compatible with GHC 7.8 through GHC 8.0
- Added test suites and benchmarks in [`TESTS.md`] and [`BENCHMARKS.md`]

### Changed

- Re-implement using [`bytestring`] instead of [`text`]

## [5.0.0] - 2016-08-11

### Removed

- Support for styles

## [4.6.4] - 2016-07-15

### Changed

- The formatted file is now created by coping/deleting a file instead of renaming.

## [4.6.3] - 2016-04-18

### Added

- Accept a filename to reformat.

### Fixed

- Fixed the whole module printer.

## [4.5.4] - 2015-06-22

### Added

- Improvements to Tibell style.
- 6x speed up on rendering operators.

## [4.4.5] - 2015-11-10

### Fixed

- Fixed a bug in infix patterns.

## [4.4.2] - 2015-04-05

### Added

- Support for CPP.

### Fixed

- Bunch of Gibiansky style bugs.
- Tibell style bugs.

## [4.3.8] - 2015-02-06

### Fixed

- A bug in printing operators in statements.

[unreleased]: https://github.com/mihaimaruseac/hindent/compare/v6.0.0...HEAD
[6.0.0]: https://github.com/mihaimaruseac/hindent/compare/v5.3.4...v6.0.0
[5.3.4]: https://github.com/mihaimaruseac/hindent/compare/v5.3.3...v5.3.4
[5.3.3]: https://github.com/mihaimaruseac/hindent/compare/v5.3.2...v5.3.3
[5.3.2]: https://github.com/mihaimaruseac/hindent/compare/5.3.1...v5.3.2
[5.3.1]: https://github.com/mihaimaruseac/hindent/compare/5.3.0...5.3.1
[5.3.0]: https://github.com/mihaimaruseac/hindent/compare/5.2.7...5.3.0
[5.2.7]: https://github.com/mihaimaruseac/hindent/compare/5.2.6...5.2.7
[5.2.6]: https://github.com/mihaimaruseac/hindent/compare/5.2.5...5.2.6
[5.2.5]: https://github.com/mihaimaruseac/hindent/compare/5.2.4...5.2.5
[5.2.4]: https://github.com/mihaimaruseac/hindent/compare/5.2.3...5.2.4
[5.2.3]: https://github.com/mihaimaruseac/hindent/compare/5.2.2...5.2.3
[5.2.2]: https://github.com/mihaimaruseac/hindent/compare/5.2.1...5.2.2
[5.2.1]: https://github.com/mihaimaruseac/hindent/compare/5.2.0...5.2.1
[5.2.0]: https://github.com/mihaimaruseac/hindent/compare/5.1.1...5.2.0
[5.1.1]: https://github.com/mihaimaruseac/hindent/compare/5.1.0...5.1.1
[5.1.0]: https://github.com/mihaimaruseac/hindent/compare/5.0.1...5.1.0
[5.0.1]: https://github.com/mihaimaruseac/hindent/compare/5.0.0...5.0.1
[5.0.0]: https://github.com/mihaimaruseac/hindent/compare/4.6.4...5.0.0
[4.6.4]: https://github.com/mihaimaruseac/hindent/compare/4.6.3...4.6.4
[4.6.3]: https://github.com/mihaimaruseac/hindent/compare/4.6.2...4.6.3
[4.5.4]: https://github.com/mihaimaruseac/hindent/compare/4.5.3...4.5.4
[4.4.5]: https://github.com/mihaimaruseac/hindent/compare/4.4.4...4.4.5
[4.4.2]: https://github.com/mihaimaruseac/hindent/compare/4.4.1...4.4.2
[4.3.8]: https://github.com/mihaimaruseac/hindent/compare/4.3.7...4.3.8

[@mattfbacon]: https://github.com/mattfbacon
[@uhbif19]: https://github.com/uhbif19
[@toku-sa-n]: https://github.com/toku-sa-n

[#706]: https://github.com/mihaimaruseac/hindent/pull/706
[#699]: https://github.com/mihaimaruseac/hindent/pull/699
[#696]: https://github.com/mihaimaruseac/hindent/pull/696
[#672]: https://github.com/mihaimaruseac/hindent/pull/672
[#671]: https://github.com/mihaimaruseac/hindent/pull/671
[#670]: https://github.com/mihaimaruseac/hindent/pull/670
[#669]: https://github.com/mihaimaruseac/hindent/pull/669
[#667]: https://github.com/mihaimaruseac/hindent/pull/667
[#666]: https://github.com/mihaimaruseac/hindent/pull/666
[#665]: https://github.com/mihaimaruseac/hindent/pull/665
[#664]: https://github.com/mihaimaruseac/hindent/pull/664
[#663]: https://github.com/mihaimaruseac/hindent/pull/663
[#662]: https://github.com/mihaimaruseac/hindent/pull/662
[#588]: https://github.com/mihaimaruseac/hindent/pull/588
[#584]: https://github.com/mihaimaruseac/hindent/pull/584
[#579]: https://github.com/mihaimaruseac/hindent/pull/579
[#227]: https://github.com/mihaimaruseac/hindent/pull/227
[#218]: https://github.com/mihaimaruseac/hindent/pull/218
[#212]: https://github.com/mihaimaruseac/hindent/pull/212
[#211]: https://github.com/mihaimaruseac/hindent/pull/211
[#208]: https://github.com/mihaimaruseac/hindent/pull/208
[#200]: https://github.com/mihaimaruseac/hindent/pull/200
[#179]: https://github.com/mihaimaruseac/hindent/pull/179

[`haskell-src-exts`]: https://hackage.haskell.org/package/haskell-src-exts
[`ghc-lib-parser`]: https://hackage.haskell.org/package/ghc-lib-parser
[`optparse-applicative`]: https://hackage.haskell.org/package/optparse-applicative
[`bytestring`]: https://hackage.haskell.org/package/bytestring
[`text`]: https://hackage.haskell.org/package/text

[`TESTS.md`]: TESTS.md
[`BENCHMARKS.md`]: BENCHMARKS.md

[Johan Tibell's Haskell Style Guide]: https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
