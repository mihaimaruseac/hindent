# Changelog

## [Unreleased]

### Added

- The getConfig function is exported.

### Changed

- Switched the parser from [`haskell-src-exts`] to [`ghc-lib-parser`].
  This switch causes changes in the formatting results in some cases.

### Removed

- Test functions except `testAst`.

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
- Support $p` pattern splices.
- Fix formatting associated type families.
- Fix formatting non-dependent record constructors.

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
- Support custom the `line-break` option.
- Added `--line-breaks` parameter.
- Added the `RecStmt` support

### Changed

- Explicit import lists are now sorted.
- Improved pretty-printing long type signatures.
- Improved pretty-printing GADT records.
- Improved pretty-printing data declaration records.
- The `RecursiveDo` and `DoRec` extensions are now disabled-by-default.

## Fixed

- Fixed pretty-printing infix data constructors.
- Fixed pretty-printing quasi-quoter names.
- Fixed pretty-printing complicated type aliases.
- Fixed pretty-printing complicated type signatures.

## [5.2.2] - 2017-01-16

### Changed

- HIndent now leaves do, lambda, and lambda-case on the previous line of `$`.

### Fixed

- Fixed pretty-printing parallel list comprehensions.
- Miscellaneous fixes.

## [5.2.1] - 2016-09-01

### Changed

- The `--tab-size` option is renamed to `--indent-size`.
- The `PatternSynonyms` extension is now disabled-by-default.
- HIndent now puts a newline before the closing bracket on a list.

### Fixed

- Fixed handling on large constraints.
- Fixed pretty-printing multi-line comments.
- Fixed adding a spurious space for comments at the end of a file.
- Fixed adding a trailing white space on `<-`

## [5.2.0] - 2016-08-30

### Added

- Support the `.hindent.yaml` file to specify alternative tab width and max
  column numbers.
- Implement the tab-size support in Emacs Lisp.

### Changed

- The default number of spaces for a tab is changed to 2.
- The last parenthesis of an export list is now put on a new line.

5.1.1:

    * Preserve spaces between groups of imports (fixes #200)
    * Support shebangs (closes #208)
    * Output filename for parse errors (fixes #179)
    * Input with newline ends with newline (closes #211)
    * Document -X (closes #212)
    * Fix explicit forall in instances (closes #218)
    * Put last paren of export list on a new line #227

5.1.0:

    * Rewrote comment association, more reliable
    * Added --tab-size flag for indentation spaces
    * Fixed some miscellaneous bugs

5.0.1:

    * Re-implement using bytestring instead of text
    * Made compatible with GHC 7.8 through to GHC 8.0
    * Added test suite and benchmarks in TESTS.md and BENCHMARKS.md

5.0.0:

	* Drop support for styles

4.6.4

	* Copy/delete file instead of renaming

4.4.6

	* Fix whole module printer
	* Accept a filename to reformat

4.4.5

	* Fix bug in infix patterns

4.4.2

	* Bunch of Gibiansky style fixes.
	* Support CPP.
	* Tibell style fixes.

4.3.8

	* Fixed: bug in printing operators in statements.

4.5.4

	* Improvements to Tibell style.
	* 6x speed up on rendering operators.

[unreleased]: https://github.com/mihaimaruseac/hindent/compare/v5.3.4...HEAD
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

[@mattfbacon]: https://github.com/mattfbacon
[@uhbif19]: https://github.com/uhbif19
[@toku-sa-n]: https://github.com/toku-sa-n

[#588]: https://github.com/mihaimaruseac/hindent/pull/588
[#584]: https://github.com/mihaimaruseac/hindent/pull/584
[#579]: https://github.com/mihaimaruseac/hindent/pull/579

[`haskell-src-exts`]: https://hackage.haskell.org/package/haskell-src-exts
[`ghc-lib-parser`]: https://hackage.haskell.org/package/ghc-lib-parser
[`optparse-applicative`]: https://hackage.haskell.org/package/optparse-applicative
