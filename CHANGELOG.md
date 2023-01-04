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

5.2.5:

    * Support get extensions from `.cabal` file
    * Improve indention with record constructions and updates
    * Fix `let ... in` bug
    * Fix top-level lambda expressions in TemplateHaskell slices
    * Update to haskell-src-exts dependency to version `>= 1.20.0`

5.2.4:

    * Pretty print imports
    * Fix pretty print for string literals for `DataKinds`
    * Support `--validate` option for checking the format without reformatting
    * Support parse `#include`, `#error`, `#warning` directives
    * Support read `LANGUAGE` pragma and parse the declared extensions from source
    * Treat `TypeApplications` extension as 'badExtensions' due to the `@` symbol
    * Improve pretty print for unboxed tuples
    * Fix many issues related to infix operators, includes TH name quotes,
      `INLINE`/`NOINLINE` pragmas, infix type operator and infix constructor
    * Fix pretty print for operators in `INLINE`/`NOINLINE` pragmas
    * Support for `EmptyCases` extension
    * Fix TH name quotes on operator names
    * Optimize pretty print for many fundeps
    * Fix extra linebreaks after short identifiers

5.2.3:

    * Sort explicit import lists
    * Report the `SrcLoc` when there's a parse error
    * Improve long type signatures pretty printing
    * Support custom line-break operators, add `--line-breaks` argument
    * Fix infix data constructor
    * Disable `RecursiveDo` and `DoRec` extensions by default
    * Add RecStmt support
    * Improve GADT records, data declaration records
    * Complicated type alias and type signatures pretty printing
    * Fix quasi-quoter names

5.2.2:

    * Parallel list comprehensions
    * Leave do, lambda, lambda-case on previous line of $
    * Misc fixes

5.2.1:

    * Fix hanging on large constraints
    * Render multi-line comments
    * Rename --tab-size to --indent-size
    * Don't add a spurious space for comments at the end of the file
    * Don't add trailing whitespace on <-
    * Disable PatternSynonyms
    * Put a newline before the closing bracket on a list

5.2.0:

    * Default tab-width is now 2
    * Supports .hindent.yaml file to specify alt tab-width and max
      column
    * Put last paren of export list on a new line
    * Implement tab-size support in Emacs Lisp

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

[@mattfbacon]: https://github.com/mattfbacon
[@uhbif19]: https://github.com/uhbif19
[@toku-sa-n]: https://github.com/toku-sa-n

[#588]: https://github.com/mihaimaruseac/hindent/pull/588
[#584]: https://github.com/mihaimaruseac/hindent/pull/584
[#579]: https://github.com/mihaimaruseac/hindent/pull/579

[`haskell-src-exts`]: https://hackage.haskell.org/package/haskell-src-exts
[`ghc-lib-parser`]: https://hackage.haskell.org/package/ghc-lib-parser
[`optparse-applicative`]: https://hackage.haskell.org/package/optparse-applicative
