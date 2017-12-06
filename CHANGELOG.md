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
