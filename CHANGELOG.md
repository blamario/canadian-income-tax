# Revision history for `canadian-income-tax`

## 2024.1.0.1

* Bumped the upper `deep-transformations` dependency bounds

## 2024.1

* Library function behaviour changes:
  * `nonNegativeDifference` returns `Nothing` instead of `Just 0` if the difference is negative
  * Default to deducting all available RRSP contributions on Schedule 7
* Fixed the collection of the T4 line 16A
* Fixed compiler warnings
* Added command line option help
* Added logging to the web server

## 2024.0.0.1

* Bumped the upper `deep-transformations` dependency bounds, backport

## 2024.0

* Breaking changes to the library:
  * Support for `T4` slips required signature change to `completeForms`
  * Generalized the `totalOf` function
  * Renamed some `T1` fields
  * Added underscore before the long permanent line numbers
  * Updated for the 2024 tax return forms
* Library additions:
  * Support for federal schedule 8 and `T4` slips
  * Added functions `formFileNames`, `relevantFormKeys`, and `completeRelevantForms`
  * Added the `Year` field constructor
  * Added the `FormKey` and `InputForms` types
* Improvements to executables:
  * Both the command-line and the web server executable now automatically include and complete all supported federal
    schedule forms that are relevant, _i.e._ that affect `T1` in any way.
  * Added command-line options `--only-given` and `--keep-irrelevant` to override the new default behaviour
  * Added command-line options `--t4` and `--s8` for newly supported forms
  * Added the save & load ability to the web server
  * Added schedule 8 support to the web server
  * Added T4 support to the web server as both a PDF upload and overlay form input
* Fixes and maintenance:
  * Fixed the handling of PDF file names with spaces
  * Updated dependency bounds
  * Added more Haddock documentation
  * Added more regression tests
  * Fixed compilation with GHC 9.6.4
  * Fixed the `cabal check` warnings

## 2023.0

* Updated for the 2023 tax return forms
* Extended the T1 form support to all provinces and territories
* Accepting the 428 forms for AB, BC, and MB beside the earlier ON support
* Accepting the 479 forms for BC and ON
* Accepting the federal schedule forms 6, 7, 9, and 11
* Streamlined the command-line options
* Allowed working directly with PDFs on the command line, with PDFTk installed
* A simple web server with interactive UI
* Refactored most of the code
* Added test suites

## 2022.2.1

* Fixed a duplicate field name

## 2022.2.0.1

* Fixed the README

## 2022.2

* Expanded README and renamed the executable
* Added the Ontario 428 tax form
* Added `leastOf` and calculated line 31260 of `T1`
* Fixed the credits left out from page 4 of `T1`
* Tightened module exports
* Fixed a line missing from total
* Fixed the spouse and dependents calculations
* Fixed the bracket calculations
* Fixed the writing and parsing of `Amount` and `Percent` field values
* The executable works on either the `T1` or `ON428` form
* Factored out the utility functions from `Fix` modules to `Util`
* Made `Tax.FDF` independent of `T1`
* Moved the `FDF` module to share it
* Filled more Cabal fields

## 2022.1

* The very first version
