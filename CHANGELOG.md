# Revision history for `canadian-income-tax`

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
