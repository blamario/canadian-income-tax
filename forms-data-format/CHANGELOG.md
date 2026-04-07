# Revision history for forms-data-format

## 0.4 -- 2026-03-18

* Added module `Text.FDF.PDF` with `parsePDF` and `fillPDF` functions for
  converting between PDF AcroForm fields and FDF format, removing the need
  for the external `pdftk` tool
* Added executables `pdf-to-fdf` and `fdf-to-pdf`
* Added `cabal.project` to allow building with GHC 9.6+ (relaxes overly
  strict upper bounds on `base` in transitive dependencies)

## 0.1 -- 2023-04-03

* First version. Released on an unsuspecting world.

## 0.2 -- 2023-04-09

* Removed an extra newline from reserialization.

## 0.2.0.1 -- 2023-11-25

* Bumped the upper bound of the `bytestring` dependency.

## 0.2.1 -- 2024-02-29

* Properly parse and serialize special and Unicode characters

## 0.3 -- 2025-02-15

* Changed the `Field` data type, as it appears to never have a value and children at the same time
* Added executables `fdfdiff` and `fdfpatch`
