Forms Data Format
=================

The Forms Data Format, or [FDF](https://helpx.adobe.com/acrobat/kb/acrobat-forms-form-data-web.html) for short, is a
horrible data format thought up by Adobe. Its only redeeming feature is that in practice it's much simpler than PDF.

This Haskell package is hacked together to parse and re-serialize *some* files in FDF format. It certainly does not
support the full range of possible FDF files. I tried to follow the specification but gave up.

The main purpose of the package is to allow parsing and serializing [Canadian tax
forms](https://github.com/blamario/canadian-income-tax) in FDF format.  For that purpose this hack seems
sufficient. If you find it doesn't work for some other FDF files, feel free to contribute some code.
