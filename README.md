Canadian Income Tax
===================

### Use ###

This Haskell package consists of a library and executable to fill out the Canadian T1 tax form. The way to
use it is as follows:

1. Download the fillable PDF form from [the canada.ca Web
site](https://www.canada.ca/en/revenue-agency/services/forms-publications/tax-packages-years/general-income-tax-benefit-package/ontario/5006-r.html).

2. Fill out the form; don't bother filling out any fields that are calculated from other fields in the same
form, that part will be performed automatically.

3. Save the filled-out PDF form.

4. Run

       pdftk 5006-r-fill-22e.pdf generate_fdf output 5006-r-fill-22e.fdf

where `5006-r-fill-22e.pdf` is the file you previously saved and `5006-r-fill-22e.pdf` is the name of the
output file; feel free to change them.

5. Run

       complete-canadian-t1-form 5006-r-fill-22e.fdf -o 5006-r-fill-22e-filled.fdf

where `5006-r-fill-22e-filled.fdf` is the output FDF file name with all calculated fields automatically
filled in.

6. Run

       pdftk 5006-r-fill-22e.pdf fill_form 5006-r-fill-22e-filled.fdf output 5006-r-fill-22e-filled.pdf

to transfer the FDF field values from the previous step to the new PDF file, `5006-r-fill-22e-filled.pdf`.

7. Verify the final PDF file. The executable comes with no warranty and has not been approved by CRA. The
reponsibility for its correctness is still yours.

8. Print and sign the tax return, then send it to CRA by mail along with your documents. At some point
they'll hopefully open a way to digitally file the same information they accept on paper, but their EFILE
protocol is so far not open to the public.


### Installation ###

As you can see from the above instructions, you'll need to install the free `pdftk` executable to deal with
PDF <-> FDF conversion. See [their instructions](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/). On
Ubuntu you can simply run

    sudo apt install pdftk

To install the `complete-canadian-t1-form` executable, you'll need Haskell development tools. The simplest
procedure overall is to install [`ghcup`](https://www.haskell.org/ghcup/), then use it to install `ghc`
(version 9.2 or greater) and `cabal`, then to run

    cabal install canadian-income-tax
