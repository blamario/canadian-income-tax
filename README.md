Canadian Income Tax
===================

## Manual ##

### Use for Canadians outside Ontario ###

This Haskell package consists of a library and executable to fill out the Canadian T1 tax form. The way to
use it is as follows:

1. Download the fillable PDF form from [the canada.ca Web
site](https://www.canada.ca/en/revenue-agency/services/forms-publications/tax-packages-years/general-income-tax-benefit-package/ontario/5006-r.html).

2. Fill in the downloaded T1 form; don't bother with any fields that are calculated from other fields in the same
form, that part will be performed automatically.

3. Save the filled-out PDF form.

4. Run

       pdftk 5006-r-fill-22e.pdf generate_fdf output 5006-r-fill-22e-filled.fdf

where `5006-r-fill-22e.pdf` is the file you previously saved and `5006-r-fill-22e-filled.fdf` is the name of the
output file; feel free to change them.

5. Run

       mkdir completed/
       complete-canadian-taxes --t1 5006-r-fill-22e-filled.fdf -o completed/

   to complete all the field calculations and store the result in the output file
   `completed/5006-r-fill-22e-filled.fdf`.

6. Run

       pdftk 5006-r-fill-22e.pdf fill_form completed/5006-r-fill-22e-filled.fdf output 5006-r-fill-22e-filled.pdf

to transfer the FDF field values from the previous step to the new PDF file, `5006-r-fill-22e-filled.pdf`.

7. Carefully examine the final PDF. The executable comes with no warranty and has not been verified by CRA. The
reponsibility for the correctness of the tax return is still yours.

8. Print and sign the tax return, then send it to CRA by mail along with your other documents. At some point they'll
hopefully leave the 19th century and let us digitally file the same information they accept on paper, but their EFILE
protocol is so far not open to the public.


### Use for Ontario tax returns ###

1. Download the fillable federal
[(T1)](https://www.canada.ca/en/revenue-agency/services/forms-publications/tax-packages-years/general-income-tax-benefit-package/ontario/5006-r.html)
and provincial
[(ON428)](https://www.canada.ca/en/revenue-agency/services/forms-publications/tax-packages-years/general-income-tax-benefit-package/ontario/5006-c.html)
PDF forms from [the canada.ca Web site](https://www.canada.ca/en/revenue-agency/services/forms-publications/tax-packages-years/general-income-tax-benefit-package/ontario.html)

2. Fill in the two downloaded forms; don't bother with any fields that are copied or calculated from other fields in
the two forms, that part will be performed automatically.

3. Save the filled-out PDF forms.

4. Run

       pdftk 5006-r-fill-22e.pdf generate_fdf output 5006-r-fill-22e-filled.fdf
       pdftk 5006-c-fill-22e.pdf generate_fdf output 5006-c-fill-22e-filled.fdf

where `5006-r-fill-22e.pdf` and `5006-c-fill-22e.pdf` are the two PDFs you previously saved and
`*-filled.fdf` are the names of the output FDF files; feel free to change them.

5. Run

       mkdir completed/
       complete-canadian-taxes --t1 5006-r-fill-22e-filled.fdf --on428 5006-c-fill-22e-filled.fdf -o completed/

   to complete all the field calculations and store the result in the output directory `completed/`.

6. Run

       pdftk 5006-r-fill-22e.pdf fill_form completed/5006-r-fill-22e-filled.fdf output completed/5006-r-fill-22e.pdf
       pdftk 5006-c-fill-22e.pdf fill_form completed/5006-c-fill-22e-filled.fdf output completed/5006-c-fill-22e.pdf

to transfer the FDF field values from the previous step to the new PDF files in the `completed/` directory.

7. Carefully examine the final PDF outputs. The executable comes with no warranty and has not been verified by
CRA. The reponsibility for the tax returns' correctness is still yours.

8. Print the PDFs, sign the tax return, then send it to CRA by mail along with the rest of your documents. At some
point they'll hopefully open a way to digitally file the same information they accept on paper, but their EFILE
protocol is so far not open to the public.

## Installation ##

As you can see from the above instructions, you'll need to install the free `pdftk` executable to deal with
PDF <-> FDF conversion. See [their instructions](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/). On
Ubuntu you can simply run

    sudo apt install pdftk

To install the `complete-canadian-t1-form` executable, you'll need Haskell development tools. The simplest
procedure overall is to install [`ghcup`](https://www.haskell.org/ghcup/), then use it to install `ghc`
(version 9.4 or greater) and `cabal`, then to run

    cabal install canadian-income-tax

## Design notes and hints ##

The executable `complete-canadian-taxes` follows the Unix philosphy of doing one well-defined thing. It doesn't
attempt to guide you, the user, through the entire process of filing the taxes. It merely performs the task that is
easy to automate, and therefore also the most boring. It's not likely to make the tax-filing process a joy, but at
least it's going to reduce the drudgery.

If at any point you find you made a mistake in your initial form entry, or you want to adjust it for any reason, you
can make the adjustments either on the forms you kept from step #2 or on the final forms, and feed them back in
step #3. The executable will overwrite all calculated fields with theit proper values.

The FDF files are almost text files, which makes them `diff`able. You can use this to quickly compare the effect of
different changes without eyeballing through the PDF forms.
