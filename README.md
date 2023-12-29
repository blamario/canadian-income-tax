Canadian Income Tax
===================

## Manual ##

This Haskell package consists of a library and executable to complete the Canadian T1 tax form. The way to use it is
as follows:

1. Download the fillable PDF forms from [the canada.ca Web
site](https://www.canada.ca/en/revenue-agency/services/forms-publications/tax-packages-years/general-income-tax-benefit-package.html).

2. Fill in the downloaded T1 form.
* If you're a resident of British Columbia or Ontario, you can also fill in the 428 and 479 forms for the province.
* If you're a resident of Alberta or Manitoba, you can also fill in the 428 form for the province.
* For all the other provinces only the T1 form is getting completed at this time.

In any case, don't bother filling in any fields that are calculated from other fields in the supported forms, that
part will be performed automatically.

3. Save the filled-in PDF form(s).

4. Run
       complete-canadian-taxes <province code> --t1 50??-r-fill-22e.pdf -o completed/

where `<province code>` is the two-letter code of the province or territory (AB, BC, QC, etc), `50??-r-fill-22e.pdf`
is the T1 form file you previously saved and `completed/` is the name of the output directory; feel free to change the
names. If applicable, you can also supply the 428 and 479 form with the command-line options of the same name. For
example, to complete all three forms for Ontario (ON) taxes the full command line would be

       complete-canadian-taxes ON --t1 5006-r-fill-22e.pdf --428 5006-c-fill-22e.pdf --479 5006-tc-fill-22e.pdf -o completed/

The executable `complete-canadian-taxes` automatically invokes `pdftk` to convert between PDF and FDF files. It needs
to be installed and in the executable path. If you'd prefer to handle the conversions manually, you can replace step 4
with the following procedure:

4a. Run

       pdftk 5006-r-fill-22e.pdf generate_fdf output 5006-r-fill-22e-filled.fdf

where `5006-r-fill-22e.pdf` is the file you previously saved and `5006-r-fill-22e-filled.fdf` is the name of the
output file; feel free to change them.

4b. Run

       complete-canadian-taxes <province code> --t1 5006-r-fill-22e-filled.fdf -o completed/

to complete all the field calculations and store the result in the output file
   `completed/5006-r-fill-22e-filled.fdf`.

4c. Run

       pdftk 5006-r-fill-22e.pdf fill_form completed/5006-r-fill-22e-filled.fdf output 5006-r-fill-22e-completed.pdf

to transfer the FDF field values from the previous step to the new PDF file, `5006-r-fill-22e-completed.pdf`.

Either way, at this point my job is done. The rest is all in your hands:

5. Carefully examine the completed PDF forms. The executable comes with no warranty and has not been verified by
CRA. The responsibility for the correctness of the tax return is still yours. If you notice any problem in the way the
forms were completed please report the issue. If the completion was correct but you want to change some field values,
you can repeat steps 2-5 to re-complete the forms with the modified values.

6. Once you're satisfied with the forms: print and sign the tax return, then send it to CRA by mail along with your
other documents. At some point they'll hopefully leave the 19th century and let us digitally file the same information
they accept on paper, but their NETFILE protocol is so far not open to the public.

## Installation ##

As you can see from the above instructions, you'll need to install the free `pdftk` executable to deal with
PDF <-> FDF conversion. See [their instructions](https://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/). On
Ubuntu you can simply run

    sudo apt install pdftk

while on Fedora the invocation is

    sudo dnf install pdftk-java

To install the `complete-canadian-taxes` executable, you'll need Haskell development tools. The simplest
procedure overall is to install [`ghcup`](https://www.haskell.org/ghcup/), then use it to install `ghc`
(version 9.4 or greater) and `cabal`, then to run

    cabal install canadian-income-tax

## Design notes and hints ##

The executable `complete-canadian-taxes` follows the Unix philosophy of doing one well-defined task and no more. It
doesn't attempt to guide you, the user, through the entire process of filing the taxes. It merely performs the task
that is easy to automate, and therefore also the most boring. It's not likely to make the tax-filing process a joy,
but it should at least reduce the drudgery.

If at any point you find you made a mistake in your initial form entry, or you want to adjust it for any reason, you
can make the adjustments either on the forms you kept from step #2 or on the final forms, and feed them back in
step #3. The executable will overwrite all calculated fields with theit proper values.

The FDF files are almost (but not quite) text files, which makes them `diff`able. You can use this to quickly compare
the effects of different changes without eyeballing through the PDF forms.
