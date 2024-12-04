## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for MBOA Analysis - Cefero
## This source code is licensed under the MIT license found in the
## LICENSE file in the root directory of this repository.
## Copyright 2024 Cefero
## Web Page: www.cefero.nl
## Contact: corneel@cefero.nl
##
##' *INFO*:
## 1) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# TODO I added inst/WORDLIST but casing remains an issue
# devtools::spell_check() #

devtools::load_all()
devtools::document()
quarto_render("how_to.qmd")
check_result <- devtools::check()
print(check_result)
