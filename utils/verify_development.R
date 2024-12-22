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

if (!requireNamespace("renv")) {
    install.packages("renv")
    renv::init()
}

source("utils/manage_packages.R")

devtools::load_all()

Sys.setenv(R_CONFIG_ACTIVE = "default")

devtools::document()
source("utils/function_checker.R")
# TODO meerdere qmds opgeven
check_exported_function_usage("how_to.qmd")

#quarto_render("how_to.qmd")
check_result <- devtools::check()
print(check_result)
