# For package development this code ensures there are no clock errors
.onLoad <- function(libname, pkgname) {
    Sys.setenv(R_CHECK_SYSTEM_CLOCK = 0)
}

source("utils/dev_functions.R")
source("utils/manage_packages.R")

load_all()

# TODO Change default in own config if needed
# Sys.setenv(R_CONFIG_ACTIVE = "default")
