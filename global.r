
##############
## Roxygen2 ##
##############

setwd("//STORE/koppers/Textmining/tmT")
library(roxygen2)
roxygenize(package.dir = ".")

##############
## testthat ##
##############

setwd("//STORE/koppers/Textmining/tmT/tests")
library(testthat)
test_check("tmT")
