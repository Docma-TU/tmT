
##############
## Roxygen2 ##
##############

library(roxygen2)
roxygenize(package.dir = ".")

##############
## testthat ##
##############

library(testthat)
test_check("tmT")
