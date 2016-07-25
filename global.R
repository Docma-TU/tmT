##############
## Roxygen2 ##
##############
setwd("//STORE/koppers/Textmining/tmT/")
library(roxygen2)
roxygenize(package.dir = ".")

##############
## testthat ##
##############
setwd("//STORE/koppers/Textmining/tmT/tests/testthat")
setwd("C:/Users/koppers/Desktop/tmT/tests")
library(testthat)
test_check("tmT")

###################
## build install ##
###################
setwd("C:/Users/koppers/Desktop")
system("Rcmd check tmT --as-cran")
system("Rcmd build tmT --resave-data")


system("Rcmd INSTALL tmT_0.1.tar.gz")
