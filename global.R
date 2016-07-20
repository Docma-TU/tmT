# setwd("//STORE/koppers/Textmining/tmT/")
##############
## Roxygen2 ##
##############

library(roxygen2)
roxygenize(package.dir = ".")

##############
## testthat ##
##############
# setwd("//STORE/koppers/Textmining/tmT/tests/testthat")
setwd("C:/Users/koppers/Desktop/tmT/tests")
library(testthat)
test_check("tmT")

###################
## build install ##
###################
# setwd("C:/Users/koppers/Desktop")
system("Rcmd check tmT")
system("Rcmd build tmT --resave-data")


system("Rcmd INSTALL tmT_0.1.tar.gz")
