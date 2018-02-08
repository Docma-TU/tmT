##############
## Roxygen2 ##
##############
setwd("//STORE/koppers/Textmining/tmT/")
setwd("/home/lars/Github/tmT")
## setwd("D:/DoCMA/tmT")
# setwd(rprojroot::find_root("README.md"))
library(roxygen2)
roxygenize(package.dir = ".")


system("Rcmd Rd2pdf C:/Users/koppers/Desktop/tmT")

##############
## testthat ##
##############
setwd("//STORE/koppers/Textmining/tmT/tests/testthat")
setwd("D:/DoCMA/tmT/tests/testthat")
setwd("/home/lars/Github/tmT/tests/testthat")
library(testthat)
setwd("//STORE/koppers/Textmining/tmT/tests")
test_check("tmT")

###################
## build install ##
###################
setwd("C:/Users/koppers/Desktop")
system("Rcmd check tmT --as-cran")
system("Rcmd build tmT --resave-data")


system("Rcmd INSTALL tmT_0.1.tar.gz")

#########################
## Install from github ##
#########################

library(devtools)
install_github("DoCMA-TU/tmT")
library(tmT)

