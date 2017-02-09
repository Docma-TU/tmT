##############
## Roxygen2 ##
##############
setwd("//STORE/koppers/Textmining/tmT/")
setwd("D:/DoCMA/tmT")
library(roxygen2)
roxygenize(package.dir = ".")

##############
## testthat ##
##############
setwd("//STORE/koppers/Textmining/tmT/tests/testthat")
setwd("D:/DoCMA/tmT/tests/testthat")
library(testthat)
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
dev_mode(on=T)
install_github("DoCMA-TU/tmT")
library(tmT)
dev_mode(on=F)

