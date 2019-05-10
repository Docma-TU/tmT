###########
###########
### tmT ###
###########
###########

##############
## Roxygen2 ##
##############
setwd("C:/Users/lkoppers/Documents/repos/tmT/")
setwd("/home/lars/Github/tmT")
## setwd("D:/DoCMA/tmT")
# setwd(rprojroot::find_root("README.md"))
library(roxygen2)
roxygenize(package.dir = ".")


setwd("C:/Users/koppers/Desktop")
system("Rcmd Rd2pdf C:/Users/koppers/Desktop/tmT")

##############
## testthat ##
##############
setwd("//STORE/koppers/Textmining/tmT/tests/testthat")
setwd("D:/DoCMA/tmT/tests/testthat")
setwd("/home/lars/Github/tmT/tests/testthat")
library(testthat)
setwd("//STORE/koppers/Textmining/tmT/tests")
setwd("/home/lars/Github/tmT/tests")
test_check("tmT")

###################
## build install ##
###################
setwd("C:/Users/koppers/Desktop")
Sys.time()
system("Rcmd build tmT --resave-data")
Sys.time()
system("Rcmd check tmT_0.1-4.tar.gz --as-cran")
Sys.time()


system("Rcmd INSTALL tmT_0.1.tar.gz")

#########################
## Install from github ##
#########################

devtools::install_github("DoCMA-TU/tmT")
library(tmT)

#############
#############
### tosca ###
#############
#############

##############
## Roxygen2 ##
##############
setwd("C:/Users/lkoppers/Documents/repos/tosca/")
setwd("/home/lars/Github/tosca")
## setwd("D:/DoCMA/tosca")
# setwd(rprojroot::find_root("README.md"))
library(roxygen2)
roxygenize(package.dir = ".")


setwd("C:/Users/koppers/Desktop")
system("Rcmd Rd2pdf C:/Users/koppers/Desktop/tosca")

##############
## testthat ##
##############
setwd("//STORE/koppers/Textmining/tosca/tests/testthat")
setwd("D:/DoCMA/tosca/tests/testthat")
setwd("/home/lars/Github/tosca/tests/testthat")
library(testthat)
setwd("//STORE/koppers/Textmining/tosca/tests")
setwd("/home/lars/Github/tosca/tests")
test_check("tosca")

###################
## build install ##
###################
setwd("C:/Users/koppers/Desktop")
setwd("/home/lars/Github")
Sys.time()
system("R CMD build tosca --resave-data")
Sys.time()
system("R CMD check tosca_0.1-2.tar.gz --as-cran")
Sys.time()


system("Rcmd INSTALL tosca_0.1-0.tar.gz")

#########################
## Install from github ##
#########################

devtools::install_github("DoCMA-TU/tosca")
library(tosca)
