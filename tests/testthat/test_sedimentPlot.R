context("sedimentPlot")

test_that("sedimentPlot", {

    set.seed(24601)
    x <- list(document_sums=matrix(sample(c(rep(0,20),1:20),10000, replace=TRUE),10,1000))
    ldaID <- paste("ID", 11:1010)
    meta1 <- data.frame(id=paste("ID", 1:1020), date=as.Date(sample(1:730, 1020, replace=TRUE), origin="1990-10-03"))

    ## sP1 <- sedimentPlot(x=x, ldaID=ldaID, usedTopics=NULL, label=NULL, threshold=NULL, meta=meta1, unit="month", xunit="year", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0)
    ## sP2 <- sedimentPlot(x=x, ldaID=ldaID, usedTopics=c(2,4,7), label=NULL, threshold=NULL, meta=meta1, unit="month", xunit="month", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0.1)
    ## sP3 <- sedimentPlot(x=x, ldaID=ldaID, usedTopics=NULL, label=NULL, threshold=0.14, meta=meta1, unit="month", xunit="month", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0.1)
    ## save(sP1, sP2, sP3, file="data/SedimentPlot.RData")

    load("data/SedimentPlot.RData")

    expect_equal(sP1, sedimentPlot(x=x, ldaID=ldaID, usedTopics=NULL, label=NULL, threshold=NULL, meta=meta1, unit="month", xunit="year", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0.08))
    expect_equal(sP2, sedimentPlot(x=x, ldaID=ldaID, usedTopics=c(2,4,7), label=NULL, threshold=NULL, meta=meta1, unit="month", xunit="month", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0.2))
    expect_equal(sP2, sedimentPlot(x=x, ldaID=ldaID, usedTopics=c("2", "4", "7"), label=as.character(1:10), threshold=NULL, meta=meta1, unit="month", xunit="month", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0))
    expect_equal(sP3, sedimentPlot(x=x, ldaID=ldaID, usedTopics=NULL, label=NULL, threshold=0.14, meta=meta1, unit="month", xunit="month", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0))
    expect_error(sedimentPlot(x=x, ldaID=paste("ID", 1011:2010), usedTopics=NULL, label=NULL, threshold=NULL, meta=meta1, unit="month", xunit="year", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0.2))
    
    })

