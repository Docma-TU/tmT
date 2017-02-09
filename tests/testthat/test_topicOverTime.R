context("topicOverTime")

test_that("topicOverTime", {

x1 <- matrix(sample(c(rep(0,20),1:20),10000, replace=TRUE),10,1000)
ldaID <- paste("ID", 1:1000)
meta1 <- as.Date(sample(1:730, 1200, replace=TRUE), origin="1990-10-03")
names(meta1) <- paste("ID", 1:1200)
x2 <- list(document_sums=x1)
meta2 <- data.frame(id=paste("ID", 1:1200), date=meta1, stringsAsFactors=FALSE)


## tOt1 <- topicOverTime(x=x1, ldaID, meta=meta1, file="test.pdf", Tnames=paste("Tnames",1:10), unit= "month")
## tOt2 <- topicOverTime(x=x1, ldaID, meta=meta1, file="test.pdf", unit= "month")
## tOt3 <- topicOverTime(x=x1, ldaID, meta=meta1, file="test.pdf", topicNo=c(1,3,9), Tnames=paste("Tnames",1:10), unit= "week")
## tOt4 <- topicOverTime(x=x1, ldaID, meta=meta1, file="test.pdf", topicNo=c(1,3,9), unit= "week")
## tOt5 <- topicOverTime(x=x2, ldaID, meta=meta2, file="test.pdf", topicNo=c(1,3,9), unit= "week")
## save(tOt1, tOt2, tOt3, tOt4, tOt5, file="data/topicOverTime.RData")

load("data/topicOverTime.RData")

expect_equal(tOt1, topicOverTime(x=x1, ldaID, meta=meta1, file="test.pdf", Tnames=paste("Tnames",1:10), unit= "month"))
expect_equal(tOt2, topicOverTime(x=x1, ldaID, meta=meta1, file="test.pdf", unit= "month"))
expect_equal(tOt3, topicOverTime(x=x1, ldaID, meta=meta1, file="test.pdf", topicNo=c(1,3,9), Tnames=paste("Tnames",1:10), unit= "week"))
expect_equal(tOt4, topicOverTime(x=x1, ldaID, meta=meta1, file="test.pdf", topicNo=c(1,3,9), unit= "week"))
expect_equal(tOt5, topicOverTime(x=x2, ldaID, meta=meta2, file="test.pdf", topicNo=c(1,3,9), unit= "week"))
})


