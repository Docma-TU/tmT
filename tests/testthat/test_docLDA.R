context("docLDA prepare data for LDA")

test_that("docLDA", {

text <- list(A=c("lorem","ipsum","dolor"),
B=c("ut","ut","ut","enim","ad","minim"),
C=c("lorem","ipsum","dolor","dolor","dolor","dolor","sit"))

A <- matrix(c(1,1,3,1,4,1),2,3)
B1 <- matrix(c(0,1,2,1,5,1,7,3),2,4)
B2 <- matrix(c(0,1,2,1,5,1,7,1,7,1,7,1),2,6)
C1 <- matrix(c(1,4,3,1,4,1,6,1),2,4)
C2 <- matrix(c(1,1,1,1,1,1,1,1,3,1,4,1,6,1),2,7)
D <- matrix(c(NA,1),2,1)

expect_equal(docLDA(corpus=c(D="",text, E=NULL), vocab=sort(unique(unlist(text))), ldacorrect=TRUE, excludeNA=TRUE),list(A=A, B=B2,C=C2))
expect_equal(docLDA(corpus=c(D="",text), vocab=sort(unique(unlist(text))), ldacorrect=FALSE, excludeNA=TRUE),list(A=A, B=B1,C=C1))
expect_equal(docLDA(corpus=c(D="",text), vocab=sort(unique(unlist(text))), ldacorrect=FALSE, excludeNA=FALSE),list(D=D,A=A, B=B1,C=C1))
})
