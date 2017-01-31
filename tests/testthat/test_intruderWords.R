context("intruderWords")

test_that("intruderWords", {

    ## library(dirmult)
    ## set.seed(24601)
    ## beta <- rbind(rdirichlet(n=10, alpha=rep(0.6,200)))
    ## colnames(beta) <- paste("Wort", 1:200)
    ## save(beta, iW, iWo, file="data/intruderWords.RData")

    load("data/intruderWords.RData")

    set.seed(24601)
    iW2 <- intruderWords(beta, byScore = TRUE, numTopwords = 10L, numIntruder = 1L:2L, numOutwords = 8L, noTopic=TRUE, printSolution = FALSE, oldResult=NULL)
    1 2 3
    6
    h
    1
    x
    q

    iWo2 <- intruderWords(beta, byScore = TRUE, numTopwords = 10L, numIntruder = 1L:2L, numOutwords = 8L, noTopic=TRUE, printSolution = FALSE, oldResult=iW)
    4 2
    1
    3
    2
    4
    2 1

    expect_equal(iW, iW2)
    expect_equal(iWo, iWo2)

})
