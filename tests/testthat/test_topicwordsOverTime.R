context("topicwordsOverTime")

test_that("topicwordsOverTime", {

    set.seed(24601)
    x1 <- matrix(sample(c(rep(0,20),1:20),10000, replace=TRUE),10,1000)
    ldaID <- paste("ID", 1:1000)
    meta1 <- as.Date(sample(1:730, 1200, replace=TRUE), origin="1990-10-03")
    names(meta1) <- paste("ID", 1:1200)
    x2 <- list(document_sums=x1)
    meta2 <- data.frame(id=paste("ID", 1:1200), date=meta1, stringsAsFactors=FALSE)

    text <- matrix(sample(paste("word", 1:100), 10000, replace=TRUE), 200,50)
    text <- apply(text,1,list)
    names(text) <- paste("ID", 101:300)

    wordlist <- makeWordlist(text)
    LDAdoc <- docLDA(text, wordlist$words)
    lda <- LDAstandard(documents=LDAdoc, K = 3L, vocab=wordlist$words, num.iterations = 20L, burnin = 70L, seed=24601, folder=paste0(getwd(),"/twOt"))

    meta1 <- as.Date(sample(1:730, 1200, replace=TRUE), origin="1990-10-03")
    names(meta1) <- paste("ID", 1:1200)
    meta2 <- data.frame(id=paste("ID", 1:1200), date=meta1, stringsAsFactors=FALSE)
    
    
    
    twOt1 <- topicwordsOverTime(ldaCorpus=LDAdoc, meta=meta1, ldaVocab=wordlist$words, ldaResult=lda, words=paste("word", c(1,3,8,55)), topics=c(2,2,5,7), file="test.pdf", ldaName="test")
    twOt2 <- topicwordsOverTime(ldaCorpus=LDAdoc, meta=meta2, ldaVocab=wordlist$words, ldaResult=lda, words=paste("word", c(1,3,8,55)), topics=c(2,2,5,7), file="test.pdf", ldaName="test", unit="week")
save(twOt1, twOt2, file="data/topicwordsOverTime.RData")
    ldaCorpus=LDAdoc; meta=meta2; ldaVocab=wordlist$words; ldaResult=lda; words=paste("word", c(1,3,8,55)); topics=c(2,2,5,7); file="test.pdf"; ldaName="test"

load("data/topicwordsOverTime.RData")
    expect_equal(twOt1, topicwordsOverTime(ldaCorpus=LDAdoc, meta=meta1, ldaVocab=wordlist$words, ldaResult=lda, words=paste("word", c(1,3,8,55)), topics=c(2,2,5,7), file="test.pdf", ldaName="test"))
    expect_equal(twOt2, topicwordsOverTime(ldaCorpus=LDAdoc, meta=meta2, ldaVocab=wordlist$words, ldaResult=lda, words=paste("word", c(1,3,8,55)), topics=c(2,2,5,7), file="test.pdf", ldaName="test", unit="week"))
})


