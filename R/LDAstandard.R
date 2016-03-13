LDAstandard <-
function(documents,K=100,vocab, num.iterations=200, burnin=70, alpha=0.1, eta=0.1, seed, folder, num.words=50, LDA=TRUE){
    if(LDA){set.seed(seed)
result <- lda.collapsed.gibbs.sampler(documents=documents, K=K, vocab=vocab,
                                      num.iterations = num.iterations,
                                      burnin = burnin,
                                      alpha = alpha,
                                      eta = eta,
                                      compute.log.likelihood=TRUE)
ldaID <- names(documents)
save(list=c("result","ldaID"),file=paste(folder, "-k", K, "i", num.iterations, "b", burnin, "s", seed, ".Rdata", sep=""))}else{
load(paste(folder, "-k", K, "i", num.iterations, "b", burnin, "s", seed, ".Rdata", sep=""))
}
write.csv(top.topic.words(result$topics, num.words=num.words, by.score=TRUE),file=paste(folder, "-k", K, "i", num.iterations, "b", burnin, "s", seed, ".csv", sep=""))
}
