#' Function to fit LDA model
#'
#' This function uses the \code{lda.collapsed.gibbs.sampler} from the LDA
#' package and additional saves topwordlists and a R workspace.
#'
#'
#' @param documents A list prepared by \code{\link{docLDA}}.
#' @param K number of topics.
#' @param vocab character vector containing the words in the corpus.
#' @param num.iterations number of iterations for the gibbs sampler.
#' @param burnin number of iterations for the burnin.
#' @param alpha Hyperparameter for the topic proportions.
#' @param eta Hyperparameter for the word distributions.
#' @param seed A seed for reproducability.
#' @param folder file for the results.
#' @param num.words number of words in the top topic words list.
#' @param LDA logical: Should a new model be fitted or a existing R workspace
#' be used?
#' @return A .csv containing the topword list and a R workspace containing the
#' result data.
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @seealso Documentation for the lda package.
#' @references Blei, David M. and Ng, Andrew and Jordan, Michael. Latent
#' Dirichlet allocation. Journal of Machine Learning Research, 2003.
#'
#' Jonathan Chang (2012). lda: Collapsed Gibbs sampling methods for topic
#' models.. R package version 1.3.2. http://CRAN.R-project.org/package=lda
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export LDAstandard
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
