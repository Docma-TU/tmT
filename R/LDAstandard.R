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
LDAstandard <- function(documents, K = 100L, vocab, num.iterations = 200L,
                        burnin = 70L, alpha = NULL, eta = NULL, seed = NULL, folder = "lda-result",
                        num.words = 50L, LDA = TRUE){
    if(is.null(alpha)) alpha <- 1/K
    if(is.null(eta)) eta <- 1/K
    if(is.null(seed)) seed <- sample(1:10^8,1)
    stopifnot(is.list(documents), as.integer(K) == K, length(K) == 1,
              is.character(vocab), as.integer(num.iterations) == num.iterations,
              length(num.iterations) == 1, as.integer(burnin) == burnin,
              length(burnin) == 1, is.numeric(alpha), length(alpha) == 1,
              is.numeric(eta), length(eta) == 1, is.numeric(seed),
              length(seed) == 1, is.character(folder), length(folder) == 1,
              as.integer(num.words) == num.words, length(num.words) == 1,
              is.logical(LDA), length(LDA) == 1)
    if(LDA){
        set.seed(seed)
        result <- lda.collapsed.gibbs.sampler(documents = documents, K = K,
                                              vocab = vocab,
                                              num.iterations = num.iterations,
                                              burnin = burnin,
                                              alpha = alpha, eta = eta,
                                              compute.log.likelihood = TRUE)
        ldaID <- names(documents)
        save(list = c("result", "ldaID"), file = paste(folder, "-k", K,
                                              "i", num.iterations,
                                              "b", burnin, "s", seed,
                                              ".Rdata", sep = ""))
    }
    else{
        load(paste(folder, "-k", K, "i", num.iterations, "b", burnin, "s", seed,
                   ".Rdata", sep = ""))
    }
    ttw <- top.topic.words(result$topics, num.words = num.words, by.score = TRUE)
    ttw <- rbind(round(t(result$topic_sums / sum(result$topic_sums)),2),ttw)
    rownames(ttw) <- c("", 1:num.words)
    write.csv(ttw, file = paste(folder, "-k", K, "i", num.iterations, "b", burnin, "s",
                  seed, ".csv", sep = ""))
    invisible(result)
}

