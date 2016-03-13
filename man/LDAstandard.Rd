\name{LDAstandard}
\alias{LDAstandard}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to fit LDA model
}
\description{
This function uses the \code{lda.collapsed.gibbs.sampler} from the LDA
package and additional saves topwordlists and a R workspace.
}
\usage{
LDAstandard(documents, K = 100, vocab, num.iterations = 200, burnin = 70, alpha = 0.1, eta = 0.1, seed, folder, num.words = 50, LDA = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{documents}{
A list prepared by \code{\link{docLDA}}.
}
  \item{K}{
number of topics.
}
  \item{vocab}{
character vector containing the words in the corpus.
}
  \item{num.iterations}{
number of iterations for the gibbs sampler.
}
  \item{burnin}{
number of iterations for the burnin.
}
  \item{alpha}{
Hyperparameter for the topic proportions.
}
  \item{eta}{
Hyperparameter for the word distributions.
}
  \item{seed}{
A seed for reproducability.
}
  \item{folder}{
file for the results.
}
  \item{num.words}{
number of words in the top topic words list.
}
  \item{LDA}{
logical: Should a new model be fitted or a existing R workspace be used?
}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
\value{
A .csv containing the topword list and a R workspace containing the
result data.
}
\references{
Blei, David M. and Ng, Andrew and Jordan, Michael. Latent Dirichlet allocation. Journal of
Machine Learning Research, 2003.

Jonathan Chang (2012). lda: Collapsed Gibbs sampling methods for topic models.. R
package version 1.3.2. http://CRAN.R-project.org/package=lda
}
\author{
Lars Koppers (<koppers@statistik.tu-dortmund.de>)
}
%% \note{
%% %%  ~~further notes~~
%% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Documentation for the lda package.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (documents, K = 100, vocab, num.iterations = 200, burnin = 70,
    alpha = 0.1, eta = 0.1, seed, file, num.words = 50, LDA = TRUE)
{
    if (LDA) {
        set.seed(seed)
        result <- lda.collapsed.gibbs.sampler(documents = documents,
            K = K, vocab = vocab, num.iterations = num.iterations,
            burnin = burnin, alpha = alpha, eta = eta, compute.log.likelihood = TRUE)
        ldaID <- names(documents)
        save(list = c("result", "ldaID"), file = paste(file,
            "-k", K, "i", num.iterations, "b", burnin, "s", seed,
            ".Rdata", sep = ""))
    }
    else {
        load(paste(file, "-k", K, "i", num.iterations, "b", burnin,
            "s", seed, ".Rdata", sep = ""))
    }
    write.csv(top.topic.words(result$topics, num.words = num.words,
        by.score = TRUE), file = paste(file, "-k", K, "i", num.iterations,
        "b", burnin, "s", seed, ".csv", sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
