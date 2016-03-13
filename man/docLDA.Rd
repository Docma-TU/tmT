\name{docLDA}
\alias{docLDA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create lda-ready dataset
}
\description{
This function transforms a text corpus like the result from
\code{\link{make.clear}} in a form needed by the lda-package.
}
\usage{
docLDA(corpus, vocab, ldacorrect = TRUE, excludeNA = TRUE, reduce = TRUE)
}

\arguments{
  \item{corpus}{
a list of tokenized texts.
}
  \item{vocab}{
a charcter vector containing all words which should beused for lda.
}
  \item{ldacorrect}{
logical: If \code{TRUE}, every word repetition gets an own column.
}
  \item{excludeNA}{
logical: Should \code{NA}s be removed?
}
  \item{reduce}{
logical: Should empty texts be deleted?
}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
\value{
A list in which every entry contains a matrix with two rows: the first
  row gives the number of the entry of the word in \code{vocab} minus
  one, the second row the number of the occurrence of the word in the
  article.
  If \code{ldacorrect=TRUE} the second row is 1 and the number of the
  occurrence of the word will be shown by the number of columns
  belonging to this word.
}
%% \references{
%% ~put references to the literature/web site here ~
%% }
\author{
Lars Koppers (<koppers@statistik.tu-dortmund.de>)
}
%% \note{
%% %%  ~~further notes~~
%% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (corpus, vocab, ldacorrect = TRUE, excludeNA = TRUE,
    reduce = TRUE)
{
    for (i in 1:length(corpus)) {
        a <- table(corpus[[i]])
        a <- rbind(as.integer(match(names(a), vocab) - 1), as.integer(a))
        if (ldacorrect) {
            corpus[[i]] <- matrix(as.integer(1), 2, sum(a[2,
                ]))
            corpus[[i]][1, ] <- as.integer(unlist(apply(a, 2,
                function(x) {
                  rep(x[1], each = x[2])
                })))
        }
        else {
            corpus[[i]] <- a
        }
        if (excludeNA) {
            if (any(is.na(corpus[[i]][1, ]))) {
                corpus[[i]] <- corpus[[i]][, -which(is.na(corpus[[i]][1,
                  ]))]
            }
        }
    }
    if (reduce) {
        DIM <- sapply(corpus, function(x) dim(x)[2])
        index1 <- which(sapply(DIM, is.null))
        corpus <- corpus[-index1]
        DIM <- sapply(corpus, function(x) dim(x)[2])
        corpus <- corpus[which(DIM > 0)]
    }
    return(corpus)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

