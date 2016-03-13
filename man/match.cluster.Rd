\name{match.cluster}
\alias{match.cluster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Preparation of different LDAs For Clustering
}
\description{
Merges different lda-results to one matrix, including only the words
which appears in all lda.
}
\usage{
match.cluster(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A list of lda results.
}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
\value{
A matrix including all topics from all lda-results. Number of rows is
the number of topics, the number of columns is the number of words which
appear in all results.
}
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{
Lars Koppers (<koppers@statistik.tu-dortmund.de>)
}
%% \note{
%% %%  ~~further notes~~
%% }

%% %% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
    n <- length(x)
    x <- lapply(x, function(x) x$topics)
    vocab <- table(unlist(sapply(x, colnames)))
    vocab <- names(vocab)[which(vocab == n)]
    mtch <- lapply(x, function(x) match(vocab, colnames(x)))
    res <- NULL
    for (i in 1:length(x)) res <- rbind(res, x[[i]][, mtch[[i]]])
    res <- res/rowSums(res)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
