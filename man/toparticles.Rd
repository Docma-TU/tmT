\name{toparticles}
\alias{toparticles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get The IDs Of The Most Representive Articles
}
\description{
Extract the article IDs belonging to the articles with the most relative
or absolute number of words per topic.
}
\usage{
toparticles(x, id, limit = 20, rel = TRUE, topn = TRUE, themes = 1:nrow(x$document_sums), minlength = 30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
LDA result
}
  \item{id}{
Vector of text IDs
}
  \item{limit}{
Integer, number of article ids per topic.
}
  \item{rel}{
Logical: Should be the relative frequency be used?
}
  \item{topn}{
Logical: Should only the topn Article IDs be reported? \code{FALSE} is
not implemented.
}
  \item{themes}{
not implemented.
}
  \item{minlength}{
Minimal total number of words a text must have to be included.
}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
\value{
Matrix of Article IDs.
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
function (x, id, limit = 20, rel = TRUE, topn = TRUE, themes = 1:nrow(x$document_sums),
    minlength = 30)
{
    small <- which(apply(x$document_sums, 2, sum) < minlength)
    if (length(small > 0)) {
        x$document_sums <- x$document_sums[, -small]
        id <- id[-small]
    }
    if (rel) {
        res <- t(t(x$document_sums)/apply(x$document_sums, 2,
            sum))
    }
    else {
        res <- x$document_sums
    }
    if (topn) {
        top <- apply(res, 1, function(x) order(x, decreasing = TRUE)[1:limit])
    }
    res <- apply(top, 2, function(x) id[x])
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

