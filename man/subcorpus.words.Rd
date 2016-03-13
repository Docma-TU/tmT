\name{subcorpus.words}
\alias{subcorpus.words}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subcorpus With Word Filter
}
\description{
Creates a subcorpus including specific words.
}
\usage{
subcorpus.words(text, wordlist, counts = 1, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
List of article texts
}
  \item{wordlist}{
List of character vectors. Every List element is an 'or' link, every
character String in a vector is linked by an 'and'. If \code{wordlist}
is only a character Vector the link is 'or'.
}
  \item{counts}{
Integer, or same list structure like \code{wordlist}. Number of times a
word must appear to be counted.
}
  \item{ignore.case}{
Option from \code{\link{grepl}}.
}
  \item{perl}{
Option from \code{\link{grepl}}.
}
  \item{fixed}{
Option from \code{\link{grepl}}.
}
  \item{useBytes}{
Option from \code{\link{grepl}}.
}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
\value{
Filtered list of texts.
}
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{
Lars Koppers (<koppers@statistik.tu-dortmund.de>)%%  ~~who you are~~
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
function (text, wordlist, counts = 1, ignore.case = FALSE, perl = FALSE,
    fixed = FALSE, useBytes = FALSE)
{
    subid <- numeric(length(text))
    for (i in 1:length(wordlist)) {
        tmp <- NULL
        for (j in 1:length(wordlist[[i]])) {
            if (length(counts) == 1) {
                co <- counts
            }
            else {
                co <- counts[[i]][j]
            }
            tmp <- cbind(tmp, sapply(text, function(x) sum(grepl(pattern = wordlist[[i]][j],
                x = x, ignore.case = ignore.case, perl = perl,
                fixed = fixed, useBytes = useBytes)) >= co))
        }
        subid <- subid + apply(tmp, 1, prod)
    }
    subid <- subid > 0
    return(text[subid])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
