\name{make.wordlist}
\alias{make.wordlist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Count words in text corpora
}
\description{
Creates a wordlist and a frequency table.
}
\usage{
make.wordlist(x, k = 1e+05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
List of texts.
}
  \item{k}{
Integer, how many Texts should be processed at one time (RAM usage)?
}
}
\details{
This function helps, if \code{table(x)} needs too much RAM.
}
\value{
 \item{words}{An alphabetical list of the words in the corpus.}
 \item{wordtable}{A frequency table of the words in the corpus.}
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, k = 1e+05)
{
    n <- length(x)
    (print(n))
    N <- 0:floor(n/k)
    words <- NULL
    for (i in N) {
        (print(i * k))
        words <- c(words, unique(unlist(x[(i * k + 1):(min(n,
            i * k + k))])))
    }
    words <- sort(unique(words))
    (print("table"))
    wordtable <- rep(0, length(words))
    names(wordtable) <- words
    for (i in N) {
        (print(i * k))
        tmp <- table(unlist(x[(i * k + 1):(min(n, i * k + k))]))
        mtch <- match(names(tmp), names(wordtable))
        wordtable[mtch] <- wordtable[mtch] + tmp
    }
    return(list(words = words, wordtable = wordtable))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

