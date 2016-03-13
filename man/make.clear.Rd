\name{make.clear}
\alias{make.clear}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Some data preprocessing
}
\description{
Removes punctuation, numbers and stopwords, change into lowercase
letters and tokenization.
}
\usage{
make.clear(x, sw = c(stopwords("german"), "dass", "fuer", "koennen", "koennte", "ueber", "waehrend", "wuerde", "wuerden"), paragraph = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
list containing character strings.
}
  \item{sw}{
list of stopwords.
}
  \item{paragraph}{
Logical: Should be set to \code{TRUE} if the article is a list of
character strings, representing the paragraphs.
}
}
\details{
Removes punctuation, numbers and stopwords, change into lowercase
letters and tokenization. Additional some cleaning steps: remove empty
words / paragraphs / article.
}
\value{
A list containing the preprocessed Article.
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
function (x, sw = c(stopwords("german"), "dass", "fuer", "koennen",
    "koennte", "ueber", "waehrend", "wuerde", "wuerden"), paragraph = TRUE)
{
    require(tm)
    (print("punctuation"))
    x <- lapply(x, removePunctuation, preserve_intra_word_dashes = FALSE)
    (print("numbers"))
    x <- lapply(x, removeNumbers)
    (print("to lower"))
    x <- lapply(x, tolower)
    (print("stopwords"))
    x <- lapply(x, removeWords, sw)
    (print("whitespace"))
    x <- lapply(x, stripWhitespace)
    if (paragraph) {
        (print("tokenization"))
        x <- lapply(x, strsplit, " ")
        (print("remove empty words"))
        x <- lapply(x, function(x) {
            lapply(x, function(y) {
                if (length(which(y == "") > 0)) {
                  y[-which(y == "")]
                }
                else {
                  y
                }
            })
        })
        (print("remove empty article"))
        x <- x[!(lengths(x) == 0)]
        (print("remove empty paragraphs"))
        x <- lapply(x, function(x) x[!(lengths(x) == 0)])
        (print("remove empty article 2"))
        x <- x[!(lengths(x) == 0)]
    }
    else {
        (print("tokenization"))
        x <- sapply(x, strsplit, " ")
        (print("remove empty words"))
        x <- lapply(x, function(y) {
            if (length(which(y == "") > 0)) {
                y[-which(y == "")]
            }
            else {
                y
            }
        })
        (print("remove empty article"))
        emptyArt <- which(sapply(x, function(x) {
            length(x) == 0
        }))
        (print(length(emptyArt)))
        if (length(emptyArt) > 0) {
            x <- x[-emptyArt]
        }
        emptyArt <- sapply(x, function(x) !is.na(x)[1])
        (print(sum(!emptyArt)))
        x <- x[emptyArt]
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

