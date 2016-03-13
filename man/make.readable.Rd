\name{make.readable}
\alias{make.readable}
\title{
Export Readable Article Lists
}
\description{
Exports the article id, text, title and date.}
\usage{
make.readable(corpus, id, file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpus}{
A corpus of type "data"
}
  \item{id}{
Vector or matrix inkluding article id's.
}
  \item{file}{
Filename for the export.
}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
\value{
No output in R, only a csv including the requested articles.
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
function (corpus, id, file)
{
    for (i in 1:ncol(id)) {
        mtch1 <- match(id[, i], names(corpus$text))
        mtch2 <- match(id[, i], corpus$meta$id)
        out <- lapply(corpus$text[mtch1], paste, collapse = " ")
        out <- unlist(out)
        out2 <- cbind(corpus$meta$id[mtch2], corpus$meta$titel[mtch2],
            as.character(corpus$meta$datum[mtch2]), out)
        colnames(out2) <- c("ID", "Titel", "Datum", "Text")
        rownames(out2) <- 1:length(out)
        write.csv(out2, file = paste(file, i, "lesen.csv", sep = ""))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

