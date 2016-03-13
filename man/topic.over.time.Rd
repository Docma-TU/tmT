\name{topic.over.time}
\alias{topic.over.time}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting Topics Over Time
}
\description{
Creates a pdf including a plot for each topic. For each topic the number
of words per month would be plotted.
}
\usage{
topic.over.time(x, ldaID, meta, file, Tnames, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
LDA result object
}
  \item{ldaID}{
Character vector including IDs of the texts.
}
  \item{meta}{
The meta data for the texts.
}
  \item{file}{
Name of the pdf file.
}
  \item{Tnames}{
Label for the topics
}
  \item{\dots}{
Further Statements passed to the plot function.
}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
\value{
A pdf.
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
function (x, ldaID, meta, file, Tnames, ...)
{
    pdf(file = file, width = 12)
    tmp <- x$document_sums
    tmpdate <- meta$datum[match(ldaID, meta$id)]
    tmp2 <- round_date(tmpdate, "month")
    for (j in 1:nrow(tmp)) {
        splt <- split(tmp[j, ], tmp2)
        splt <- sapply(splt, sum)
        if (length(Tnames) > 1) {
            plot(as.Date(names(splt)), splt, main = paste("Topic",
                j, ":", Tnames[j], sep = " "), type = "l")
        }
        else {
            plot(as.Date(names(splt)), splt, main = paste("Topic",
                j, ":", Tnames, sep = " "), type = "l")
        }
    }
    dev.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
