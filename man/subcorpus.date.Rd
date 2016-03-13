\name{subcorpus.date}
\alias{subcorpus.date}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subcorpus With Date Filter
}
\description{
Creates a subcorpus including a specific time span.
}
\usage{
subcorpus.date(text, meta, s.date = min(meta$datum), e.date = max(meta$datum))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
List of article texts
}
  \item{meta}{
Dataframe of metadata
}
  \item{s.date}{
Start date of subcorpus as date object
}
  \item{e.date}{
End date of subcorpus as date object
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
function (text, meta, s.date = min(meta$datum), e.date = max(meta$datum))
{
    mtch <- match(names(text), meta$id)
    dateID <- which(meta$datum[mtch] >= s.date & meta$datum[mtch] <=
        e.date)
    text <- text[dateID]
    return(text)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
