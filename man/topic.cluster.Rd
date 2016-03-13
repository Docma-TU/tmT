\name{topic.cluster}
\alias{topic.cluster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cluster Analysis
}
\description{
Makes a cluster analysis with the hellinger distance.
}
\usage{
topic.cluster(topics, file, topicnames = 1:nrow(topics), method = "average")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{topics}{
A matrix like the \code{topic} matrix from \code{lda.collapse.gibbs.sampler}
}
  \item{file}{
File for the dendogram pdf.
}
  \item{topicnames}{
Character vector as label for the topics.
}
  \item{method}{
Method statement from \code{\link{hclust}}
}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
\value{
A dendogram as pdf and a List
\item{dist}{A distance matrix}
\item{clust}{The result from \code{hclust}.}
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
function (topics, file, topicnames = 1:nrow(topics), method = "average")
{
    topics <- topics/rowSums(topics)
    topics <- sqrt(topics)
    Dist <- 1/sqrt(2) * dist(topics)
    clust <- hclust(d = Dist, method)
    pdf(file, width = 30, height = 15)
    plot(clust, label = topicnames)
    dev.off()
    return(list(Dist, clust))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
