% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topicsInTextList.R
\name{topicsInTextList}
\alias{topicsInTextList}
\title{Creates list of topics to which most words of a text were assigned}
\usage{
topicsInTextList(text, ldaID, scIDs, ldaresult, label = NULL, vocab)
}
\arguments{
\item{text}{The result of \code{\link{LDAprep}}}

\item{ldaID}{List of IDs for \code{text}}

\item{scIDs}{List of IDs for which topic assignment will be given}

\item{ldaresult}{A result object from the \code{standardLDA}}

\item{label}{Optional label for each topic}

\item{vocab}{Character: Vector of \code{vocab} corresponding to the \code{text} object}
}
\value{
TopicWordAssignment: matrix of texts ids and the topic most words were assigned to + share
}
\description{
For all texts of a given subcorpus, the function creates a list of topics 
to which most and second-most words from a text were assigned to and
the share of words.
}
