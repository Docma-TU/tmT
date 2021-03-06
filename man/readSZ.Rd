% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readSZ.R
\name{readSZ}
\alias{readSZ}
\title{Read the SZ corpus}
\usage{
readSZ(path = getwd(), file = list.files(path = path, pattern =
  "*.xml$", full.names = FALSE, recursive = TRUE, ignore.case = TRUE),
  do.meta = TRUE, do.text = TRUE)
}
\arguments{
\item{path}{Path where the data files are.}

\item{file}{Character string with names of the HTML files.}

\item{do.meta}{Logical: Should the algorithm collect meta data?}

\item{do.text}{Logical: Should the algorithm collect text data?}
}
\value{
\item{meta}{ id date rubrik page AnzChar AnzWoerter dachzeile title
zwischentitel untertitel} \item{text}{ Text (Paragraphenweise)}
}
\description{
Reads the XML-files from the SZ corpus and seperates the text and meta data.
}
\examples{

##---- Should be DIRECTLY executable !! ----
}
\keyword{manip}
