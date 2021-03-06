% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readTagesschau.R
\name{readTagesschau}
\alias{readTagesschau}
\title{Read Tagesschau files}
\usage{
readTagesschau(path = getwd(), file = list.files(path = path, pattern =
  "*.xml$", full.names = FALSE, recursive = TRUE), encoding = "utf-8",
  type = c("condensed", "detail"))
}
\arguments{
\item{path}{Character: string with path where the data files are.}

\item{file}{Character: string with names of the XML files.}

\item{encoding}{encoding of the input files.}

\item{type}{Character: string whether to get one text with paragraphs
per show (condensed) or one text per comment/message/report (detail).}
}
\value{
\code{\link{textmeta}} object
}
\description{
Reads XML-files from Tagesschau and separates the text and meta data.
}
\author{
Jonas Rieger (<jonas.rieger@tu-dortmund.de>)
}
\keyword{manip}
