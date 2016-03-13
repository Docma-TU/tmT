\name{readSZ}
\alias{readSZ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read the SZ corpus
}
\description{
Reads the XML-files from the SZ corpus and seperates the text and meta data.
}
\usage{
readSZ(file, folder, do.meta = TRUE, do.text = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Names of the XML files.
}
  \item{folder}{
Names of the folder, in which the files are. Must have same length like \code{file}.
}
  \item{do.meta}{
Logical: Should the algorithm collect meta data?
}
  \item{do.text}{
Logical: Should the algorithm collect text data?
}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
\value{
\item{meta}{
    id
    datum
    rubrik
    seite
    AnzChar
    AnzWoerter
    dachzeile
    titel
    utitel}
\item{text}{    Text (Paragraphenweise)}

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
function (file, folder, do.meta = TRUE, do.text = TRUE)
{
    text <- NULL
    meta <- NULL
    for (i in 1:length(file)) {
        (print(file[i]))
        lines <- NULL
        try(openfile <- file(paste("Daten/", folder[i], "/",
            file[i], sep = ""), open = "rt"))
        try(lines <- readLines(con = openfile))
        try(close(openfile))
        if (length(lines) == 0) {
            next
        }
        artikel <- strsplit(paste(lines, collapse = " "), "<ARTICLE")[[1]]
        artikel <- artikel[-1]
        id <- strsplit(artikel, "ID=\"")
        id <- sapply(id, function(x) {
            x[2]
        })
        id <- strsplit(id, "\"")
        id <- sapply(id, function(x) {
            x[1]
        })
        if (do.meta) {
            datum <- strsplit(artikel, "DATE=\"")
            datum <- sapply(datum, function(x) {
                x[2]
            })
            datum <- strsplit(datum, "\"")
            datum <- as.character(sapply(datum, function(x) {
                x[1]
            }))
            datum <- as.Date(datum, format = "\%Y\%m\%d")
            rubrik <- strsplit(artikel, "SECTION=\"")
            rubrik <- sapply(rubrik, function(x) {
                x[2]
            })
            rubrik <- strsplit(rubrik, "\"")
            rubrik <- sapply(rubrik, function(x) {
                x[1]
            })
            seite <- strsplit(artikel, "PAGE=\"")
            seite <- sapply(seite, function(x) {
                x[2]
            })
            seite <- strsplit(seite, "\"")
            seite <- as.numeric(sapply(seite, function(x) {
                x[1]
            }))
            AnzChar <- strsplit(artikel, "NUM.CHARS=\"")
            AnzChar <- sapply(AnzChar, function(x) {
                x[2]
            })
            AnzChar <- strsplit(AnzChar, "\"")
            AnzChar <- as.numeric(sapply(AnzChar, function(x) {
                x[1]
            }))
            AnzWoerter <- strsplit(artikel, "NUM.WORDS=\"")
            AnzWoerter <- sapply(AnzWoerter, function(x) {
                x[2]
            })
            AnzWoerter <- strsplit(AnzWoerter, "\"")
            AnzWoerter <- as.numeric(sapply(AnzWoerter, function(x) {
                x[1]
            }))
            dachzeile <- strsplit(artikel, "<SZ.DT>")
            dachzeile <- sapply(dachzeile, function(x) {
                x[2]
            })
            dachzeile <- strsplit(dachzeile, "</SZ.DT>")
            dachzeile <- sapply(dachzeile, function(x) {
                x[1]
            })
            titel <- strsplit(artikel, "<SZ.T>")
            titel <- sapply(titel, function(x) {
                x[2]
            })
            titel <- strsplit(titel, "</SZ.T>")
            titel <- sapply(titel, function(x) {
                x[1]
            })
            untertitel <- strsplit(artikel, "<SZ.UT>")
            untertitel <- sapply(untertitel, function(x) {
                x[2]
            })
            untertitel <- strsplit(untertitel, "</SZ.UT>")
            untertitel <- sapply(untertitel, function(x) {
                x[1]
            })
            titel <- gsub(pattern = "<[^>]*>", replacement = "",
                titel, perl = TRUE)
            untertitel <- gsub(pattern = "<[^>]*>", replacement = "",
                untertitel, perl = TRUE)
            dachzeile <- gsub(pattern = "<[^>]*>", replacement = "",
                dachzeile, perl = TRUE)
            mData <- data.frame(id, datum, rubrik, seite, AnzChar,
                AnzWoerter, dachzeile, titel, untertitel, stringsAsFactors = FALSE)
            meta <- rbind(meta, mData)
        }
        if (do.text) {
            text_neu <- strsplit(artikel, "<TEXT.*?>", perl = TRUE)
            text_neu <- sapply(text_neu, function(x) {
                x[-1]
            })
            text_neu <- gsub(pattern = "<SZ.DT>.*?</SZ.DT>",
                replacement = " ", x = text_neu, perl = TRUE)
            text_neu <- gsub(pattern = "<SZ.T>.*?</SZ.T>", replacement = " ",
                x = text_neu, perl = TRUE)
            text_neu <- gsub(pattern = "<SZ.UT>.*?</SZ.UT>",
                replacement = " ", x = text_neu, perl = TRUE)
            text_neu <- gsub(pattern = "<PICTEXT>.*?</PICTEXT>",
                replacement = " ", x = text_neu, perl = TRUE)
            text_neu <- gsub(pattern = "<SZ.ZT>.*?</SZ.ZT>",
                replacement = " ", x = text_neu, perl = TRUE)
            text_neu <- gsub(pattern = "<TABLE>.*?</TABLE>",
                replacement = " ", x = text_neu, perl = TRUE)
            text_neu <- strsplit(text_neu, "</TEXT>", perl = TRUE)
            text_neu <- sapply(text_neu, function(x) {
                x[1]
            })
            text_neu <- sapply(text_neu, function(x) {
                x <- paste(" ", x)
            })
            text_neu <- strsplit(text_neu, "<P>|<P [^>]*>", perl = TRUE)
            text_neu <- sapply(text_neu, function(x) {
                x[-1]
            })
            text_neu <- lapply(text_neu, function(x) {
                gsub(pattern = "</P>", replacement = "", x)
            })
            text <- c(text, text_neu)
        }
    }
    return(list(meta = meta, text = text))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
