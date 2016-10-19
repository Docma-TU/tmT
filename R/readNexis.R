#' Read preprocessed files from Lexis Nexis
#'
#' Reads the XML-files from preprocessed Lexis Nexis files and seperates the text and meta
#' data.
#'
#'
#' @param file Character string with names of the XML files.
#' @param do.meta Logical: Should the algorithm collect meta data?
#' @param do.text Logical: Should the algorithm collect text data?
#' @param encoding encoding of the input files.
#' @return List of two \item{meta}{id url date page resource author leadtext downloadDate}
#' \item{text}{text}
#' @author Sakander Zirai (<s.zirai@@live.de>), Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#' @examples
#'
#' @export readNexis
#'
readNexis <- function (file, do.meta = TRUE, do.text = TRUE, encoding="utf-8")
{
  text <- NULL
  meta <- NULL
  for (i in 1:length(file)) {
    (print(file[i]))
    lines <- NULL

    try(openfile <- file(file[i], open = "rt", encoding=encoding))
    try(lines <- readLines(con = openfile))
    try(close(openfile))
    if (length(lines) == 0) {
      next
    }

    fulldata <- paste(lines, collapse = " ")
    downloadDate <- str_extract(fulldata, "timestamp=(.*?)>")
    fulldata <-  str_extract_all(fulldata, "<record>(.*?)</record>")[[1]]

    id <-  str_extract(fulldata, "<record_id>(.*?)</record_id>")
    id <- removeTAG(id)

    if (do.meta) {
    url <- str_extract(fulldata, "<source_id>(.*?)</source_id>")
    url <- removeTAG(url)

    date <- str_extract(fulldata, "<publishDate>(.*?)</publishDate>")
    date <- removeTAG(date)
    date <- as.Date(date, format = "%Y%m%d")

    page <-  str_extract(fulldata, "<series2>(.*?)</series2>")
    page <- removeTAG(page)

    resource <-  str_extract(fulldata, "<series>(.*?)</series>")
    resource <- removeTAG(resource)

    author <-  str_extract(fulldata, "<author>(.*?)</author>")
    author <- removeTAG(author)

    leadtext <-  str_extract(fulldata, "<leadtext>(.*?)</leadtext>")
    leadtext <- removeTAG(leadtext)
    leadtext <- remoceMISC(leadtext)

    downloadDate <- str_extract(downloadDate, "[0-9-]+")
    downloadDate <- as.Date(downloadDate, format = "%Y-%m-%d")
    downloadDate <- rep(downloadDate, times=length(id))

      mData <- data.frame(id,url, date, page, resource, author, leadtext, downloadDate,stringsAsFactors = FALSE)
      meta <- rbind(meta, mData)
    }
    if (do.text) {
    fulltext <-  str_extract(fulldata, "(<fulltext>(.*?)</fulltext>)|(<fulltext(.*?)/>)")
    fulltext <- removeTAG(fulltext)
    leadtext <- remoceMISC(fulltext)
    text <- c(text, fulltext)
    names(text) <- id
    }
  }
  return(list(meta = meta, text = text))
}
