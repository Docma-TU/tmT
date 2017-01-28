#' Read the SZ corpus
#'
#' Reads the XML-files from the SZ corpus and seperates the text and meta data.
#'
#'
#' @param path Path where the data files are.
#' @param file Character string with names of the HTML files.
#' @param do.meta Logical: Should the algorithm collect meta data?
#' @param do.text Logical: Should the algorithm collect text data?
#' @return \item{meta}{ id datum rubrik seite AnzChar AnzWoerter dachzeile
#' titel utitel} \item{text}{ Text (Paragraphenweise)}
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export readSZ
readSZ <- function(path = getwd(),
                   file = list.files(path = path, pattern = "*.xml",
                                     full.names = FALSE, recursive = TRUE),
                   do.meta = TRUE, do.text = TRUE){
  stopifnot(is.character(file), is.character(path),
            is.logical(do.meta), is.logical(do.text),
            length(do.meta) == 1, length(do.text) == 1, length(path) == 1)
  text <- NULL
  meta <- NULL
  
  for(i in 1:length(file)){
    cat(paste(file[i]), "\n")
    article <- readLines(con = paste(path, file[i], sep = "/"), encoding = "latin1")
    article <- gsub(pattern = "&quot;", replacement = "\"",article)
    article <- gsub(pattern = "&amp;", replacement = "&",article)
    article <- gsub(pattern = "&apos;", replacement = "\'",article)
    lines <- grep(pattern = "</ARTICLE>", article)
    lines <- cbind(c(1, lines[-length(lines)]), lines)
    article <- apply(lines, 1, function(x) paste(article[x[1]:x[2]], collapse = " "))
    id <- stringr::str_extract(article, "ID=\"(.*?)\"")
    id <- gsub(pattern = "ID=|\"", replacement = "", x = id)
    
    if(do.meta){
      date <- stringr::str_extract(article, "DATE=\"(.*?)\"")
      date <- as.Date(gsub(pattern = "DATE=\"|\"", replacement = "", x = date), format = "%Y%m%d")
      rubrik <- stringr::str_extract(article, "SECTION=\"(.*?)\"")
      rubrik <- gsub(pattern = "SECTION=\"|\"", replacement = "", x = rubrik)
      page <- stringr::str_extract(article, "PAGE=\"(.*?)\"")
      page <- as.integer(gsub(pattern = "PAGE=\"|\"", replacement = "", x = page))
      AnzChar <- stringr::str_extract(article, "NUM.CHARS=\"(.*?)\"")
      AnzChar <- as.integer(gsub(pattern = "NUM.CHARS=\"|\"", replacement = "", x = AnzChar))
      AnzWoerter <- stringr::str_extract(article, "NUM.WORDS=\"(.*?)\"")
      AnzWoerter <- as.integer(gsub(pattern = "NUM.WORDS=\"|\"", replacement = "", x = AnzWoerter))
      
      dachzeile <- stringr::str_extract(article, "<SZ.DT>(.*?)</SZ.DT>")
      dachzeile <- removeXML(dachzeile)
      title <- stringr::str_extract(article, "<SZ.T>(.*?)</SZ.T>")
      title <- removeXML(title)
      zwischentitel <- stringr::str_extract(article, "<SZ.ZT>(.*?)</SZ.ZT>")
      zwischentitel <- removeXML(zwischentitel)
      untertitel <- stringr::str_extract(article, "<SZ.UT>(.*?)</SZ.UT>")
      untertitel <- removeXML(untertitel)
      
      mData <- data.frame(id, date, rubrik, page, AnzChar, AnzWoerter,
                          dachzeile, title, zwischentitel, untertitel,
                          stringsAsFactors = FALSE)
      meta <- rbind(meta,mData)
    }
    if(do.text){
      text_new <- stringr::str_extract(article, "<TEXT(.*?)</TEXT>")
      to_delete <- c("<SZ.DT>(.*?)</SZ.DT>", "<SZ.T>(.*?)</SZ.T>", "<SZ.UT>(.*?)</SZ.UT>",
                     "<SZ.ZT>(.*?)</SZ.ZT>", "<PICTEXT>(.*?)</PICTEXT>",
                     "<TABLE>(.*?)</TABLE>", "<AUTHOR>(.*?)</AUTHOR>")
      for(i in to_delete){
        text_new <- gsub(pattern = i, replacement = " ", x = text_new, perl = TRUE)
      }
      text_new <- strsplit(text_new, "<P>|<P [^>]*>", perl = TRUE)
      text_new <- lapply(text_new, removeXML)
      names(text_new) <- id
      text <- as.list(c(text, text_new))
    }
  }
  res <- list("meta" = meta, "text" = text, metamult = NULL)
  class(res) <- "textmeta"
  summary(res)
}
