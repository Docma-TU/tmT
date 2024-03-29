#' Read preprocessed files from Lexis Nexis
#'
#' Reads the XML-files from preprocessed Lexis Nexis files and seperates the text and meta
#' data.
#'
#'
#' @param path Character string with Path where the data files are.
#' @param file Character string with names of the XML files.
#' @param do.meta Logical: Should the algorithm collect meta data?
#' @param do.text Logical: Should the algorithm collect text data?
#' @param encoding encoding of the input files.
#' @return List of two \item{meta}{id url date title page resource author
#' leadtext downloadDate}
#' \item{text}{text}
#' @author Sakander Zirai (<s.zirai@@live.de>), Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#'
#' @export readNexis
#'
readNexis <- function(path = getwd(),
                      file = list.files(path = path, pattern = "*.xml$",
                                        full.names = FALSE, recursive = TRUE),
                      do.meta = TRUE, do.text = TRUE, encoding = "utf-8"){
  stopifnot(is.character(file), is.character(path),
            is.logical(do.meta), is.logical(do.text), is.character(encoding),
            length(path) == 1, length(do.meta) == 1, length(do.text) == 1,
            length(encoding) == 1)
  text <- NULL
  meta <- NULL
  metamult <- NULL
  for (i in seq_along(file)) {
    cat(paste(file[i]), "\n")
    article <- readLines(con = paste(path,file[i], sep="/"), encoding = encoding)
    downloadDate <- as.character(na.omit(stringr::str_extract(article,"timestamp=(.*?)>")))[1]
    #if (length(downloadDate) < 1) downloadDate = ""
    lines <- grep(pattern = "</record>", article)
    lines <- cbind(c(1,lines[-length(lines)]),lines)
    article <- apply(lines, 1, function(x)paste(article[x[1]:x[2]], collapse = " "))
    article <-  stringr::str_extract(article, "<record(.*?)</record>")
    
    id <- stringr::str_extract(article, "<record_id>(.*?)</record_id>")
    id <- removeXML(id)
    
    leadtext <-  stringr::str_extract(article, "<leadtext>(.*?)</leadtext>")
    leadtext <- removeXML(leadtext)
    leadtext <- gsub(pattern = "^\\s+|\\s+$|Original Gesamtseiten-PDF",
                     replacement = "", leadtext, perl = TRUE)
    
    if (do.meta) {
      url <- stringr::str_extract(article, "<source_id>(.*?)</source_id>")
      url <- removeXML(url)
      
      date <- stringr::str_extract(article, "<publishDate>(.*?)</publishDate>")
      date <- removeXML(date)
      date <- as.Date(date, format = "%Y%m%d")
      
      title <-  stringr::str_extract(article, "<title>(.*?)</title>")
      title <- removeXML(title)
      
      page <-  stringr::str_extract(article, "<series2>(.*?)</series2>")
      page <- removeXML(page)
      
      resource <-  stringr::str_extract(article, "<series>(.*?)</series>")
      resource <- removeXML(resource)
      
      author <-  stringr::str_extract(article, "<author>(.*?)</author>")
      author <- removeXML(author)
      
      downloadDate <- stringr::str_extract(downloadDate, "[0-9-]+")
      downloadDate <- as.Date(downloadDate, format = "%Y-%m-%d")
      downloadDate <- rep(downloadDate, times = length(id))
      
      mData <- data.frame(id, url, date, title, page, resource, author, leadtext,
                          downloadDate, stringsAsFactors = FALSE)
      meta <- rbind(meta, mData)
    }
    if (do.text) {
      text_new <- stringr::str_extract(article,"(<fulltext>(.*?)</fulltext>)|(<fulltext(.*?)/>)")
      text_new <- removeXML(text_new)
      text_new <- gsub(pattern = "^\\s+|\\s+$|Original Gesamtseiten-PDF",
                       replacement = "", text_new, perl = TRUE)
      text_new[is.na(text_new)] <- ""
      leadtext[is.na(leadtext)] <- ""
      text_new <- trimws(paste(leadtext, text_new))
      names(text_new) <- id
      text <- as.list(c(text, text_new))
    }
  }
  res <- list("meta" = meta, "text" = text, "metamult" = metamult)
  class(res) <- "textmeta"
  if (do.text){
    if (any(is.na(names(res$text)))){
      if (!all(names(res$text) == res$meta$id, na.rm = TRUE)) stop("names(text) do not equal IDs!")
      if (!all(is.na(names(res$text)) == is.na(res$meta$id))) stop("NAs in names(text) do not equal NAs in IDs!")
      res$text = res$text[!is.na(names(res$text))]
      res$meta = res$meta[!is.na(res$meta$id),]
    }
    res <- deleteAndRenameDuplicates(res)
  }
  summary(res)
}
