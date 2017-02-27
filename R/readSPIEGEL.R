#' Read the SPIEGEL Corpus
#'
#' Reads the XML-files from the SPIEGEL corpus and seperates the text and meta
#' data.
#'
#'
#' @param path Character string with Path where the data files are.
#' @param file Character string with names of the XML files.
#' @param do.meta Logical: Should the algorithm collect meta data?
#' @param do.text Logical: Should the algorithm collect text data?
#' @return \item{meta}{ id date title year number page_start page_stop pagetitle
#' shorttitle rubrik ressort dokumentmerkmal dachzeile abstract}
#' \item{text}{ Text (Paragraphenweise)} \item{metamult}{ signature person
#' koerperschaft company inkl. Kategorie(n)}
#' @keywords manip
#' @examples
#' ##---- Should be DIRECTLY executable !! ----
#' @export readSPIEGEL
readSPIEGEL <- function(path = getwd(), file = list.files(path=path, pattern="*.xml$",
                                                          full.names = FALSE, recursive = TRUE),
                        do.meta = TRUE, do.text = TRUE){
    stopifnot(is.character(file), is.character(path),
              is.logical(do.meta), is.logical(do.text),
              length(path) == 1, length(do.meta) == 1, length(do.text) == 1)
    text <- NULL
    meta <- NULL
    metamult <- NULL
    for(i in 1:length(file)){
        cat(paste(file[i]), "\n")
        article <- readLines(con = paste(path,file[i], sep="/"), encoding = "latin1")
        lines <- grep(pattern = "</artikel>", article)
        lines <- cbind(c(1,lines[-length(lines)]),lines)
        article <- apply(lines, 1, function(x)paste(article[x[1]:x[2]], collapse = " "))
        id <- stringr::str_extract(article, "<artikel-id>(.*?)</artikel-id>")
        id <- removeXML(id)

        if(do.meta){
            cand <- c("datum", "titel", "jahrgang", "nummer", "seite-start", "seite-ende", "seitentitel", "kurztitel", "rubrik", "ressort", "dokumentmerkmal", "dachzeile", "vorspann")

            cand_names <- c("date", "title", "year", "number", "page_start", "page_stop", "pagetitle", "shorttitle", "rubrik", "ressort", "dokumentmerkmal", "dachzeile", "abstract")
            mData <- NULL
            for(k in 1:length(cand)){
                mData_new <- stringr::str_extract(article, paste0("<", cand[k], ">(.*?)</",cand[k],">"))
                mData_new <- removeXML(mData_new)
                mData <- cbind(mData, mData_new)
            }
            mData <- as.data.frame(mData, stringsAsFactors = FALSE)
            colnames(mData) <- cand_names

            mData$year <- as.numeric(mData$year)
            mData$number <- as.numeric(mData$number)
            mData$date <- as.Date(mData$date, format = "%d%m%Y")
            mData$page_start <- as.numeric(mData$page_start)
            mData$page_stop <- as.numeric(mData$page_stop)

            signature <- stringr::str_extract_all(article, "<signatur>(.*?)</signatur>")
            names(signature) <- id
            tmp <- rep(names(signature), lengths(signature))
            signature <- unlist(signature)
            names(signature) <- tmp
            signature <- removeXML(signature)

            person <- stringr::str_extract_all(article, "<person>(.*?)</person>")
            names(person) <- id
            tmp <- rep(names(person), lengths(person))
            person <- unlist(person)
            names(person) <- tmp
            person <- removeXML(person)

            koerperschaft <- stringr::str_extract_all(article, "<koerperschaft>(.*?)</koerperschaft>")
            names(koerperschaft) <- id
            tmp <- rep(names(koerperschaft), lengths(koerperschaft))
            koerperschaft <- unlist(koerperschaft)
            names(koerperschaft) <- tmp
            koerperschaft <- removeXML(koerperschaft)

            company <- stringr::str_extract_all(article, "<company>(.*?)</company>")
            names(company) <- id
            tmp <- rep(names(company), lengths(company))
            company <- unlist(company)
            names(company) <- tmp
            company <- removeXML(company)

            mData <- cbind(id, mData, stringsAsFactors = FALSE)
            meta <- rbind(meta, mData)

            metamult$signature <- c(metamult$signature, signature)
            metamult$person <- c(metamult$person, person)
            metamult$koerperschaft <- c(metamult$koerperschaft, koerperschaft)
            metamult$company <- c(metamult$company, company)
        }
        if(do.text){
            text_new <- stringr::str_extract_all(article, "<absatz>(.*?)<absatz>")
            text_new <- lapply(text_new,removeXML)
            names(text_new) <- id
            text <- as.list(c(text, text_new))
        }
    }
    res <- list("meta" = meta, "text" = text, "metamult" = metamult)
    class(res) <- "textmeta"
    res <- tmT:::deleteAndRenameDuplicates(res, paragraph = TRUE)
    summary(res)
}
