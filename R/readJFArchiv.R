#' Read the JF-Archiv.de Corpus as CSV
#'
#' Reads the CSV-files from a WORDPRESS BLOG corpus and seperates the text and meta
#' data.
#'
#'
#' @param path Character string with Path where the data files are.
#' @param file Character string with names of the CVS files.
#' @param do.meta Logical: Should the algorithm collect meta data?
#' @param do.text Logical: Should the algorithm collect text data?
#' @return \item{meta}{ id date title year number page_start page_stop pagetitle
#' shorttitle rubrik ressort dokumentmerkmal dachzeile abstract}
#' \item{text}{ Text (Paragraphenweise)} \item{metamult}{ signature person
#' koerperschaft company inkl. Kategorie(n)}
#' @keywords manip
#' @examples
#' ##---- Should be DIRECTLY executable !! ----
#' @export readJFArchiv
#' 
#' 

readJFArchiv <- function(path = getwd(), file = list.files(path=path, pattern="*.csv$",
                                                          full.names = FALSE, recursive = TRUE),
                        do.meta = TRUE, do.text = TRUE){
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
    stopifnot(is.character(file), is.character(path),
              is.logical(do.meta), is.logical(do.text),
              length(path) == 1, length(do.meta) == 1, length(do.text) == 1)
    text <- NULL
    meta <- NULL
  
    for(i in 1:length(file)){
        #cat(paste(file[i]), "\n")
        print(file[i])
        csv <- read.csv(file= paste(path,file[i], sep="/"), header=TRUE, sep=",", encoding = "utf-8", stringsAsFactors=FALSE)
        id <- csv["url"][[1]]

        if(do.meta){
            cand <- c("datum", "titel", "jahrgang", "nummer", "seite-start", "seite-ende", "seitentitel", "kurztitel", "rubrik", "ressort", "dokumentmerkmal", "dachzeile", "vorspann")

            cand_names <- c("article_date", "title","author","text","url")
            mData <- NULL
            
            for(k in 1:length(cand_names)){
                mData_new <- csv[cand_names[k]]
                #mData_new <- removeXML(mData_new)
                if(is.null(mData)){
                  mData<-mData_new
                }else{
                  mData <- cbind(mData, mData_new)
                }
            }
            #mData <- as.data.frame(mData, stringsAsFactors = FALSE)
            #colnames(mData) <- cand_names
            mData$date <- as.Date(mData$article_date, format = "%d.%m.%y")
            
            for(j in 1:length(mData$article_date)){
              
              date_help <- as.Date(mData$article_date[j], format = "%d.%m.%y")
              if (is.na(date_help) & (j>1)){
                
                help_str  <- paste(trim(toString(mData$article_date[j])), format( mData$date[j-1], '%y'),sep="")
                date_help <- as.Date(help_str, format = "%d.%m.%y")
              }
              mData$date[j] <- date_help
              
            }


            mData$title <- trim(mData$title)
            mData$author <- trim(mData$author)
            mData$text  <- trim(mData$text)

            mData$article_date <- NULL

            mData <- cbind(id, mData, stringsAsFactors = FALSE)
            meta <- rbind(meta, mData)

           
        }
        if(do.text){
            text_new <- csv["text"][[1]]
            text_new <- removeXML(text_new)
            names(text_new) <- id
            text <- as.list(c(text, text_new))
        }
    }
    res <- list("meta" = meta, "text" = text)
    class(res) <- "textmeta"
    return(res)
    #if (do.text) res <- deleteAndRenameDuplicates(res, paragraph = TRUE)
    #summary(res)
    #res2 <<- res
}

