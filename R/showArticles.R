#' Export Readable Article Lists
#'
#' Exports the article id, text, title and date.
#'
#'
#' @param corpus A corpus of type "data"
#' @param id Vector or matrix inkluding article id's.
#' @param file Filename for the export.
#' @return No output in R, only a csv including the requested articles.
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export showArticles
showArticles <- function(corpus,id, file){
    more_files <- TRUE
    if(is.vector(id)){ id <- as.matrix(id); more_files <- FALSE}
    outlist <- list()
    for(i in 1:ncol(id)){
        mtch1 <- match(id[,i],names(corpus$text))
        mtch2 <- match(id[,i],corpus$meta$id)
        out <- lapply(corpus$text[mtch1],paste,collapse=" ")
        out <- unlist(out)
        out2 <- cbind(corpus$meta$id[mtch2],corpus$meta$title[mtch2],as.character(corpus$meta$date[mtch2]),out)
        colnames(out2) <- c("id","title","date","text")
        rownames(out2) <- 1:length(out)
        write.csv(out2, file=paste(file,i,"lesen.csv",sep=""))
        outlist <- c(outlist, list(out2))
    }
    if(more_files) names(outlist) <- 1:ncol(id)
    invisible(outlist)
}
