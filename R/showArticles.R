#' Export Readable Article Lists
#'
#' Exports the article id, text, title and date.
#'
#'
#' @param object \code{\link{textmeta}} object
#' @param id Vector or matrix inkluding article id's.
#' @param file Filename for the export.
#' @return No output in R, only a csv including the requested articles.
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export showArticles
showArticles <- function(object, id = names(object$text), file = deparse(substitute(object))){
  stopifnot(is.textmeta(object))
  more_files <- TRUE
  if(is.vector(id)){
    id <- as.matrix(id)
    more_files <- FALSE
  }
  outlist <- list()
  for(i in 1:ncol(id)){
    mtch1 <- match(id[,i],names(object$text))
    mtch2 <- match(id[,i],object$meta$id)
    out <- lapply(object$text[mtch1],paste,collapse=" ")
    out <- unlist(out)
    out2 <- cbind(object$meta$id[mtch2],object$meta$title[mtch2],as.character(object$meta$date[mtch2]),out)
    out2 <- data.frame(out2, stringsAsFactors = FALSE, row.names = 1:length(out))
    colnames(out2) <- c("id","title","date","text")
    write.csv(out2, file=paste(file,i,"lesen.csv",sep=""))
    outlist <- c(outlist, list(out2))
  }
  if(more_files) names(outlist) <- 1:ncol(id)
  else outlist <- outlist[[1]]
  invisible(outlist)
}
