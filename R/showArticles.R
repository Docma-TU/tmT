#' Export Readable Article Lists
#'
#' Exports the article id, text, title and date.
#'
#'
#' @param object \code{\link{textmeta}} object
#' @param id Character vector or matrix including article ids.
#' @param file Character Filename for the export.
#' @return A list of the requested articles. If file is set, writes a csv including the meta-data of the
#' requested articles.
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export showArticles
showArticles <- function(object, id = names(object$text), file){
  stopifnot(is.textmeta(object), all(id %in% object$meta$id),
    all(id %in% names(object$text)))
  more_files <- TRUE
  if(is.vector(id)){
    id <- as.matrix(id)
    more_files <- FALSE
    nameArg <- ""
  }
  else{
    if(is.null(colnames(id))) nameArg <- 1:ncol(id)
    else nameArg <- colnames(id)
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
    if(!missing(file)) write.csv(out2, file = paste0(file, nameArg[i], ".csv"))
    outlist <- c(outlist, list(out2))
  }
  if(more_files){
    names(outlist) <- nameArg
  }
  else outlist <- outlist[[1]]
  invisible(outlist)
}
