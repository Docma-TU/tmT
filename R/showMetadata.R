#' Export Readable Meta-Data of Articles.
#'
#' Exports requested meta-data of articles for given ids.
#'
#'
#' @param meta A data.frame of meta-data as a result of a read-function.
#' @param file Filename for the export.
#' @param id Vector of characters including article ids.
#' @param cols vector of characters including the requested colnames of meta.
#' @param csv.ger Logical: Should the function use write.csv2 instead of write.csv?
#' @return No output in R, writes a csv (or csv2) including the meta-data of the
#' requested articles.
#' @keywords manip
#' @examples
#'
#' @export showMetadata
#'
showMetadata <- function(meta, file = "corpus", id = meta$id,
  cols = colnames(meta), csv.ger = FALSE){
  stopifnot(is.data.frame(meta), is.character(file), length(file) == 1,
    all(id %in% meta$id), is.character(cols), all(cols %in% colnames(meta)))
  more_files <- TRUE
  if(is.vector(id)){
    id <- as.matrix(id)
    more_files <- FALSE
  }
  outlist <- list()
  for(i in 1:ncol(id)){
    out <-  meta[meta$id %in% id[, i], cols]
    if (csv.ger) write.csv2(out, file = paste0(file, i, "meta.csv"))
    else write.csv(out, file = paste0(file, i, "meta.csv"))
    outlist <- c(outlist, list(out))
  }
  if(more_files) names(outlist) <- 1:ncol(id)
  else outlist <- outlist[[1]]
  invisible(outlist)
}
