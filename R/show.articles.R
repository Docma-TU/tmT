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
#' @export show.articles
#' 
show.articles <- function(meta, file, id = meta$id, cols = colnames(meta),
                          csv.ger = FALSE){
  stopifnot(is.data.frame(meta), is.character(file), length(file) == 1,
            is.character(id), all(id %in% meta$id),
            is.character(cols), all(cols %in% colnames(meta)))
  if (csv.ger){
    write.csv2(meta[meta$id %in% id, cols], file = file, row.names = FALSE)
  }
  else {
    write.csv(meta[meta$id %in% id, cols], file = file, row.names = FALSE)
  }
}