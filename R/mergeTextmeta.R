#' Merge textmeta objects
#'
#' Merges two textmeta objects to one. It is possible to control whether all
#' columns or the intersect should be considered.
#'
#'
#' @param t1 \code{\link{textmeta}} object
#' @param t2 \code{\link{textmeta}} object
#' @param all \code{logical} (default: \code{TRUE}) should the result contain
#' \code{\link{union}} (\code{TRUE}) or \code{\link{intersect}} of columns of
#' \code{t1} and \code{t2}, if \code{TRUE} columns which only appears in one of
#' the meta components the merged meta component is filled with \code{NA}s 
#' @return \code{\link{textmeta}} object
#' @keywords manip
#' @examples
#'
#' @export mergeTextmeta
#'

mergeTextmeta <- function(t1, t2, all = TRUE){
  stopifnot(is.textmeta(t1), is.textmeta(t2), is.logical(all), length(all) == 1)
  if (all) cols <- union(colnames(t1$meta), colnames(t2$meta))
  else cols <- intersect(colnames(t1$meta), colnames(t2$meta))
  if (length(cols) > 0){
    if (is.null(t1$meta)){
      if (is.null(t2$meta)) meta = NULL
      else meta = t2$meta[, cols]
    }
    else{
      if (is.null(t2$meta)) meta = t1$meta[, cols]
      else meta = merge(t1$meta, t2$meta, all = TRUE, sort = FALSE)[, cols]
    }
  }
  else meta = NULL
  object <-
    textmeta(
      meta = meta,
      text = c(t1$text, t2$text),
      metamult = c(t1$metamult, t2$metamult))
  return(object)
}