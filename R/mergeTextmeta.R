#' Title
#'
#' Descrp.
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
  stopifnot(is.textmeta(t1), is.textmeta(t2))
  if (all) cols <- union(colnames(t1$meta), colnames(t2$meta))
  else cols <- intersect(colnames(t1$meta), colnames(t2$meta))
  object <-
    textmeta(
      meta = merge(t1$meta, t2$meta, all = TRUE, sort = FALSE)[, cols],
      text = c(t1$text, t2$text),
      metamult = c(t1$metamult, t2$metamult))
  return(object)
}