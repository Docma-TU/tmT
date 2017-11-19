#' Merge textmeta objects
#'
#' Merges a list of textmeta objects to one. It is possible to control whether all
#' columns or the intersect should be considered.
#'
#'
#' @param x a list of \code{\link{textmeta}} objects
#' @param all \code{logical} (default: \code{TRUE}) should the result contain
#' \code{\link{union}} (\code{TRUE}) or \code{\link{intersect}} of columns of
#' all objects, if \code{TRUE} columns which only appears in one of
#' the meta components the merged meta component is filled with \code{NA}s 
#' @return \code{\link{textmeta}} object
#' @keywords manip
#' @examples
#'
#' @export mergeTextmeta
#'

mergeTextmeta <- function(x, all = TRUE){
  stopifnot(is.list(x), all(sapply(x, is.textmeta)),
    is.logical(all), length(all) == 1)
  if (any(duplicated(unlist(lapply(x, function(y) names(y$text))))))
    cat(paste0("NOTE: There are duplicates in the names of texts",
      ", could result in problems with unambiguity."))
  if (all) cols <- Reduce(union, lapply(x, function(y) colnames(y$meta)))
  else cols <- Reduce(intersect, lapply(x, function(y) colnames(y$meta)))
  meta = NULL
  if (length(cols) > 0){
    ind <- which(sapply(x, function(y) is.data.frame(y$meta)))
    mymerge <- function(t1, t2)
      merge(t1$meta, t2$meta, all = TRUE, sort = FALSE)[, cols]
    # does not work: if (length(ind) > 1) meta <- Reduce(mymerge, x[ind])
    if (length(ind) > 1){
      meta <- (x[[ind[1]]])$meta
      for (i in 2:length(ind))
        meta <- merge(meta, (x[[ind[i]]])$meta, all = TRUE, sort = FALSE)[, cols]
    }
    else meta <- (x[[ind]])$meta # only one data.frame
  }
  object <- textmeta(meta = meta, text = Reduce(c, lapply(x, function(y) y$text)),
    metamult = Reduce(c, lapply(x, function(y) y$metamult)))
  return(object)
}

# mergeTextmeta <- function(t1, t2, all = TRUE){
#   stopifnot(is.textmeta(t1), is.textmeta(t2), is.logical(all), length(all) == 1)
#   if (all) cols <- union(colnames(t1$meta), colnames(t2$meta))
#   else cols <- intersect(colnames(t1$meta), colnames(t2$meta))
#   if (length(cols) > 0){
#     if (is.null(t1$meta)){
#       if (is.null(t2$meta)) meta = NULL
#       else meta = t2$meta[, cols]
#     }
#     else{
#       if (is.null(t2$meta)) meta = t1$meta[, cols]
#       else meta = merge(t1$meta, t2$meta, all = TRUE, sort = FALSE)[, cols]
#     }
#   }
#   else meta = NULL
#   object <- textmeta(meta = meta, text = c(t1$text, t2$text),
#     metamult = c(t1$metamult, t2$metamult))
#   return(object)
# }