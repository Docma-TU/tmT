#' Get The IDs Of The Most Representive Articles
#'
#' Extract the article IDs belonging to the articles with the most relative or
#' absolute number of words per topic.
#'
#'
#' @param x LDA result
#' @param id Vector of text IDs
#' @param limit Integer, number of article ids per topic.
#' @param rel Logical: Should be the relative frequency be used?
#' @param topn Logical: Should only the topn Article IDs be reported?
#' \code{FALSE} is not implemented.
#' @param themes not implemented.
#' @param minlength Minimal total number of words a text must have to be
#' included.
#' @return Matrix of Article IDs.
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export toparticles
toparticles <- function(x, id, limit = 20L, rel = TRUE, topn = TRUE,
                        themes = 1:nrow(x$document_sums), minlength=30L){
  # returns for every theme the article ID of the article containing
  # more than minwords words of this theme
  # themes is not accessed in the function, delete?
  stopifnot(is.character(id), as.integer(limit) == limit, length(limit) == 1,
            is.logical(rel), length(rel) == 1, is.logical(topn),
            length(topn) == 1, as.integer(minlength) == minlength,
            length(minlength) == 1)
  small <- which(apply(x$document_sums, 2, sum) < minlength)
  if(length(small > 0)){x$document_sums <- x$document_sums[,-small]
  id <- id[-small]}
  if(rel){
    res <- t(t(x$document_sums) / apply(x$document_sums,2,sum))
  }else{res <- x$document_sums}
  if(topn){
    top <- apply(res, 1, function(x) order(x,decreasing=TRUE)[1:limit])
  }
  ## else{
  ## }
  res <- apply(top, 2, function(x) id[x])
  return(res)
}
