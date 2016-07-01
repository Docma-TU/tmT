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
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (x, id, limit = 20, rel = TRUE, topn = TRUE, themes = 1:nrow(x$document_sums),
#'     minlength = 30)
#' {
#'     small <- which(apply(x$document_sums, 2, sum) < minlength)
#'     if (length(small > 0)) {
#'         x$document_sums <- x$document_sums[, -small]
#'         id <- id[-small]
#'     }
#'     if (rel) {
#'         res <- t(t(x$document_sums)/apply(x$document_sums, 2,
#'             sum))
#'     }
#'     else {
#'         res <- x$document_sums
#'     }
#'     if (topn) {
#'         top <- apply(res, 1, function(x) order(x, decreasing = TRUE)[1:limit])
#'     }
#'     res <- apply(top, 2, function(x) id[x])
#'     return(res)
#'   }
#' 
#' @export toparticles
toparticles <-
function(x, id, limit=20, rel=TRUE, topn=TRUE, themes=1:nrow(x$document_sums), minlength=30){ # returns for every theme the article ID of the article containing more than minwords words of this theme
small <- which(apply(x$document_sums, 2, sum)<minlength)
if(length(small>0)){x$document_sums <- x$document_sums[,-small]
id <- id[-small]}
if(rel){
res <- t(t(x$document_sums) / apply(x$document_sums,2,sum))
}else{res <- x$document_sums}
if(topn){
top <- apply(res,1,function(x)order(x,decreasing=TRUE)[1:limit])
}
## else{
## }
res <- apply(top,2,function(x)id[x])
return(res)
}
