#' Get The IDs Of The Most Representive Articles
#'
#' Extract the article IDs belonging to the articles with the most relative or
#' absolute number of words per topic.
#'
#'
#' @param ldaresult LDA result
#' @param ldaid Vector of text IDs
#' @param limit Integer, number of article ids per topic.
#' @param rel Logical: Should be the relative frequency be used?
#' @param select which topics should be returned?
#' @param minlength Minimal total number of words a text must have to be
#' included.
#' @return Matrix of Article IDs.
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export topArticles
topArticles <- function(ldaresult, ldaid, limit = 20L, rel = TRUE,
  select = 1:nrow(ldaresult$document_sums), minlength = 30L){
    stopifnot(is.character(ldaid), as.integer(limit) == limit, length(limit) == 1,
              is.logical(rel), length(rel) == 1,
              as.integer(minlength) == minlength,
              length(minlength) == 1)

    small <- apply(ldaresult$document_sums, 2, sum) >= minlength
    ldaresult$document_sums <- ldaresult$document_sums[,small]
    ldaid <- ldaid[small]

    if(rel)
      res <- t(t(ldaresult$document_sums) / colSums(ldaresult$document_sums))
    else res <- ldaresult$document_sums
    res <- res[select, ]
    if(limit)
      res <- apply(res, 1, function(x) order(x,decreasing=TRUE)[1:limit])
    else res <- apply(res, 1, function(x) order(x,decreasing=TRUE))
    res <- apply(res, 2, function(x) ldaid[x])
    return(res)
}