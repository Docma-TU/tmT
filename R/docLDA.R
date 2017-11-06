#' create lda-ready dataset
#'
#' This function transforms a text corpus like the result from
#' \code{\link{makeClear}} in a form needed by the lda-package.
#'
#'
#' @param text a list of tokenized texts.
#' @param vocab a character vector containing all words which should beused for
#' lda.
#' @param ldacorrect logical: If \code{TRUE}, every word repetition gets an own
#' column.
#' @param excludeNA logical: Should \code{NA}s be removed?
#' @param reduce logical: Should empty texts be deleted?
#' @return A list in which every entry contains a matrix with two rows: the
#' first row gives the number of the entry of the word in \code{vocab} minus
#' one, the second row the number of the occurrence of the word in the article.
#' If \code{ldacorrect=TRUE} the second row is 1 and the number of the
#' occurrence of the word will be shown by the number of columns belonging to
#' this word.
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export docLDA

docLDA <- function(text, vocab, ldacorrect = TRUE, excludeNA = TRUE,
  reduce = TRUE){
  
  stopifnot(is.textmeta(textmeta(text = text)), is.character(vocab),
    is.logical(ldacorrect), is.logical(excludeNA), is.logical(reduce),
    length(ldacorrect) == 1, length(excludeNA) == 1, length(reduce) == 1)
  text <- lapply(text, unlist)
  text <- lapply(text, table)
  text <- lapply(text, function(x)
    rbind(as.integer(match(names(x), vocab) - 1), as.integer(x)))
  if(ldacorrect) text <- lapply(text, function(x)
    rbind(as.integer(rep(x[1, ], x[2, ])), as.integer(rep(1, sum(x[2, ])))))
  if(excludeNA) text <- lapply(text, function(x) x[, !is.na(x[1, ])])
  tmp <- lengths(lapply(text, dim)) == 0
  text[tmp] <- lapply(text[tmp], as.matrix)
  if(reduce){
    # delete entries where dimension is not computable
    tmp <- lengths(lapply(text, dim)) == 0
    if (length(tmp) > 0) text <- text[!tmp]
    # reducing
    text <- text[sapply(text, dim)[2,] != 0]
  }
  return(text)
}
