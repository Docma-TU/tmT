#' Transform textmeta to an object with tidy text data
#'
#' Transfers data from a text component of a \code{\link{textmeta}} object to a
#' tidy data.frame.
#'
#' @param object A \code{\link{textmeta}} object
#' @return An object with tidy text data
#' @keywords manip
#' @export tidy.textmeta

tidy.textmeta <- function(object){
  stopifnot(is.textmeta(object))
  emptyText <- lengths(object$text) == 0
  if (any(emptyText)){
    message("Deleting ", sum(emptyText),  " empty texts...")
    object$text <- object$text[!emptyText]
    object$meta <- object$meta[object$meta %in% names(object$text)]
  }
  if (all(lengths(object$text) == 1)){
    dat <- data.frame(id = names(object$text),
      text = unlist(object$text), stringsAsFactors = FALSE)
  }else{
    dat <- data.frame(id = rep(names(object$text), times = lengths(object$text)),
      tokenid = unlist(lapply(object$text, seq_along)),
      token = unlist(object$text), stringsAsFactors = FALSE)
  }
  row.names(dat) <- 1:nrow(dat)
  object$text <- dat
  class(object) <- "textmeta (tidy text)"
  return(object)
}
