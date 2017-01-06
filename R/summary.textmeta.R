#' Print Summarys of Objects of class "textmeta".
#'
#' Prints a summary of a given Dataset, which should be of class "textmeta".
#'
#'
#' @param object A list of corpus data of class "textmeta".
#' @param list.names character: Which list.elements should be summarized?
#' @param ... additional arguments affecting the summary produced.
#' @return The dataset itself.
#' @author Jonas Rieger (<riegerjonas@@gmx.de>)
#' @keywords manip
#' @examples
#'
#' @export summary.textmeta
#'
summary.textmeta <- function(object, list.names = names(object), ...){
  stopifnot(is.list(object), is.character(list.names),
            all(list.names %in% names(object)),
            ifelse("text" %in% list.names && length(object$text) > 0,
                   is.list(object$text) || is.character(object$text), TRUE),
            ifelse("meta" %in% list.names && !is.null(nrow(object$meta)),
                   is.data.frame(object$meta), TRUE),
            ifelse("metamult" %in% list.names && !is.null(nrow(object$metamult)),
                   is.list(object$metamult), TRUE))
  nextprint <- paste(paste0(rep("-", 70), collapse = ""), "\n\n")
  # object$text:
  if ("text" %in% list.names && length(object$text) > 0){
    n.text <- length(object$text)
    na.text <- sum(is.na(object$text))
    # print:
    cat(paste("number of observations text:", n.text, "\n"))
    cat("\nNAs in text:\n")
    print(data.frame(abs = na.text, rel = na.text/n.text), row.names = FALSE)
  }
  # object$meta:
  if ("meta" %in% list.names && !is.null(nrow(object$meta))){
    cols <- colnames(object$meta)
    n.meta <- nrow(object$meta)
    na.meta <- sapply(object$meta, function(x) sum(is.na(x)))
    # print:
    cat(paste0(nextprint, "number of observations meta: ", n.meta, "\n"))
    cat("\nNAs in meta:\n")
    print(cbind(abs = na.meta, rel = na.meta/n.meta))
    # print date-range:
    cat(paste(nextprint, "range of date:\n"))
    if ("date" %in% cols){
      print(range(object$meta$date))
    }
    if ("datum" %in% cols){
      print(range(object$meta$datum))
    }
    # print tables of candidates
    candidates <- c("resource", "downloadDate")
    for (i in candidates){
      if (i %in% cols){
        tab <- table(object$meta[, i])
        cat(paste0(nextprint, i, ":\n"))
        print(cbind(abs = tab, rel = tab/n.meta))
      }
    }
  }
  if ("metamult" %in% list.names && !is.null(nrow(object$metamult))){
    n.metamult <- nrow(object$metamult)
    na.metamult <- sapply(object$metamult, function(x) sum(is.na(x)))
    cat(paste0(nextprint, "metamult:\n",
               "number of observations: ", n.metamult, "\n",
               "NAs in metamult:\n"))
    print(cbind(abs = na.metamult, rel = na.metamult/n.metamult))
  }
  invisible(object)
}