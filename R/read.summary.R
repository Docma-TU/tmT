#' Print Summarys of Datasets.
#'
#' Prints a summary of a given dataset, which should be a result of a read-funtion.
#'
#'
#' @param data A list of corpus data as a result of a read-function.
#' @param list.names character: Which list.elements should be summarized?
#' @return The Dataset itself.
#' @author Jonas Rieger (<riegerjonas@@gmx.de>)
#' @keywords manip
#' @examples
#'
#' @export read.summary
#'
read.summary <- function(data, list.names = names(data)){
  stopifnot(is.list(data), is.character(list.names),
            all(list.names %in% names(data)),
            ifelse("text" %in% list.names && length(data$text) > 0,
                   is.list(data$text) || is.character(data$text), TRUE),
            ifelse("meta" %in% list.names && !is.null(nrow(data$meta)),
                   is.data.frame(data$meta), TRUE),
            ifelse("metamult" %in% list.names && !is.null(nrow(data$metamult)),
                   is.list(data$metamult), TRUE))
  nextprint <- paste(paste0(rep("-", 70), collapse = ""), "\n\n")
  # data$text:
  if ("text" %in% list.names && length(data$text) > 0){
    n.text <- length(data$text)
    na.text <- sum(is.na(data$text))
    # print:
    cat(paste("number of observations text:", n.text, "\n"))
    cat("\nNAs in text:\n")
    print(data.frame(abs = na.text, rel = na.text/n.text), row.names = FALSE)
  }
  # data$meta:
  if ("meta" %in% list.names && !is.null(nrow(data$meta))){
    cols <- colnames(data$meta)
    n.meta <- nrow(data$meta)
    na.meta <- sapply(data$meta, function(x) sum(is.na(x)))
    # print:
    cat(paste0(nextprint, "number of observations meta: ", n.meta, "\n"))
    cat("\nNAs in meta:\n")
    print(cbind(abs = na.meta, rel = na.meta/n.meta))
    # print date-range:
    cat(paste(nextprint, "range of date:\n"))
    if ("date" %in% cols){
      print(range(data$meta$date))
    }
    if ("datum" %in% cols){
      print(range(data$meta$datum))
    }
    # print tables of candidates
    candidates <- c("resource", "downloadDate")
    for (i in candidates){
      if (i %in% cols){
        tab <- table(data$meta[, i])
        cat(paste0(nextprint, i, ":\n"))
        print(cbind(abs = tab, rel = tab/n.meta))
      }
    }
  }
  if ("metamult" %in% list.names && !is.null(nrow(data$metamult))){
    n.metamult <- nrow(data$metamult)
    na.metamult <- sapply(data$metamult, function(x) sum(is.na(x)))
    cat(paste0(nextprint, "metamult:\n",
               "number of observations: ", n.metamult, "\n",
               "NAs in metamult:\n"))
    print(cbind(abs = na.metamult, rel = na.metamult/n.metamult))
  }
  return(data)
}