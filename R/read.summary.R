#' Print Summarys of Datasets.
#'
#' Prints a summary of a given dataset, which should be a result of a read-funtion.
#'
#'
#' @param data A list of corpus data as a result of a read-function.
#' @param metamult Logical: Should the function return a summary of data$metamult?
#' @return The Dataset itself.
#' @author Jonas Rieger (<riegerjonas@@gmx.de>)
#' @keywords manip
#' @examples
#'
#' @export read.summary
#'
read.summary <- function(data, metamult = "metamult" %in% colnames(data)){
  stopifnot(is.list(data), all(c("text", "meta") %in% names(data)),
            is.character(data$text), is.data.frame(data$meta),
            is.logical(metamult), length(metamult) == 1)
  # data$text:
  n.text <- length(data$text)
  na.text <- sum(is.na(data$text))
  # data$meta:
  cols <- colnames(data$meta)
  n.meta <- nrow(data$meta)
  na.meta <- sapply(data$meta, function(x) sum(is.na(x)))
  # print summary:
  cat("number of observations:\n")
  print(data.frame(n.text, n.meta), row.names = FALSE)
  cat("\nNAs in text:\n")
  print(data.frame(abs = na.text, rel = na.text/n.text), row.names = FALSE)
  cat("\nNAs in meta:\n")
  print(cbind(abs = na.meta, rel = na.meta/n.meta))
  nextprint <- paste(paste0(rep("-", 70), collapse = ""), "\n\n")
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
  if (metamult){
    n.metamult <- nrow(data$metamult)
    na.metamult <- sapply(data$metamult, function(x) sum(is.na(x)))
    cat(paste0(nextprint, "metamult:\n",
               "number of observations: ", n.metamult, "\n",
               "NAs in metamult:\n"))
    print(cbind(abs = na.metamult, rel = na.metamult/n.metamult))
  }
  return(data)
}