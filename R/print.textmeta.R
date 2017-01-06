#' Prints Objects of class "textmeta".
#'
#' Prints a given Dataset, which should be of class "textmeta".
#'
#'
#' @param x A list of corpus data of class "textmeta".
#' @param ... additional arguments affecting the summary produced.
#' @return The dataset itself.
#' @author Jonas Rieger (<riegerjonas@@gmx.de>)
#' @keywords manip
#' @examples
#'
#' @export print.textmeta
#'
print.textmeta <- function(x, ...){
  stopifnot(is.list(x))
  cat("Object of class \"textmeta\":\n")
  # x$text:
  cat(paste(" number of observations in text:", length(x$text), "\n"))
  # x$meta:
  if (!is.null(nrow(x$meta)) && !is.null(ncol(x$meta))){
    cat(paste0(" meta: ", nrow(x$meta), " observations of ", ncol(x$meta),
               " variables\n"))
  }
  if ("metamult" %in% names(x) && length(x$metamult) > 0){
    cat(paste0(" metamult: ", sum(lengths(x$metamult[[1]])),
               " observations of ", length(x$metamult), " variables\n"))
  }
  # date-range:
  if ("date" %in% colnames(x$meta)){
    cat(paste0(" range of date: ",
               paste(range(x$meta$date), collapse = " till ")))
  }
  if ("datum" %in% colnames(x$meta)){
    cat(paste0(" range of date: ",
               paste(range(x$meta$datum), collapse = " till ")))
  }
  invisible(x)
}