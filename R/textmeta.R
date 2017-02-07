#' "textmeta"-Objects
#'
#' Creates "textmeta"-Objects.
#'
#' @param meta data.frame of the meta-data.
#' @param text list or character of the text-data.
#' @param metamult list of the metamult-data.
#' @return a "textmeta"-Object.
#' @keywords manip
#' @examples
#'
#' @export textmeta
textmeta <- function(meta = NULL, text = NULL, metamult = NULL){
  stopifnot(is.null(meta) || is.data.frame(meta),
            is.null(text) || is.list(text) || is.character(text),
            is.null(metamult) || is.list(metamult))
  res <- list(meta = meta, text = text, metamult = metamult)
  class(res) <- "textmeta"
  return(res)
}

#' "textmeta"-Objects
#'
#' Tests for "textmeta"-Objects.
#'
#' @param x an R Object.
#' @return TRUE or FALSE.
#'
#' @export is.textmeta
is.textmeta <- function(x){
  return(all(class(x) == "textmeta",
             is.null(x$meta) || is.data.frame(x$meta),
             is.null(x$text) || is.list(x$text) || is.character(x$text),
             is.null(x$metamult) || is.list(x$metamult)))
}

#' @export
print.textmeta <- function(x, ...){
  stopifnot(is.textmeta(x))
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
    na.date <- is.na(x$meta$date)
    if (any(!na.date)){
      cat(paste0(" range of date: ", paste(range(x$meta$date, na.rm = TRUE),
                                           collapse = " till ")), "\n")
    }
    if (any(na.date)){
      cat(paste0(" NAs in date: ", sum(na.date), " (",
                 round(mean(na.date), 2), ")\n"))
    }
  }
  ## if ("datum" %in% colnames(x$meta)){
  ##   na.date <- is.na(x$meta$datum)
  ##   if (any(!na.date)){
  ##     cat(paste0(" range of date: ", paste(range(x$meta$datum, na.rm = TRUE),
  ##                                          collapse = " till "), "\n"))
  ##   }
  ##   if (any(na.date)){
  ##     cat(paste0(" NAs in date: ", sum(na.date), " (", mean(na.date, 2), ")"))
  ##   }
  ## }
  invisible(x)
}

#' @export
summary.textmeta <- function(object, list.names = names(object),
                             candidates = c("resource", "downloadDate"), ...){
  stopifnot(is.textmeta(object), is.character(list.names),
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
    cat(paste("number of observations in text:", n.text, "\n"))
    cat("\nNAs in text:\n")
    print(data.frame(NA.abs = na.text,
                     NA.rel = round(na.text/n.text, 2)), row.names = FALSE)
  }
  # object$meta:
  if ("meta" %in% list.names && !is.null(nrow(object$meta))){
    cols <- colnames(object$meta)
    n.meta <- nrow(object$meta)
    na.meta <- sapply(object$meta, function(x) sum(is.na(x)))
    # print:
    cat(paste0(nextprint, "meta: ", nrow(object$meta), " observations of ",
               ncol(object$meta), " variables\n"))
    cat("\nNAs in meta:\n")
    print(cbind(abs = na.meta, rel = round(na.meta/n.meta, 2)))
    # print tables (not NA-table) of candidates
    for (i in candidates){
      if (i %in% cols){
        tab <- table(object$meta[, i])
        cat(paste0(nextprint, i, ":\n"))
        print(cbind(abs = tab, rel = round(tab/n.meta, 2)))
      }
    }
    # print date-range:
    if ("date" %in% cols){
      cat(nextprint)
      if (na.meta["date"] < n.meta){
        cat(paste0("range of date: ",
                   paste(range(object$meta$date, na.rm = TRUE),
                         collapse = " till "), "\n"))
      }
      if (na.meta["date"] > 0){
        cat(paste0("NAs in date: ", na.meta["date"], " (",
                   round(na.meta["date"]/n.meta, 2), ")\n"))
      }
    }
    ## if ("datum" %in% cols){
    ##   cat(nextprint)
    ##   if (na.meta["datum"] < n.meta){
    ##     cat(paste0("range of date: ",
    ##                paste(range(object$meta$datum, na.rm = TRUE),
    ##                      collapse = " till "), "\n"))
    ##   }
    ##   if (na.meta["datum"] > 0){
    ##     cat(paste0("NAs in date: ", na.meta["datum"], " (",
    ##                round(na.meta["datum"]/n.meta, 2), ")\n"))
    ##   }
    ## }
  }
  if ("metamult" %in% list.names && length(object$metamult) > 0){
    n.metamult <- sapply(object$metamult, function(x) sum(lengths(x)))
    na.metamult <- sapply(object$metamult, function(x) sum(is.na(x)))
    cat(paste0(nextprint, "metamult:\n"))
    print(cbind(n = n.metamult, NA.abs = na.metamult,
                NA.rel = ifelse(n.metamult > 0, round(na.metamult/n.metamult, 2), 0)))
  }
  invisible(object)
}
