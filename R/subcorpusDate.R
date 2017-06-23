#' Subcorpus With Date Filter
#'
#' Creates a subcorpus including a specific time span.
#'
#' @param object \code{\link{textmeta}} object
#' @param text not necassary if \code{object} is specified, else should be
#' \code{object\$text}
#' @param meta not necassary if \code{object} is specified, else should be
#' \code{object\$meta}
#' @param s.date Start date of subcorpus as date object
#' @param e.date End date of subcorpus as date object
#' @return \code{\link{textmeta}} object if \code{object} is specified,
#' else only the filtered \code{text}.
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export subcorpusDate
subcorpusDate <- function(object, text = object$text, meta = object$meta,
  s.date = min(meta$date, na.rm = TRUE),
  e.date = max(meta$date, na.rm = TRUE)){
  
  returnTextmeta <- TRUE
  if(missing(object)){
    object <- textmeta(meta = meta, text = text)
    returnTextmeta <- FALSE
  }
  stopifnot(is.textmeta(object), length(s.date) == 1, length(e.date) == 1)
  object$meta <- object$meta[match(names(object$text), object$meta$id),]
  ind <- object$meta$date >= s.date & object$meta$date <= e.date
  ind[is.na(ind)] <- FALSE
  object$meta <- object$meta[ind, ]
  object$text <- object$text[match(object$meta$id, names(object$text))]
  if(returnTextmeta) return(object)
  return(object$text)
}

