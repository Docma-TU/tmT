#' Subcorpus With Date Filter
#'
#' Creates a subcorpus including a specific time span.
#'
#'
#' @param text List of article texts
#' @param meta Dataframe of metadata
#' @param s.date Start date of subcorpus as date object
#' @param e.date End date of subcorpus as date object
#' @return Filtered list of texts.
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export subcorpusDate
subcorpusDate <- function(text, meta, s.date = min(meta$date), e.date = max(meta$date)){
    stopifnot((is.list(text) || is.character(text)), is.data.frame(meta),
              length(s.date) == 1, length(e.date) == 1)
    mtch <- match(names(text), meta$id)
    dateID <- meta$date[mtch] >= s.date & meta$date[mtch] <= e.date
    text <- text[dateID]
    return(text)
}

