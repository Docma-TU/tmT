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
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export subcorpus.date
subcorpus.date <-
function(text, meta,s.date=min(meta$datum), e.date=max(meta$datum)){
mtch <- match(names(text),meta$id)
dateID <- which(meta$datum[mtch]>=s.date & meta$datum[mtch]<=e.date)
text <- text[dateID]
return(text)
}
