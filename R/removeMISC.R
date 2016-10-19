#' Removes trailing and leading whitespaces and Original Gesamtseiten-PDF
#'
#'
#' @param text character string including XML-Tags.
#' @author Sakander Zirai (<s.zirai@@live.de>)
#' @keywords manip
#' @examples
#'
#' @export removeMISC
#'
removeMISC <- function(text){
  newtext  <- gsub(pattern = "^\\s+|\\s+$|Original Gesamtseiten-PDF", replacement = "", text, perl = TRUE) 
  return (newtext)
}
