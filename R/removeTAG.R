#' Removes XML Tags
#'
#'
#' @param text character string including XML-Tags.
#' @author Sakander Zirai (<s.zirai@@live.de>)
#' @keywords manip
#' @examples
#'
#' @export removeTAG
#'
removeTAG <- function(text){
    newtext <- gsub(pattern = "<.*?>", replacement = "", text, perl = TRUE)
    newtext <- trimws(newtext)
  return (newtext)
}
