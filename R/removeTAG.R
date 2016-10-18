#' Removes XML Tags
#'
#'
#' @param text character string including XML-Tags.
#' @author Sakander Zirai (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#' @examples
#'
#' @export removeTAG
#'
removeTAG <- function(text){
  newtext  <- gsub(pattern = "<.*?>", replacement = "", text, perl = TRUE)
  return (newtext)
}
