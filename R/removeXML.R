#' Removes XML tags and umlauts
#'
#' Removes XML tags and changes umlauts to a standardized form.
#'
#'
#' @param x \code{character} vector.
#' @param xml \code{logical} Should XML-TAGs be removed?
#' @param umlauts \code{logical}: Should umlauts be changed?
#' @param u.type Type of umlaut changing: \code{normal} for normal umlauts (\enc{ü}{u}
#' -> ue), \code{html} for html representation of umlauts (&uuml; -> ue) and
#' \code{all} for both.
#' @param remove.html Logical: Should Numeric character references beeing deleted (after replacing umlauts etc.)
#' @details The decision which u.type is used should consider the language of the corpus, because in some languages the replacement of umlauts can change the meaning of the a word.
#' To change which columns are used by removeXML use argument xmlAction in \code{\link{readTextmeta}}.
#' @return Adjusted corpus
#' @keywords manip
#' @examples
#'
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export removeXML

removeXML <- function(x, xml = TRUE, umlauts = FALSE,
                      u.type = c("normal", "html", "all"), remove.html=TRUE){
  if(xml){
    x <- gsub(pattern="<[^><]*>", replacement=" ",x, perl=TRUE)}
  if(umlauts){
    if(u.type[1]=="html" | u.type[1]=="all"){
      x <- gsub(pattern="&auml;", replacement="ae",x)
      x <- gsub(pattern="&Auml;", replacement="Ae",x)
      x <- gsub(pattern="&ouml;", replacement="oe",x)
      x <- gsub(pattern="&Ouml;", replacement="Oe",x)
      x <- gsub(pattern="&uuml;", replacement="ue",x)
      x <- gsub(pattern="&Uuml;", replacement="Ue",x)
      x <- gsub(pattern="&szlig;", replacement="ss",x)
      for (i in c(base::letters, base::LETTERS)){
        x <- gsub(pattern=paste0("&", i, "grave;"), replacement=i, x)
        x <- gsub(pattern=paste0("&", i, "acute;"), replacement=i, x)
        x <- gsub(pattern=paste0("&", i, "circ;"), replacement=i, x)
        x <- gsub(pattern=paste0("&", i, "tilde;"), replacement=i, x)
      }
      if(remove.html)x <- gsub(pattern="&[^;]*;", replacement="",x)
    }
    if(u.type[1]=="normal" | u.type[1]=="all"){
      x <- gsub(pattern="\U00E4", replacement="ae",x)
      x <- gsub(pattern="\U00C4", replacement="Ae",x)
      x <- gsub(pattern="\U00F6", replacement="oe",x)
      x <- gsub(pattern="\U00D6", replacement="Oe",x)
      x <- gsub(pattern="\U00FC", replacement="ue",x)
      x <- gsub(pattern="\U00DC", replacement="Ue",x)
      x <- gsub(pattern="\U00DF", replacement="ss",x)
    }
  }
  x <- trimws(x)
  return(x)
}
