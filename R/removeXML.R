#' Removes XML tags and umlauts
#'
#' Removes XML tags and changes umlauts to a standardized form.
#'
#'
#' @param x List of texts.
#' @param xml Logical: Should XML-TAGs be removed?
#' @param umlaute Logical: Should umlauts be changed?
#' @param u.type Type of umlaut changing: \code{normal} for normal umlauts (\enc{ü}{u}
#' -> ue), \code{html} for html representation of umlauts (&uuml; -> ue) and
#' \code{all} for both.
#' @return Adjusted corpus
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#' @examples
#'
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export removeXML

removeXML <-
function(x,xml=TRUE, umlauts=FALSE, u.type=c("normal", "html", "all")){
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
            x <- gsub(pattern="&[^;]*;", replacement="",x)}

        if(u.type=="normal" | u.type[1]=="all"){
            x <- gsub(pattern="\U00E4", replacement="ae",x)
            x <- gsub(pattern="\U00C4", replacement="Ae",x)
            x <- gsub(pattern="\U00F6", replacement="oe",x)
            x <- gsub(pattern="\U00D6", replacement="Oe",x)
            x <- gsub(pattern="\U00FC", replacement="ue",x)
            x <- gsub(pattern="\U00DC", replacement="Ue",x)
            x <- gsub(pattern="\U00DF", replacement="ss",x)}
    }
    x <- trimws(x)
    return(x)
}
