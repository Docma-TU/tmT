#' Removes XML tags and umlauts
#'
#' Removes XML tags and changes umlauts to a standardized form.
#'
#'
#' @param x List of texts.
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
function(x,umlaute=FALSE, u.type=c("normal", "html", "all")){
(print("<[^>]*>"))
x <- lapply(x,function(x){gsub(pattern="<[^>]*>", replacement=" ",x, perl=TRUE)})
if(umlaute){
if(u.type[1]=="html" || u.type[1]=="all"){
(print("\U00E4"))
x <- lapply(x,function(x){gsub(pattern="&auml;", replacement="ae",x)})
(print("\U00C4"))
x <- lapply(x,function(x){gsub(pattern="&Auml;", replacement="Ae",x)})
(print("\U00F6"))
x <- lapply(x,function(x){gsub(pattern="&ouml;", replacement="oe",x)})
(print("\U00D6"))
x <- lapply(x,function(x){gsub(pattern="&Ouml;", replacement="Oe",x)})
(print("\U00FC"))
x <- lapply(x,function(x){gsub(pattern="&uuml;", replacement="ue",x)})
(print("\U00DC"))
x <- lapply(x,function(x){gsub(pattern="&Uuml;", replacement="Ue",x)})
(print("\U00DF"))
x <- lapply(x,function(x){gsub(pattern="&szlig;", replacement="ss",x)})
(print("&;"))
x <- lapply(x,function(x){gsub(pattern="&[^;]*;", replacement="",x)})}
if(u.type=="normal" || u.type[1]=="all"){
(print("\U00E4"))
x <- lapply(x,function(x){gsub(pattern="ä", replacement="ae",x)})
(print("\U00C4"))
x <- lapply(x,function(x){gsub(pattern="Ä", replacement="Ae",x)})
(print("\U00F6"))
x <- lapply(x,function(x){gsub(pattern="ö", replacement="oe",x)})
(print("\U00D6"))
x <- lapply(x,function(x){gsub(pattern="Ö", replacement="Oe",x)})
(print("\U00FC"))
x <- lapply(x,function(x){gsub(pattern="ü", replacement="ue",x)})
(print("\U00DC"))
x <- lapply(x,function(x){gsub(pattern="Ü", replacement="Ue",x)})
(print("\U00DF"))
x <- lapply(x,function(x){gsub(pattern="ß", replacement="ss",x)})
}
## if(u.type=="normal" || u.type[1]=="all"){
## (print("\U00E4"))
## x <- lapply(x,function(x){gsub(pattern="\U00E4", replacement="ae",x)})
## (print("\U00C4"))
## x <- lapply(x,function(x){gsub(pattern="\U00C4", replacement="Ae",x)})
## (print("\U00F6"))
## x <- lapply(x,function(x){gsub(pattern="\U00F6", replacement="oe",x)})
## (print("\U00D6"))
## x <- lapply(x,function(x){gsub(pattern="\U00D6", replacement="Oe",x)})
## (print("\U00FC"))
## x <- lapply(x,function(x){gsub(pattern="\U00FC", replacement="ue",x)})
## (print("\U00DC"))
## x <- lapply(x,function(x){gsub(pattern="\U00DC", replacement="Ue",x)})
## (print("\U00DF"))
## x <- lapply(x,function(x){gsub(pattern="\U00DF", replacement="ss",x)})
## }
}
return(x)
}
