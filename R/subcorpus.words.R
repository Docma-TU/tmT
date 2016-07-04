#' Subcorpus With Word Filter
#' 
#' Creates a subcorpus including specific words.
#' 
#' 
#' @param text List of article texts
#' @param wordlist List of character vectors. Every List element is an 'or'
#' link, every character String in a vector is linked by an 'and'. If
#' \code{wordlist} is only a character Vector the link is 'or'.
#' @param counts Integer, or same list structure like \code{wordlist}. Number
#' of times a word must appear to be counted.
#' @param ignore.case Option from \code{\link{grepl}}.
#' @param perl Option from \code{\link{grepl}}.
#' @param fixed Option from \code{\link{grepl}}.
#' @param useBytes Option from \code{\link{grepl}}.
#' @return Filtered list of texts.
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)%% ~~who you are~~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (text, wordlist, counts = 1, ignore.case = FALSE, perl = FALSE,
#'     fixed = FALSE, useBytes = FALSE)
#' {
#'     subid <- numeric(length(text))
#'     for (i in 1:length(wordlist)) {
#'         tmp <- NULL
#'         for (j in 1:length(wordlist[[i]])) {
#'             if (length(counts) == 1) {
#'                 co <- counts
#'             }
#'             else {
#'                 co <- counts[[i]][j]
#'             }
#'             tmp <- cbind(tmp, sapply(text, function(x) sum(grepl(pattern = wordlist[[i]][j],
#'                 x = x, ignore.case = ignore.case, perl = perl,
#'                 fixed = fixed, useBytes = useBytes)) >= co))
#'         }
#'         subid <- subid + apply(tmp, 1, prod)
#'     }
#'     subid <- subid > 0
#'     return(text[subid])
#'   }
#' 
#' @export subcorpus.words
subcorpus.words <-
function(text, wordlist, counts=1, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE){
subid <- numeric(length(text))
    for(i in 1:length(wordlist)){
tmp <- NULL
    for(j in 1:length(wordlist[[i]])){
if(length(counts)==1){co <- counts}else{co <- counts[[i]][j]}
        tmp <- cbind(tmp,sapply(text, function(x)sum(grepl(pattern=wordlist[[i]][j], x=x, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))>=co))
}
subid <- subid + apply(tmp,1,prod)
}
subid <- subid>0
return(text[subid])
}
