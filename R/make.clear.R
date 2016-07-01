#' Some data preprocessing
#' 
#' Removes punctuation, numbers and stopwords, change into lowercase letters
#' and tokenization.
#' 
#' Removes punctuation, numbers and stopwords, change into lowercase letters
#' and tokenization. Additional some cleaning steps: remove empty words /
#' paragraphs / article.
#' 
#' @param x list containing character strings.
#' @param sw list of stopwords.
#' @param paragraph Logical: Should be set to \code{TRUE} if the article is a
#' list of character strings, representing the paragraphs.
#' @return A list containing the preprocessed Article.
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (x, sw = c(stopwords("german"), "dass", "fuer", "koennen",
#'     "koennte", "ueber", "waehrend", "wuerde", "wuerden"), paragraph = TRUE)
#' {
#'     require(tm)
#'     (print("punctuation"))
#'     x <- lapply(x, removePunctuation, preserve_intra_word_dashes = FALSE)
#'     (print("numbers"))
#'     x <- lapply(x, removeNumbers)
#'     (print("to lower"))
#'     x <- lapply(x, tolower)
#'     (print("stopwords"))
#'     x <- lapply(x, removeWords, sw)
#'     (print("whitespace"))
#'     x <- lapply(x, stripWhitespace)
#'     if (paragraph) {
#'         (print("tokenization"))
#'         x <- lapply(x, strsplit, " ")
#'         (print("remove empty words"))
#'         x <- lapply(x, function(x) {
#'             lapply(x, function(y) {
#'                 if (length(which(y == "") > 0)) {
#'                   y[-which(y == "")]
#'                 }
#'                 else {
#'                   y
#'                 }
#'             })
#'         })
#'         (print("remove empty article"))
#'         x <- x[!(lengths(x) == 0)]
#'         (print("remove empty paragraphs"))
#'         x <- lapply(x, function(x) x[!(lengths(x) == 0)])
#'         (print("remove empty article 2"))
#'         x <- x[!(lengths(x) == 0)]
#'     }
#'     else {
#'         (print("tokenization"))
#'         x <- sapply(x, strsplit, " ")
#'         (print("remove empty words"))
#'         x <- lapply(x, function(y) {
#'             if (length(which(y == "") > 0)) {
#'                 y[-which(y == "")]
#'             }
#'             else {
#'                 y
#'             }
#'         })
#'         (print("remove empty article"))
#'         emptyArt <- which(sapply(x, function(x) {
#'             length(x) == 0
#'         }))
#'         (print(length(emptyArt)))
#'         if (length(emptyArt) > 0) {
#'             x <- x[-emptyArt]
#'         }
#'         emptyArt <- sapply(x, function(x) !is.na(x)[1])
#'         (print(sum(!emptyArt)))
#'         x <- x[emptyArt]
#'     }
#'   }
#' 
#' @export make.clear
make.clear <-
function(x, sw=c(stopwords("german"),"dass", "fuer","koennen","koennte","ueber","waehrend","wuerde","wuerden"), paragraph=TRUE){
(print("punctuation"))
x <- lapply(x,removePunctuation, preserve_intra_word_dashes = FALSE)
(print("numbers"))
x <- lapply(x,removeNumbers)
(print("to lower"))
x <- lapply(x,tolower)
(print("stopwords"))
x <- lapply(x,removeWords, sw)
(print("whitespace"))
x <- lapply(x,stripWhitespace)
if(paragraph){ ## SPIEGEL
(print("tokenization"))
x <- lapply(x,strsplit," ")
(print("remove empty words"))
x <- lapply(x, function(x){lapply(x, function(y){if(length(which(y=="")>0)){y[-which(y=="")]}else{y}})}) # remove empty words
(print("remove empty article"))
x <- x[!(lengths(x)==0)]
(print("remove empty paragraphs"))
x <- lapply(x, function(x)x[!(lengths(x)==0)])
## x <- lapply(x, function(y){y[sapply(y,function(z){length(z)>0})]}) # remove empty paragraphs
(print("remove empty article 2"))
x <- x[!(lengths(x)==0)]
}else{
(print("tokenization"))
x <- sapply(x,strsplit," ")
(print("remove empty words"))
x <- lapply(x, function(y){if(length(which(y=="")>0)){y[-which(y=="")]}else{y}}) # remove empty words
(print("remove empty article"))
emptyArt <- which(sapply(x,function(x){length(x)==0}))
(print(length(emptyArt)))
if(length(emptyArt)>0){
x <- x[-emptyArt]}
emptyArt <- sapply(x,function(x)!is.na(x)[1])
(print(sum(!emptyArt)))
x <- x[emptyArt]
}
}
