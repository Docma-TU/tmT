#' Some data preprocessing
#'
#' Removes punctuation, numbers and stopwords, change into lowercase letters
#' and tokenization.
#'
#' Removes punctuation, numbers and stopwords, change into lowercase letters
#' and tokenization. Additional some cleaning steps: remove empty words /
#' paragraphs / article.
#'
#' @param object \code{\link{textmeta}} object
#' @param text not necassary if \code{object} is specified, else should be
#' \code{object\$text}: list of article texts
#' @param sw \code{character} vector of stopwords. If the vector is of length
#' one, \code{sw} is interpretated as argument for \code{\link{stopwords}}
#' @param paragraph Logical: Should be set to \code{TRUE} if one article is a
#' list of character strings, representing the paragraphs.
#' @param lowercase Logical: Should be set to \code{TRUE} if all letters should
#' be coerced to lowercase.
#' @param rmPunctuation Logical: Should be set to \code{TRUE} if punctuation should
#' be removed from articles.
#' @param rmNumbers Logical: Should be set to \code{TRUE} if numbers should
#' be removed from articles.
#' @param checkUTF8 Logical: Should be set to \code{TRUE} if articles should
#' be tested on UTF-8 - which is package standard.
#' @return A list containing the preprocessed Article.
#' @keywords manip
#' @import graphics
#' @import grDevices
#' @import stats
#' @import utils
#' @import tm
#' @import lda
#' @import lubridate
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export makeClear
makeClear <- function(object, text, sw = "en", paragraph = FALSE,
  lowercase = TRUE, rmPunctuation = TRUE, rmNumbers = TRUE, checkUTF8 = TRUE){

  if(length(sw) == 1){
    if(sw %in% c("de", "ge", "german"))
      sw <- c(tm::stopwords("german"), "dass", "fuer", "koennen", "koennte",
        "ueber", "waehrend", "wuerde", "wuerden")
    else sw <- tm::stopwords(sw)
  }
  returnTextmeta <- FALSE
  if(!missing(object)){
    stopifnot(is.textmeta(object))
    text <- object$text
    returnTextmeta <- TRUE
  }
  stopifnot(is.textmeta(textmeta(text = text)), is.character(sw),
    is.logical(paragraph), length(paragraph) == 1,
    all(is.logical(c(lowercase, rmPunctuation, rmNumbers))))
  cat("Check Articles on UTF8: ")
  if(checkUTF8){
    stopifnot(all(unlist(lapply(text, function(x) lapply(x, validUTF8)))))
    cat("next Step\n")
  }
  else cat("skip\n")
  cat("Change to Lowercase: ")
  if(lowercase){
    text <- lapply(text, tolower)
    cat("next Step\n")
  }
  else cat("skip\n")
  cat("Remove Punctuation: ")
  if(rmPunctuation){
    text <- lapply(text, tm::removePunctuation, preserve_intra_word_dashes = FALSE)
    cat("next Step\n")
  }
  else cat("skip\n")
  cat("Remove Numbers: ")
  if(rmNumbers){
    text <- lapply(text, tm::removeNumbers)
    cat("next Step\n")
  }
  else cat("skip\n")
  cat("Remove Stopwords: ")
  text <- lapply(text, tm::removeWords, sw)
  cat("next Step\nRemove redundant Whitespace: ")
  text <- lapply(text, tm::stripWhitespace)
  text <- lapply(text, trimws)
  cat("next Step\n")
  if(paragraph){
    cat("Tokenize: ")
    text <- lapply(text, strsplit, "\\s")
    cat(paste("next Step\nRemove empty Articles: ",
      sum(lengths(text) == 0 | is.na(text))))
    text <- text[!(lengths(text) == 0 | is.na(text))]
    cat(" next Step\nRemove empty Paragraphs: ")
    text <- lapply(text, function(x) x[!(lengths(x) == 0 | is.na(x))])
    cat(paste(" next Step\nRemove empty Articles (2): ", sum(lengths(text) == 0)))
    text <- text[!(lengths(text) == 0)]
  }
  else{
    cat("Tokenize: ")
    text <- sapply(text, function(x) strsplit(x, "\\s")[1])
    cat(paste("next Step\nRemove empty Articles: ",
      sum(lengths(text) == 0 | is.na(text))))
    text <- text[!(lengths(text) == 0 | is.na(text))]
  }
  cat(" Success")
  if(returnTextmeta){
    object$text <- text
    object$meta <- object$meta[object$meta$id %in% names(object$text), ]
    return(object)
  }
  return(text)
}
