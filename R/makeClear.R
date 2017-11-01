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
#' @param sw list of stopwords.
#' @param paragraph Logical: Should be set to \code{TRUE} if the article is a
#' list of character strings, representing the paragraphs.
#' @return A list containing the preprocessed Article.
#' @keywords manip
#' @import graphics
#' @import grDevices
#' @import stats
#' @import utils
#' @import tm
#' @import lda
#' @import lubridate
#' @import ggplot2
#' @import ggdendro
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export makeClear
makeClear <- function(object, text, sw = "en", paragraph = FALSE){
  
  if(sw %in% c("de", "ge", "german"))
    sw <- c(tm::stopwords("german"), "dass", "fuer", "koennen", "koennte",
      "ueber", "waehrend", "wuerde", "wuerden")
  else sw <- tm::stopwords(sw)
  
  returnTextmeta <- FALSE
  if(!missing(object)){
    text <- object$text
    returnTextmeta <- TRUE
  }
  stopifnot(is.list(text), is.character(sw), is.logical(paragraph),
    length(paragraph) == 1)
  cat("punctuation \n")
  text <- lapply(text, tm::removePunctuation, preserve_intra_word_dashes = FALSE)
  cat("numbers \n")
  text <- lapply(text, tm::removeNumbers)
  cat("to lower \n")
  text <- lapply(text, tolower)
  cat("stopwords \n")
  text <- lapply(text, tm::removeWords, sw)
  cat("whitespace \n")
  text <- lapply(text, tm::stripWhitespace)
  text <- lapply(text, trimws)
  if(paragraph){ ## SPIEGEL
    cat("tokenization \n")
    text <- lapply(text, strsplit, "\\s")
    cat("remove empty article \n")
    cat(paste("remove empty articles:",sum(lengths(text) == 0 | is.na(text)), "\n"))
    text <- text[!(lengths(text) == 0 | is.na(text))]
    cat("remove empty paragraphs \n")
    text <- lapply(text, function(x) x[!(lengths(x) == 0 | is.na(x))])
    cat(paste("remove empty articles (2):",sum(lengths(text) == 0), "\n"))
    text <- text[!(lengths(text) == 0)]
  }
  else{
    cat("tokenization \n")
    text <- sapply(text, function(x) strsplit(x, "\\s")[1])
    cat("remove empty article \n")
    cat(paste("Empty Articles:",sum(lengths(text) == 0 | is.na(text)), "\n"))
    text <- text[!(lengths(text) == 0 | is.na(text))]
  }
  if(returnTextmeta){
    object$text <- text
    object$meta <- object$meta[object$meta$id %in% names(object$text), ]
    return(object)
  }
  return(text)
}
