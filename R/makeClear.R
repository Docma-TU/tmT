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
makeClear <- function(x, sw = c(stopwords("german"), "dass", "fuer", "koennen",
                              "koennte", "ueber", "waehrend", "wuerde",
                              "wuerden"), paragraph = TRUE){
    stopifnot(is.list(x), is.character(sw), is.logical(paragraph),
              length(paragraph) == 1)
    cat("punctuation \n")
    x <- lapply(x, removePunctuation, preserve_intra_word_dashes = FALSE)
    cat("numbers \n")
    x <- lapply(x, removeNumbers)
    cat("to lower \n")
    x <- lapply(x, tolower)
    cat("stopwords \n")
    x <- lapply(x, removeWords, sw)
    cat("whitespace \n")
    x <- lapply(x, stripWhitespace)
    x <- lapply(x, trimws)
    if(paragraph){ ## SPIEGEL
        cat("tokenization \n")
        x <- lapply(x, strsplit, "\\s")
        cat("remove empty article \n")
        cat(paste("remove empty articles:",sum(lengths(x) == 0 | is.na(x)), "\n"))
        x <- x[!(lengths(x) == 0 | is.na(x))]
        cat("remove empty paragraphs \n")
        x <- lapply(x, function(x) x[!(lengths(x) == 0 | is.na(x))])
        cat(paste("remove empty articles (2):",sum(lengths(x) == 0), "\n"))
        x <- x[!(lengths(x) == 0)]
    }
    else{
        cat("tokenization \n")
        x <- sapply(x, function(x)strsplit(x, "\\s")[1])
        cat("remove empty article \n")
        cat(paste("Empty Articles:",sum(lengths(x) == 0 | is.na(x)), "\n"))
        x <- x[!(lengths(x) == 0 | is.na(x))]
    }
    return(x)
}
