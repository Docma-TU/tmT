#' Count words in text corpora
#'
#' Creates a wordlist and a frequency table.
#'
#' This function helps, if \code{table(x)} needs too much RAM.
#'
#' @param x List of texts.
#' @param k Integer, how many Texts should be processed at one time (RAM
#' usage)?
#' @return \item{words}{An alphabetical list of the words in the corpus.}
#' \item{wordtable}{A frequency table of the words in the corpus.}
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export make.wordlist
make.wordlist <- function(x, k = 100000L){
  stopifnot(is.list(x), is.numeric(k), as.integer(k) == k, length(k) == 1)
  n <- length(x)
  (print(n))
  N <- 0:floor(n/k)
  words <- NULL
  for(i in N){
    (print(i*k))
    words <- c(words, unique(unlist(x[(i*k+1):(min(n, i*k+k))])))
  }
  words <- sort(unique(words))
  (print("table"))
  wordtable <- rep(0, length(words))
  names(wordtable) <- words
  for(i in N){
    (print(i*k))
    tmp <- table(unlist(x[(i*k+1):(min(n, i*k+k))]))
    mtch <- match(names(tmp), names(wordtable))
    wordtable[mtch] <- wordtable[mtch] + tmp
  }
  return(list(words = words, wordtable = wordtable))
}
