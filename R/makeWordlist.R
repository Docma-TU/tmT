#' Counts Words in Text Corpora
#'
#' Creates a wordlist and a frequency table.
#'
#' This function helps, if \code{table(x)} needs too much RAM.
#'
#' @param text List of texts.
#' @param k Integer: How many texts should be processed at once (RAM
#' usage)?
#' @return \item{words}{An alphabetical list of the words in the corpus}
#' \item{wordtable}{A frequency table of the words in the corpus}
#' @keywords manip
#' @examples
#' texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
#' Teach a Man To Fish, and You Feed Him for a Lifetime",
#' B="So Long, and Thanks for All the Fish",
#' C="A very able manipulative mathematician, Fisher enjoys a real mastery
#' in evaluating complicated multiple integrals.")
#'
#' texts <- makeClear(text=texts)
#' makeWordlist(text=texts, k = 2L)
#'
#' @export makeWordlist
makeWordlist <- function(text, k = 100000L){
  stopifnot(is.textmeta(textmeta(text = text)),
    is.numeric(k), as.integer(k) == k, length(k) == 1)
  n <- length(text)
  cat(paste("Number of Articles:", n,"\n"))
  cat("Find out Vocabularies...\n Done:\n")
  N <- 0:floor(n/k)
  words <- NULL
  for(i in N){
    cat("  ", i*k, "\n")
    words <- c(words, unique(unlist(text[(i*k+1):(min(n, i*k+k))])))
  }
  cat("  ", n, " next Step\n")
  words <- sort(unique(words))
  cat("Calculate Counts...\n Done:\n")
  wordtable <- rep(0, length(words))
  names(wordtable) <- words
  for(i in N){
    cat("  ", i*k, "\n")
    tmp <- table(unlist(text[(i*k+1):(min(n, i*k+k))]))
    mtch <- match(names(tmp), names(wordtable))
    wordtable[mtch] <- wordtable[mtch] + tmp
  }
  cat("  ", n, " Success\n")
  return(list(words = words, wordtable = wordtable))
}
