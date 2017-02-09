#' Plotting Topicwords Over Time
#'
#' Creates a pdf including a plot for each choosen word. For each word the
#' number of this word per month in the whole corpus and in the choosen topic
#' would be plotted.
#'
#'
#' @param ldaCorpus Corpus of type lda.
#' @param meta The meta data for the texts
#' @param ldaVocab Character vector of the lda vocabulary
#' @param ldaResult LDA result object
#' @param words List of word vectors.
#' @param topics Same structure like \code{woerter}. Each Entry gives the
#' number of the topic for the word at the same position in \code{woerter}.
#' @param file Name of the pdf file
#' @param ldaName Name of the lda, as a label for the pdf output.
#' @param unit unit for\code{\link{round_date}} 
#' @param \dots additional parameter for \code{\link{plot}} 
#' @return A pdf.
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export topicwordsOverTime
#'
#'
topicwordsOverTime <- function(ldaCorpus, meta, ldaVocab, ldaResult, words, topics, file, ldaName, unit="month", ...){
    if(is.data.frame(meta)){tmp <- meta$date; names(tmp) <- meta$id; meta <- tmp}
    outlist <- list()
    pdf(file=file, width=12)
    
    for(i in 1:length(topics)){
      wordid <- match(words[i], ldaVocab) -1
      wordintopic <- lapply(ldaResult$assignment, grepl, pattern=topics[i])
      
        for(j in 1:length(wordintopic)){
          wordintopic[[j]] <- ldaCorpus[[j]][1,wordintopic[[j]]]
        }

  wordcount <- sapply(ldaCorpus, function(x)sum(x[1,]==wordid))
  wordcountTopic <- sapply(wordintopic, function(x)sum(x==wordid))
  tmpdate <- meta[match(names(wordcount),names(meta))]
  tmpdate <- round_date(tmpdate, unit)
  
  splt1 <- split(wordcount,tmpdate)
  splt1 <- sapply(splt1,sum)
  splt2 <- split(wordcountTopic,tmpdate)
  splt2 <- sapply(splt2,sum)
  plot(as.Date(names(splt1)),splt1, main=paste(ldaName,"Topic", topics[i], words[i],sep=" "), type="l", ylim=c(0,max(splt1)), ...)
  lines(as.Date(names(splt1)),splt2, col="red", type="l")
  outlist <- c(outlist, list(cbind(splt1, splt2)))
        }
    dev.off()
    invisible(outlist)
}


