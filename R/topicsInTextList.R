#' @title Creates list of topics to which most words of a text were assigned
#' @description For all texts of a given subcorpus, the function creates a list of topics 
#' to which most and second-most words from a text were assigned to and
#' the share of words.
#'
#' @param text The result of \code{\link{LDAprep}}
#' @param ldaID List of IDs for \code{text}
#' @param scIDs List of IDs for which topic assignment will be given 
#' @param ldaresult A result object from the \code{standardLDA}
#' @param label Optional label for each topic
#' @param vocab Character: Vector of \code{vocab} corresponding to the \code{text} object
#' @export topicsInTextList
#' @return TopicWordAssignment: matrix of texts ids and the topic most words were assigned to + share 

topicsInTextList <- function (text, ldaID, scIDs, ldaresult, label = NULL, vocab) 
{

TopicMostWords <-  rep(1, length(scIDs))
ShareWords <- rep(1, length(scIDs))
TopicMostWords2 <-  rep(1, length(scIDs))
ShareWords2 <- rep(1, length(scIDs))
returnID <- rep(1, length(scIDs))

for (i in 1:length(scIDs)){
  id = scIDs[i]
  texttopic <- data.frame(word = vocab[text[[id]][1, ] + 1], 
                              topic = result$assignments[[which(ldaID == id)]],
                              stringsAsFactors = FALSE)
  texttopic$topic <- texttopic$topic +1
  
  topictable <- sort(table(texttopic$topic), decreasing = TRUE)
  #topictable[i]
  #sum(topictable)
  #names(topictable)[i]
  
  TopicMostWords[i] <- names(topictable)[1]
  ShareWords[i] <- topictable[1]/sum(topictable)
  TopicMostWords2[i] <- names(topictable)[2]
  ShareWords2[i] <- topictable[2]/sum(topictable)
  returnID[i] <- id
  TopicWordAssignment <- cbind(returnID, TopicMostWords, ShareWords, TopicMostWords2, ShareWords2) #ALT: [i,]
}
#write.csv(TopicWordAssignment, file = paste(folder, "-TopicAssignment",".csv", sep = ""), fileEncoding = "UTF-8")
invisible(TopicWordAssignment)
}
