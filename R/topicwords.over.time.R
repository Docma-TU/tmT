#' Plotting Topicwords Over Time
#' 
#' Creates a pdf including a plot for each choosen word. For each word the
#' number of this word per month in the whole corpus and in the choosen topic
#' would be plotted.
#' 
#' 
#' @param lda.corpus Corpus of type lda.
#' @param meta The meta data for the texts
#' @param lda.vocab Character vector of the lda vocabulary
#' @param lda.result LDA result object
#' @param woerter List of word vectors.
#' @param topics Same structure like \code{woerter}. Each Entry gives the
#' number of the topic for the word at the same position in \code{woerter}.
#' @param file Name of the pdf file
#' @param lda.name Name of the lda, as a lable for the pdf output.
#' @return A pdf.
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (lda.corpus, meta, lda.vocab, lda.result, woerter, topics,
#'     file, lda.name)
#' {
#'     pdf(file = file, width = 12)
#'     for (i in 1:length(topics)) {
#'         wid <- match(woerter[[i]], lda.vocab)
#'         wid <- wid - 1
#'         tw <- lapply(lda.result$assignment, function(x) which(x ==
#'             (as.numeric(topics[i]) - 1)))
#'         for (j in 1:length(tw)) {
#'             tw[[j]] <- lda.corpus[[j]][1, tw[[j]]]
#'         }
#'         for (j in 1:length(wid)) {
#'             tmp <- sapply(lda.corpus, function(x) sum(x[1, ] ==
#'                 wid[j]))
#'             tmp2 <- sapply(tw, function(x) sum(x == wid[j]))
#'             tmpdate <- meta$datum[match(names(tmp), meta$id)]
#'             tmpdate <- round_date(tmpdate, "month")
#'             splt1 <- split(tmp, tmpdate)
#'             splt1 <- sapply(splt1, sum)
#'             splt2 <- split(tmp2, tmpdate)
#'             splt2 <- sapply(splt2, sum)
#'             plot(as.Date(names(splt1)), splt1, main = paste(lda.name,
#'                 "Topic", topics[i], woerter[[i]][j], sep = " "),
#'                 type = "l", ylim = c(0, max(splt1)))
#'             lines(as.Date(names(splt1)), splt2, col = "red",
#'                 type = "l")
#'         }
#'     }
#'     dev.off()
#'   }
#' 
#' @export topicwords.over.time
topicwords.over.time <-
function(lda.corpus, meta, lda.vocab, lda.result, woerter, topics, file, lda.name){
pdf(file=file, width=12)
for(i in 1:length(topics)){
wid <- match(woerter[[i]], lda.vocab)
wid <- wid-1
tw <- lapply(lda.result$assignment, function(x)which(x==(as.numeric(topics[i])-1)))
for(j in 1:length(tw)){
tw[[j]] <- lda.corpus[[j]][1,tw[[j]]]
}
for(j in 1:length(wid)){
tmp <- sapply(lda.corpus, function(x)sum(x[1,]==wid[j]))
tmp2 <- sapply(tw, function(x)sum(x==wid[j]))
tmpdate <- meta$datum[match(names(tmp),meta$id)]
tmpdate <- round_date(tmpdate, "month")
splt1 <- split(tmp,tmpdate)
splt1 <- sapply(splt1,sum)
splt2 <- split(tmp2,tmpdate)
splt2 <- sapply(splt2,sum)
plot(as.Date(names(splt1)),splt1, main=paste(lda.name,"Topic", topics[i], woerter[[i]][j],sep=" "), type="l", ylim=c(0,max(splt1)))
lines(as.Date(names(splt1)),splt2, col="red", type="l")
}}
dev.off()
}
