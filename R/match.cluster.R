#' Preparation of different LDAs For Clustering
#'
#' Merges different lda-results to one matrix, including only the words which
#' appears in all lda.
#'
#'
#' @param x A list of lda results.
#' @return A matrix including all topics from all lda-results. Number of rows
#' is the number of topics, the number of columns is the number of words which
#' appear in all results.
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export match.cluster
match.cluster <-
function(x){
n <- length(x)
x <- lapply(x,function(x)x$topics)
vocab <- table(unlist(sapply(x,colnames)))
vocab <- names(vocab)[which(vocab==n)]
mtch <- lapply(x,function(x)match(vocab, colnames(x)))
res <- NULL
for(i in 1:length(x))res <- rbind(res,x[[i]][,mtch[[i]]])
res <- res/rowSums(res)
return(res)}
