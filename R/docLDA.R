#' create lda-ready dataset
#'
#' This function transforms a text corpus like the result from
#' \code{\link{make.clear}} in a form needed by the lda-package.
#'
#'
#' @param corpus a list of tokenized texts.
#' @param vocab a charcter vector containing all words which should beused for
#' lda.
#' @param ldacorrect logical: If \code{TRUE}, every word repetition gets an own
#' column.
#' @param excludeNA logical: Should \code{NA}s be removed?
#' @param reduce logical: Should empty texts be deleted?
#' @return A list in which every entry contains a matrix with two rows: the
#' first row gives the number of the entry of the word in \code{vocab} minus
#' one, the second row the number of the occurrence of the word in the article.
#' If \code{ldacorrect=TRUE} the second row is 1 and the number of the
#' occurrence of the word will be shown by the number of columns belonging to
#' this word.
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export docLDA
docLDA <-
function(corpus, vocab, ldacorrect=TRUE, excludeNA=TRUE, reduce=TRUE){
  for(i in 1:length(corpus)){
    a <- table(corpus[[i]])
    a <- rbind(as.integer(match(names(a),vocab)-1),as.integer(a))
    ## a <- rbind(as.integer(apply(as.matrix(names(a),length(a),1),1,function(x)which(vocab==x))-1),as.integer(a))
    if(ldacorrect){
      corpus[[i]] <- matrix(as.integer(1),2,sum(a[2,]))
      corpus[[i]][1,] <- as.integer(unlist(apply(a,2,function(x){rep(x[1],each=x[2])})))
    }else{
      corpus[[i]] <- a
    }
    if(excludeNA){
      if(any(is.na(corpus[[i]][1,]))){
    corpus[[i]] <- corpus[[i]][,-which(is.na(corpus[[i]][1,]))]}}
    }
    if(reduce){
    DIM <- sapply(corpus, function(x)dim(x)[2])
    index1 <- which(sapply(DIM,is.null))
    if(length(index1)!=0){
    corpus <- corpus[-index1]
    DIM <- sapply(corpus, function(x)dim(x)[2])}
    corpus <- corpus[which(DIM>0)]
    }
  return(corpus)
}
