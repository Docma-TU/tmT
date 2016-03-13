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
