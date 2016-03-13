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
