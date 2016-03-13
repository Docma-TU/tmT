toparticles <-
function(x, id, limit=20, rel=TRUE, topn=TRUE, themes=1:nrow(x$document_sums), minlength=30){ # returns for every theme the article ID of the article containing more than minwords words of this theme
small <- which(apply(x$document_sums, 2, sum)<minlength)
if(length(small>0)){x$document_sums <- x$document_sums[,-small]
id <- id[-small]}
if(rel){
res <- t(t(x$document_sums) / apply(x$document_sums,2,sum))
}else{res <- x$document_sums}
if(topn){
top <- apply(res,1,function(x)order(x,decreasing=TRUE)[1:limit])
}
## else{
## }
res <- apply(top,2,function(x)id[x])
return(res)
}
