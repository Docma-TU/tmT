topic.over.time <-
function(x, ldaID, meta, file, Tnames, ...){
pdf(file=file, width=12)
tmp <- x$document_sums
tmpdate <- meta$datum[match(ldaID,meta$id)]
tmp2 <- round_date(tmpdate, "month")
for(j in 1:nrow(tmp)){
splt <- split(tmp[j,],tmp2)
splt <- sapply(splt,sum)
if(length(Tnames)>1){plot(as.Date(names(splt)),splt, main=paste("Topic", j,":",Tnames[j],sep=" "), type="l")}else{
plot(as.Date(names(splt)),splt, main=paste("Topic", j,":",Tnames,sep=" "), type="l")}
}
dev.off()
}
