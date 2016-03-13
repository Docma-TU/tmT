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
