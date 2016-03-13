make.readable <-
function(corpus,id, file){
for(i in 1:ncol(id)){
mtch1 <- match(id[,i],names(corpus$text))
mtch2 <- match(id[,i],corpus$meta$id)
out <- lapply(corpus$text[mtch1],paste,collapse=" ")
out <- unlist(out)
## out <- unlist(corpus$text[mtch1])
out2 <- cbind(corpus$meta$id[mtch2],corpus$meta$titel[mtch2],as.character(corpus$meta$datum[mtch2]),out)
colnames(out2) <- c("ID","Titel","Datum","Text")
rownames(out2) <- 1:length(out)
write.csv(out2, file=paste(file,i,"lesen.csv",sep=""))
}}
