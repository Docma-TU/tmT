#' Export Readable Article Lists
#'
#' Exports the article id, text, title and date.
#'
#'
#' @param corpus A corpus of type "data"
#' @param id Vector or matrix inkluding article id's.
#' @param file Filename for the export.
#' @return No output in R, only a csv including the requested articles.
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export make.readable
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
