#' Plotting Topics Over Time
#'
#' Creates a pdf including a plot for each topic. For each topic the number of
#' words per month would be plotted.
#'
#'
#' @param x LDA result object
#' @param ldaID Character vector including IDs of the texts.
#' @param meta The meta data for the texts.
#' @param file Name of the pdf file.
#' @param Tnames Label for the topics
#' @param \dots Further Statements passed to the plot function.
#' @return A pdf.
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export topic.over.time
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
