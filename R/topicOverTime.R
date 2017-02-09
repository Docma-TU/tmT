#' Plotting Topics Over Time
#'
#' Creates a pdf including a plot for each topic. For each topic the number of
#' words per month would be plotted.
#'
#'
#' @param x LDA result object
#' @param ldaID Character vector including IDs of the texts.
#' @param meta The meta data for the texts or a date-string.
#' @param file Name of the pdf file.
#' @param Tnames Label for the topics
#' @param unit unit for \code{\link{round_date}}
#' @param \dots Further Statements passed to the plot function.
#' @return A pdf.
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export topicOverTime
#'
topicOverTime <- function(x, ldaID, meta=NULL, file, topicNo=NULL, Tnames=NULL, unit= "month", ...){
    if(is.list(x)) x <- x$document_sums
    if(is.data.frame(meta)){tmpdate <- meta$date; names(tmpdate) <- meta$id}else tmpdate <- meta
    if(is.null(topicNo)) topicNo <- seq_along(x[,1])
    if(is.null(Tnames)) Tnames <-  paste("Topic", topicNo, sep=" ") else Tnames <- paste("Topic", topicNo,":",Tnames[topicNo],sep=" ")
    outlist <- list()
    pdf(file=file, width=12)
    tmpdate <- tmpdate[match(ldaID,names(tmpdate))]
    tmp2 <- lubridate::round_date(tmpdate, unit = unit)
    for(j in topicNo){
        splt <- split(x[j,],tmp2)
        splt <- sapply(splt,sum)
        plot(as.Date(names(splt)),splt, main=Tnames[j], type="l")
        outlist <- c(outlist, list(cbind(as.Date(names(splt)),splt)))
    }
    dev.off()
    names(outlist) <- Tnames
    invisible(outlist)
}

Tnames <- paste("A", 1:10)
