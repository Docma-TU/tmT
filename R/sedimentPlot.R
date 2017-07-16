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
#' @param topicNo character string of used topicnumbers
#' @param Tnames Label for the topics
#' @param unit unit for \code{\link{round_date}}
#' @param \dots Further Statements passed to the plot function.
#' @return A pdf.
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export sedimentPlot

library(devtools)
dev_mode(on=T)
install_github("DoCMA-TU/tmT")
library(tmT)
library(tm)
library(lubridate)
dev_mode(on=F)
setwd("//129.217.206.11/DoCMA")
#memory.limit(size = 2^15)

load("projects/2017_Homogenitaetsmass/lda_gesamt1-k100i200b70s6668754.RData")
load("data/Spiegel/data/Spiegel-meta.RData")

x <- result
label <- paste("T",1:100)
usedTopics <- paste("T", c(22,3,8))
usedTopics <- NULL
unit="quarter"
sort=TRUE
threshold <- 0.05
color <- NULL
legendLimit=0.08
xunit="year"
legend="topleft"
peak <- 0.05

out <- sedimentPlot(x=result, ldaID=ldaID, usedTopics=NULL, label=NULL, threshold=NULL, meta=meta, unit="quarter", xunit="year", color=NULL, sort=TRUE, legend="topleft", legendLimit=0.06, peak=0.05)

x=result; ldaID=ldaID; usedTopics=NULL; label=NULL; threshold=NULL; meta=meta; unit="quarter"; xunit="year"; color=NULL; sort=TRUE; legend="topleft"; legendLimit=0; peak=0.03

sedimentPlot <- function(x, ldaID, usedTopics=NULL, label=NULL, threshold=NULL, meta, unit="quarter", xunit="year", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0, ...){
    if(is.null(color)) color <- RColorBrewer::brewer.pal(n=12, name="Paired")[c(2*(1:6),2*(1:6)-1)]
    if(is.null(label)) label <- 1:nrow(x$document_sums)

    IDmatch <- match(ldaID,meta$id)
    if(any(is.na(IDmatch))){stop("missing id's in meta")}
    x <- as.data.frame(t(x$document_sums))
    textDate <- lubridate::floor_date(meta$date[IDmatch], unit)
    x <- split(x=x, f=textDate)
    x <- sapply(x,colSums)
    x <- t(t(x)/colSums(x))
    rownames(x) <- label

    ## reduce to used topics
    if(!is.null(usedTopics)){
        if(is.numeric(usedTopics) | is.integer(usedTopics)){x <- x[usedTopics,]
                                                        }else{x <- x[match(usedTopics, label),]
                                                          }
    }

    ## reduce via threshold
    if(!is.null(threshold)){x <- x[apply(x,1, function(x)any(x>=threshold)),]}


    ## sort topics
    if(sort){topicVolume <- rowSums(x)
             x <- x[order(topicVolume, decreasing=FALSE),]}

    ## cumsum
    y <- apply(x,2, cumsum)
    y <- rbind(0,y)

    ## expand color vector if necessary
    if(length(color) < nrow(y)) color <- rep(color, ceiling(nrow(y)/length(color)))[1:nrow(x)]

    ## plotting
    par(las=2)
    plot(NULL, xlim=range(as.Date(colnames(x))), ylim=c(0,max(y)), ylab="", xlab="", xaxt="n")
    for(i in 1:(nrow(y)-1)){
        polygon(x=c(as.Date(colnames(y)), rev(as.Date(colnames(y)))), c(y[i,], rev(y[i+1,])), col=color[i])
    }
    axis(side=1, seq(min(as.Date(colnames(y))), max(as.Date(colnames(y))), by=xunit), labels=seq(min(as.Date(colnames(y))), max(as.Date(colnames(y))), by=xunit))

    ## label peaks
    if(peak>0){
        xPeak <- x>peak
        xPeaklower <- x>0.01
        for(i in 1:nrow(xPeak)){
            wPeak <- which(xPeak[i,])
            if(length(wPeak)==0)next
            if(length(wPeak)==1){ text(x=as.Date(names(wPeak)),y=(y[i,wPeak] + y[i+1,wPeak])/2, labels=rownames(y)[i+1])
                                  next}
            wPeakNew <- NULL
            j <- 1
            while(j <length(wPeak)){
                kold <- 0
                for(k in j:length(wPeak)){
                    if(any(x[i,(wPeak[j]+1):(wPeak[j+1]-1)]<0.01)){ wPeakNew <- c(wPeakNew, wPeak[j:k][which.max(x[i,wPeak[j:k]])])
                                                                    kold <- k
                                                                    j <- k+1
                                                                    break}
                    if(k==length(wPeak)){wPeakNew <- c(wPeakNew, wPeak[j:k][which.max(x[i,wPeak[j:k]])])
                                         j <- k+1}
                }
            }
            if(rev(wPeakNew)[1] != rev(wPeak)[1] & k==kold+1) wPeakNew <- c(wPeakNew, rev(wPeak)[1])
            text(x=as.Date(names(wPeakNew)),y=(y[i,wPeakNew] + y[i+1,wPeakNew])/2, labels=rownames(y)[i+1])
        }
    }

    ## legend
    if(!is.null(legend)){legend(x=legend, legend= rev(rownames(y)[-1][apply(x,1,function(z)any(z>legendLimit))]), bg="white", pch=15, col=rev(color[apply(x,1,function(z)any(z>legendLimit))]))}

    invisible(list(x=x,y=y))
}



