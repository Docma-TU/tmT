#' Plotting Sediment plot of topics over time 
#'
#' Creates a sediment plot of all or selected topics.
#'
#'
#' @param ldaresult LDA result object
#' @param ldaid Character vector including IDs of the texts
#' @param select Selects all topics if parameter is null. Otherwise vector of integers or topic label. Only topics belonging to that numbers, and labels respectively would be plotted.
#' @param label Character vector of topic labels. Must have same length than number of topics in the model.
#' @param threshold Numeric treshold between 0 and 1. Topics would only be used if at least one time unit exist with a topic proportion abov the treshold
#' @param meta The meta data for the texts or a date-string.
#' @param unit Time unit for x-axis. Possible units see \code{\link[lubridate]{round_date}}
#' @param xunit Time unit for tiks on the x-axis. Possible units see \code{\link[lubridate]{round_date}}
#' @param color Color vector. Color vector would be replicated if the number of plotted topics is bigger than length of the vector.
#' @param sort logical. Should the topics be sorted by topic proportion?
#' @param legend Poisition of legend. If \code{NULL} (default), no legend will be plotted   
#' @param legendLimit numeric between 0 (default) and 1. Only Topics with proportions abov this limit appear in the legend.
#' @param peak numeric between 0 (default) and 1. Label peaks abov \code{peak}. For each Topic every area which are at least once above \code{peak} will e labeled. An area ends if the topic proportion is under 1 percent. 
#' @return list of two matrices. \code{x} contains the topic proportions over time, \code{y} contains the cumulated topic proportions
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export sedimentPlot

sedimentPlot <- function(ldaresult, ldaid, select=NULL, label=NULL, threshold=NULL, meta, unit="quarter", xunit="year", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0){
    if(is.null(color)) color <- RColorBrewer::brewer.pal(n=12, name="Paired")[c(2*(1:6),2*(1:6)-1)]
    if(is.null(label)) label <- 1:nrow(ldaresult$document_sums)

    IDmatch <- match(ldaid,meta$id)
    if(any(is.na(IDmatch))){stop("missing id's in meta")}
    x <- as.data.frame(t(ldaresult$document_sums))
    textDate <- lubridate::floor_date(meta$date[IDmatch], unit)
    x <- split(x=x, f=textDate)
    x <- sapply(x,colSums)
    x <- t(t(x)/colSums(x))
    rownames(x) <- label

    ## reduce to used topics
    if(!is.null(select)){
        if(is.numeric(select) | is.integer(select)){x <- x[select,]
                                                        }else{x <- x[match(select, label),]
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

