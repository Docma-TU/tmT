#' Cluster Analysis
#'
#' Makes a cluster analysis with the hellinger distance.
#'
#'
#' @param topics A matrix like the \code{topic} matrix from
#' \code{lda.collapse.gibbs.sampler}
#' @param file File for the dendogram pdf.
#' @param topicnames Character vector as label for the topics.
#' @param method Method statement from \code{\link{hclust}}
#' @param width grafical parameter for pdf output. See \code{\link{pdf}}
#' @param heigth grafical parameter for pdf output. See \code{\link{pdf}}
#' @param ... additional parameter for \code{\link{plot}}
#' @return A dendogram as pdf and a List \item{dist}{A distance matrix}
#' \item{clust}{The result from \code{hclust}.}
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export clusterTopics
clusterTopics <- function(topics, file, topicnames=NULL, method = "average", width=30, height=15, ...){
    if(is.null(topicnames)) topicnames <- 1:nrow(topics)
    topics <- topics/rowSums(topics)
    topics <- sqrt(topics)
    Dist <- 1/sqrt(2) * dist(topics)
    attr(Dist, "method") <- "hellinger"
    clust <- hclust(d=Dist, method)
    pdf(file, width, height)
    plot(clust, label=topicnames, ...)
    dev.off()
    invisible(list(dist=Dist, cluster=clust))
}
