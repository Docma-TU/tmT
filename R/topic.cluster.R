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
#' @return A dendogram as pdf and a List \item{dist}{A distance matrix}
#' \item{clust}{The result from \code{hclust}.}
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export topic.cluster
topic.cluster <-
function(topics, file, topicnames=1:nrow(topics), method = "average"){
topics <- topics/rowSums(topics)
topics <- sqrt(topics)
Dist <- 1/sqrt(2) * dist(topics)
clust <- hclust(d=Dist, method)
pdf(file, width=30, height=15)
plot(clust, label=topicnames)
dev.off()
return(list(Dist, clust))
}
