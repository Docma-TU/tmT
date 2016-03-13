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
