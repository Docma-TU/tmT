context("clusterTopics")

test_that("clusterTopics", {

load("test-k3i20b70s24601.RData")

## cT <- clusterTopics(topics=result$topics, file="test.pdf", method = "average", width=30, height=15)
## cT2 <- clusterTopics(topics=result$topics, file="test.pdf", method = "single", width=30, height=15)
## save(cT,cT2,file="data/clusterTopics.RData")

load("data/clusterTopics.RData")

expect_equal(cT, clusterTopics(topics=result$topics, file="test.pdf", method = "average", width=30, height=15))
expect_equal(cT2, clusterTopics(topics=result$topics, file="test.pdf", method = "single", width=30, height=15))
})
