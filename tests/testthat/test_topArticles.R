context("topArticles")

test_that("topArticles", {

load("test-k3i20b70s24601alpha0.33eta0.33.RData")

M <- matrix(c(
"I","H","E",
"C","D","G",
"A","B","B",
"H","G","D",
"B","A","A"
),5,3,byrow=TRUE)

M2 <- matrix(c(
"I","G",
"C","A",
"A","B",
"H","E",
"F","D",
"B","H",
"D","C",
"E","F",
"G","I"),9,2,byrow=TRUE)

expect_equal(M,topArticles(ldaresult=result, ldaID=ldaID, limit = 5L, rel = TRUE, minlength=30L))
expect_equal(M2,topArticles(ldaresult=result, ldaID=ldaID, limit = 0L, rel = FALSE, select = c(1,3), minlength=1L))
})
