context("topArticles")

test_that("topArticles", {

load("test-k3i20b70s24601.RData")

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

expect_equal(M,topArticles(x=result, id=ldaID, limit = 5L, rel = TRUE, themes = 1:nrow(x$document_sums), minlength=30L))
expect_equal(M2,topArticles(x=result, id=ldaID, limit = 9L, rel = FALSE, themes = c(1,3), minlength=1L))
})
