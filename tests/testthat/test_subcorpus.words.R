context("create subcorpus based on keyword search")

test_that("subcorpus.words", {
text <- list("abaabcaa", "ababc", c("aba", "aabc"), c("aa", "aab"))
wordlist1 <- c("aa", "bc")
wordlist2 <- c("aa")
counts <- 2

expect_error(subcorpus.words(counts, out="text"))

res <- subcorpus.words(text,wordlist1, counts, out="bin")
expect_equal(res, c(FALSE, FALSE, FALSE, TRUE))

res <- subcorpus.words(text,wordlist1, counts, out="count")
test <- matrix(c(1,0,1,2,1,1,1,0), nrow=4)
colnames(test) <- c("aa","bc")
expect_equal(res, test)

res <- subcorpus.words(text,wordlist2, counts, out="count")
test <- matrix(c(1,0,1,2), nrow=4)
colnames(test) <- "aa"
expect_equal(res, test)

res <- subcorpus.words(text,wordlist1, counts, out="text")
expect_equal(res, list(c("aa", "aab")))

res <- subcorpus.words(text,wordlist2, counts, out="text")
expect_equal(res, list(c("aa", "aab")))

})
