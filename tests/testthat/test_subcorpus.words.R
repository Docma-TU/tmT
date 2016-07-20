context("subcorpus.words creates subcorpus based on keyword search")

test_that("subcorpus.words", {
text <- list("abaabcaa", "ababc", c("aba", "aabc"), c("aa", "aab", "bc"))
wordlist1 <- c("aa", "bc")
wordlist2 <- c("aa")

expect_error(subcorpus.words(counts, out="text"))

counts <- c(2,1)
res <- subcorpus.words(text,wordlist1, counts=counts, out="bin")
expect_equal(res, c(FALSE, FALSE, FALSE, TRUE))

res <- subcorpus.words(text,wordlist1, out="bin")
expect_equal(res, c(TRUE, FALSE, TRUE, TRUE))


res <- subcorpus.words(text,wordlist1, out="count")
test <- matrix(c(1,0,1,2,1,1,1,1), nrow=4)
colnames(test) <- c("aa","bc")
expect_equal(res, test)

res <- subcorpus.words(text,wordlist2, out="count")
test <- matrix(c(1,0,1,2), nrow=4)
colnames(test) <- "aa"
expect_equal(res, test)

res <- subcorpus.words(text,wordlist1, counts, out="text")
expect_equal(res, list(c("aa", "aab", "bc")))

res <- subcorpus.words(text,wordlist1, out="text")
expect_equal(res, list(c("abaabcaa"), c("aba","aabc"), c("aa", "aab", "bc")))

res <- subcorpus.words(text,wordlist2, counts=2, out="text")
expect_equal(res, list(c("aa", "aab", "bc")))

res <- subcorpus.words(text,wordlist2, counts=3, out="text")
expect_equal(res, list())

## ignore.case
expect_equal(subcorpus.words(text=list("a"),wordlist="A", out="text"), list())
expect_equal(subcorpus.words(text="a",wordlist="A", out="text"), character())
expect_equal(subcorpus.words(text="a",wordlist="A", out="text", ignore.case=TRUE), "a")
expect_equal(subcorpus.words(text=c("a","A"),wordlist="A", out="text", ignore.case=TRUE), c("a", "A"))

## counts
counts <- list(c(2,1), 3)
text2 <- list("abc", c("a","a","b"), c("c","c","c"))
res <- subcorpus.words(text2,wordlist=list(c("a","b"),"c"), counts=counts, out="bin")
expect_equal(res, c(FALSE, TRUE, TRUE))
})
