context("subcorpusWord creates subcorpus based on keyword search")

test_that("subcorpusWord", {
  text <- list("abaabcaa", "ababc", c("aba", "aabc"), c("aa", "aab", "bc"))
  wordlist1 <- c("aa", "bc")
  wordlist2 <- c("aa")

  expect_error(subcorpusWord(counts, out = "text"))

  counts <- c(2,1)
  res <- subcorpusWord(text, search=wordlist1, out = "bin")
  expect_equal(res, rep(TRUE, 4))

  res <- subcorpusWord(text, list(data.frame(pattern=wordlist1, word=FALSE, count=counts)), out = "bin")
  expect_equal(res, c(TRUE, FALSE, FALSE, TRUE))

  res <- subcorpusWord(text, search=wordlist1, out = "count")
  test <- matrix(c(2,0,1,2,1,1,1,1), nrow=4)
  colnames(test) <- c("aa","bc")
  expect_equal(res, test)

  res <- subcorpusWord(text, wordlist2, out="count")
  test <- matrix(c(2,0,1,2), nrow=4)
  colnames(test) <- "aa"
  expect_equal(res, test)

  res <- subcorpusWord(text, search=data.frame(pattern=wordlist1, count=counts, word=FALSE), out = "text")
  expect_equal(res, list("abaabcaa", c("aa", "aab", "bc")))

  res <- subcorpusWord(text, search= list(data.frame(pattern=wordlist1[1], count=counts[1], word=FALSE), data.frame(pattern=wordlist1[2], count=counts[2], word=FALSE)), out = "text")
  expect_equal(res, text)

  res <- subcorpusWord(text, data.frame(pattern=wordlist2, count=2, word=FALSE), out = "text")
  expect_equal(res, list("abaabcaa", c("aa", "aab", "bc")))

  res <- subcorpusWord(text, data.frame(pattern=wordlist2, count=3, word=FALSE), out = "text")
  expect_equal(res, list())

  ## ignore.case
  expect_equal(subcorpusWord(text=list("a"), search="A", out="text"), list())
  expect_equal(subcorpusWord(text="a", search="A", out="text"), list())
  expect_equal(subcorpusWord(text="a", search="A", out="text", ignore.case=TRUE), list("a"))
  expect_equal(subcorpusWord(text=list("a","A"), search="A", out="text", ignore.case=TRUE), list("a", "a"))
  expect_equal(subcorpusWord(text=c("a","A"), search="a", out="text", ignore.case=TRUE), list("a", "a"))

  ## counts
  counts <- list(c(2,1), 3)
  text2 <- list("abc", c("a","a","b"), c("c","c","c"))
  res <- subcorpusWord(text2, search=list(data.frame(pattern=c("a","b"), count=c(2,1), word=c(FALSE, FALSE)), data.frame(pattern="c", count=3, word=FALSE)), out="bin")
  expect_equal(res, c(FALSE, TRUE, TRUE))

  ## words
  text <- list("abaabcaa", "ababc", c("aba", "aabc"), c("aa", "aab", "bc"))
  search1 <- data.frame(pattern=c("aa", "bc"), word=c(FALSE, TRUE), count=1)
  expect_equal(subcorpusWord(text=text, search=search1, out="bin"), c(FALSE, FALSE, FALSE, TRUE))

  text <- list("abaabcaa", "ababc", c("aba", "aabc"), c("aab", "bc"))
  search1 <- data.frame(pattern=c("aa", "bc"), word=c(TRUE, TRUE), count=1)
  expect_equal(subcorpusWord(text=text, search=search1, out="bin"), c(FALSE, FALSE, FALSE, FALSE))

  text <- list("abaabcaa", "aab")
  search1 <- data.frame(pattern=c("aab"), word=c(TRUE), count=1)
  expect_equal(subcorpusWord(text=text, search=search1, out="bin"), c(FALSE, TRUE))

  text <- list("abaabcaa", "ababc", c("aba", "aabc"), c("aa", "aab", "bc"))
  search1 <- data.frame(pattern=c("aab", "a", "bc", "ab"), word=c(TRUE, FALSE, TRUE, TRUE), count=1)
  tr <- matrix(c(0,0,0,1,5,2,4,4,0,0,0,1,0,0,0,0), nrow=4)
  colnames(tr) <- c("aab_w", "a", "bc_w", "ab_w")
  expect_equal(subcorpusWord(text=text, search=search1, out="count"),tr)

  search1 <- data.frame(pattern=c("aab", "a", "bc", "ab"), word=c("word", "pattern", "right", "left"), count=1)
  tr <- matrix(c(0,0,0,1,5,2,4,4,0,1,1,1,1,1,1,0), nrow=4)
  colnames(tr) <- c("aab_w", "a", "bc_r", "ab_l")
  expect_equal(subcorpusWord(text=text, search=search1, out="count"),tr)

  search1 <- data.frame(pattern=c("aab", "a", "bc", "ab"), word=c("word", "pattern", "pattern", "pattern"), count=1)
  tr <- matrix(c(0,0,0,1,5,2,4,4,1,1,1,1,2,2,2,1), nrow=4)
  colnames(tr) <- c("aab_w", "a", "bc", "ab")
  expect_equal(subcorpusWord(text=text, search=search1, out="count"),tr)
})
