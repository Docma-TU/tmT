context("merge textmeta objects")

test_that("mergeTextmeta", {
  abc <- textmeta(text = list(a = "abcd", b = c("abcd", "de"), y = "xyz"))
  abc2 <- textmeta(
    text = list(a = "abcd", b = c("abcd", "de"), y = "xyz"),
    meta = data.frame(id = c("a", "b", "y"),
      date = as.Date(c("2017-01-01", "2017-01-01", "2017-01-01")),
      title = c("abc", "bed", "yxy"), stringsAsFactors = FALSE))
  m1 <- mergeTextmeta(abc, abc)
  m2 <- mergeTextmeta(abc, abc2)
  m3 <- mergeTextmeta(abc2, abc)
  m4 <- mergeTextmeta(abc2, abc2)
  expect_true(
    all(is.textmeta(m1), is.textmeta(m2), is.textmeta(m3), is.textmeta(m4)))
  expect_error(mergeTextmeta(abc, NULL))
  expect_equal(mergeTextmeta(abc, abc2, all = FALSE), m1)
})
