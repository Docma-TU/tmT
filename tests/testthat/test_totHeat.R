context("plot Topics as Heatmap including Clustering")

test_that("totHeat", {
  set.seed(123)
  x1 <- matrix(sample(c(rep(0, 20), 1:20), 10000, replace = TRUE), 10, 1000)
  ldaID <- paste("ID", 1:200)
  x2 <- list(document_sums = x1)
  
  text <- matrix(sample(paste("word", 1:100), 10000, replace = TRUE), 200, 50)
  text <- apply(text, 1, list)
  names(text) <- paste("ID", 101:300)
  
  words <- makeWordlist(text)$words
  LDAdoc <- docLDA(text, words)
  lda <- LDAstandard(documents = LDAdoc, K = 3L, vocab = words,
    num.iterations = 20L, burnin = 70L, seed = 123)
  
  meta1 <- as.Date(sample(1:730, 1200, replace = TRUE), origin = "1990-10-03")
  names(meta1) <- paste("ID", 1:1200)
  meta <- data.frame(id = paste("ID", 1:1200), date = meta1, 
    title = as.character(NA), stringsAsFactors = FALSE)
  
  obj <- textmeta(text = text, meta = meta)
  
  res1 <- totHeat(object = obj, ldaresult = lda, ldaid = ldaID, file = "abc.pdf")
  expect_equal(dim(res1), c(3, 4))
  res2 <- totHeat(object = obj, ldaresult = lda, ldaid = ldaID, unit = "month",
    file = "abc.pdf")
  expect_true(all(res2$date == seq(min(res2$date), max(res2$date), "month")))
  res3 <- totHeat(object = obj, ldaresult = lda, ldaid = ldaID, file = "abc.pdf",
    norm = TRUE)
  expect_equal(dim(res3), c(3, 4))
})
