context("read JFArchiv files")
#setwd("/Users/SuiOni/Development/Docma/tmT/tests/testthat")

test_that("readJFArchiv", {
  JFA2 <- readJFArchiv(path=paste0(getwd(),"/data/JFArchiv"))
  text2 <- readJFArchiv(path=paste0(getwd(),"/data/JFArchiv"), do.meta = FALSE, do.text = TRUE)
  meta2 <- readJFArchiv(path=paste0(getwd(),"/data/JFArchiv"), do.meta = TRUE, do.text = FALSE)
  
  load("data/JFA-compare.RData")
  expect_equal(JFA2, JFA)
  expect_equal(text2, text)
  expect_equal(meta2, meta)
})



