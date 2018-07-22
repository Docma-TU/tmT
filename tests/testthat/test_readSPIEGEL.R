context("read SPIEGEL files")

test_that("readSPIEGEL", {
  SP2 <- readSPIEGEL(path=paste0(getwd(),"/data/Spiegel"))
  text2 <- readSPIEGEL(path=paste0(getwd(),"/data/Spiegel"), do.meta = FALSE, do.text = TRUE)
  meta2 <- readSPIEGEL(path=paste0(getwd(),"/data/Spiegel"), do.meta = TRUE, do.text = FALSE)

  load("data/SP_compare.RData")
  expect_equal(SP2, SP)
  expect_equal(text2, text)
  expect_equal(meta2, meta)
})
