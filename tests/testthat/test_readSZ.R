context("read SZ files")

test_that("readSZ", {
  SZ2 <- readSZ(path=paste0(getwd(),"/data/SZ"))
  text2 <- readSZ(path=paste0(getwd(),"/data/SZ"), do.meta = FALSE, do.text = TRUE)
  meta2 <- readSZ(path=paste0(getwd(),"/data/SZ"), do.meta = TRUE, do.text = FALSE)

  load("data/SZ_compare.RData")
  expect_equal(SZ2, SZ)
  expect_equal(text2, text)
  expect_equal(meta2, meta)
})

  ## SZ <- readSZ(path=paste0(getwd(),"/data/SZ"))
  ## text <- readSZ(path=paste0(getwd(),"/data/SZ"), do.meta = FALSE, do.text = TRUE)
  ## meta <- readSZ(path=paste0(getwd(),"/data/SZ"), do.meta = TRUE, do.text = FALSE)
  ## save(list=c("SZ", "text", "meta"), file="data/SZ_compare.RData")
