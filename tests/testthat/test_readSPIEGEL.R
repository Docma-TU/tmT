context("read SPIEGEL files")

test_that("readSPIEGEL", {

SPIEGEL2 <- readSPIEGEL(year=2012:2013)
text2 <- readSPIEGEL(year=2012:2013, do.meta = FALSE, do.text = TRUE)
meta2 <- readSPIEGEL(year=2012:2013, do.meta = TRUE, do.text = FALSE)

load("tests/testthat/SP_compare.Rdata")
expect_equal(SPIEGEL2, SPIEGEL)
expect_equal(text2, text)
expect_equal(meta2, meta)
})
