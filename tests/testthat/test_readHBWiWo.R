context("read HB or WiWo files")

test_that("readHBWiWo", {
HBWiWo2 <- readHBWiWo(path=paste0(getwd(),"/data/HBWiWo"))
text2 <- readHBWiWo(path=paste0(getwd(),"/data/HBWiWo"), do.meta = FALSE, do.text = TRUE)
meta2 <- readHBWiWo(path=paste0(getwd(),"/data/HBWiWo"), do.meta = TRUE, do.text = FALSE)

load("data/HBWiWo_compare.Rdata")
expect_equal(HBWiWo2, HBWiWo)
expect_equal(text2, text)
expect_equal(meta2, meta)
})
