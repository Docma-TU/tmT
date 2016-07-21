context("read HB or WiWo files")

test_that("readHBWiWo", {
HBWiWo2 <- readHBWiWo(file="../tests/testthat/data/HBWiWo1.txt", en=FALSE)
text2 <- readHBWiWo(file="../tests/testthat/data/HBWiWo1.txt", do.meta = FALSE, do.text = TRUE, en=FALSE)
meta2 <- readHBWiWo(file="../tests/testthat/data/HBWiWo1.txt", do.meta = TRUE, do.text = FALSE, en=FALSE)

load("tests/testthat/data/HBWiWo_compare.Rdata")
expect_equal(HBWiWo2, HBWiWo)
expect_equal(text2, text)
expect_equal(meta2, meta)


HBWiWo2 <- readHBWiWo(file="../tests/testthat/data/HBWiWo2.txt", en=TRUE)
text2 <- readHBWiWo(file="../tests/testthat/data/HBWiWo2.txt", do.meta = FALSE, do.text = TRUE, en=TRUE)
meta2 <- readHBWiWo(file="../tests/testthat/data/HBWiWo2.txt", do.meta = TRUE, do.text = FALSE, en=TRUE)

load("tests/testthat/data/HBWiWo_en_compare.Rdata")
expect_equal(HBWiWo2, HBWiWo)
expect_equal(text2, text)
expect_equal(meta2, meta)
})


