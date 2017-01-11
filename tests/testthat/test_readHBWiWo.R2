context("read HB or WiWo files")

test_that("readHBWiWo", {
HBWiWo2 <- readHBWiWo(file="data/HBWiWo1.txt", en=FALSE, test=TRUE)
text2 <- readHBWiWo(file="data/HBWiWo1.txt", do.meta = FALSE, do.text = TRUE, en=FALSE, test=TRUE)
meta2 <- readHBWiWo(file="data/HBWiWo1.txt", do.meta = TRUE, do.text = FALSE, en=FALSE, test=TRUE)

load("data/HBWiWo_compare.Rdata")
expect_equal(HBWiWo2, HBWiWo)
expect_equal(text2, text)
expect_equal(meta2, meta)


HBWiWo2 <- readHBWiWo(file="data/HBWiWo2.txt", en=TRUE, test=TRUE)
text2 <- readHBWiWo(file="data/HBWiWo2.txt", do.meta = FALSE, do.text = TRUE, en=TRUE, test=TRUE)
meta2 <- readHBWiWo(file="data/HBWiWo2.txt", do.meta = TRUE, do.text = FALSE, en=TRUE, test=TRUE)

load("data/HBWiWo_en_compare.Rdata")
expect_equal(HBWiWo2, HBWiWo)
expect_equal(text2, text)
expect_equal(meta2, meta)
})


