context("read SZ files")

test_that("readSZ", {

SZ2 <- readSZ(file=c("data/SZ/SZ1.txt","tests/testthat/data/SZ/SZ2.txt"), folder="tests/testthat/data/SZ")
text2 <- readSZ(file=c("data/SZ/SZ1.txt","tests/testthat/data/SZ/SZ2.txt"), folder="tests/testthat/data/SZ", do.meta = FALSE, do.text = TRUE)
meta2 <- readSZ(file=c("data/SZ/SZ1.txt","tests/testthat/data/SZ/SZ2.txt"), folder="tests/testthat/data/SZ", do.meta = TRUE, do.text = FALSE)

load("data/SZ_compare.Rdata")
expect_equal(SZ2, SZ)
expect_equal(text2, text)
expect_equal(meta2, meta)
})


