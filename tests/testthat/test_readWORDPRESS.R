context("read WORDPRESS files")

test_that("readWORDPRESS", {
  WP2 <- readWORDPRESS(path=paste0(getwd(),"/data/Wordpress"))
  text2 <- readWORDPRESS(path=paste0(getwd(),"/data/Wordpress"), do.meta = FALSE, do.text = TRUE)
  meta2 <- readWORDPRESS(path=paste0(getwd(),"/data/Wordpress"), do.meta = TRUE, do.text = FALSE)
  
  load("data/WP_compare.RData")
  expect_equal(WP2, WP)
  expect_equal(text2, text)
  expect_equal(meta2, meta)
})
