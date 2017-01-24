context("read Nexis Online files")

test_that("readNexisOnline", {
  NexisOnline2 <- readNexisOnline(path = paste0(getwd(),"/data/NexisOnline"))
  text2 <- readNexisOnline(path = paste0(getwd(),"/data/NexisOnline"), do.meta = FALSE)
  meta2 <- readNexisOnline(path = paste0(getwd(),"/data/NexisOnline"), do.text = FALSE)

  load("data/NexisOnline_compare.RData")
  expect_equal(NexisOnline2, NexisOnline)
  expect_equal(text2, text)
  expect_equal(meta2, meta)
})
