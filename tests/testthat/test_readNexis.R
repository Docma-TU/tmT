context("read Nexis files")

test_that("readNexis", {
  Nexis <- readNexis(path = paste0(getwd(),"/data/Nexis"))
   text <- readNexis(path = paste0(getwd(),"/data/Nexis"), do.meta = FALSE)
   meta <- readNexis(path = paste0(getwd(),"/data/Nexis"), do.text = FALSE)

   save(Nexis, text, meta, file="data/Nexis_compare.RData")

  Nexis2 <- readNexis(path = paste0(getwd(),"/data/Nexis"))
  text2 <- readNexis(path = paste0(getwd(),"/data/Nexis"), do.meta = FALSE)
  meta2 <- readNexis(path = paste0(getwd(),"/data/Nexis"), do.text = FALSE)

  load("data/Nexis_compare.RData")
  expect_equal(Nexis2, Nexis)
  expect_equal(text2, text)
  expect_equal(meta2, meta)
})
