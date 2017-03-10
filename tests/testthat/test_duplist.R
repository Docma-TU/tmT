context("creating duplists")

test_that("readSPIEGEL", {
  load("data/SP_compare.RData")
  load("data/SZ_compare.RData")
  load("data/Nexis_compare.RData")
  
  SPduplist2 <- duplist(SPIEGEL, paragraph = TRUE)
  SZduplist2 <- duplist(SZ, paragraph = TRUE)
  Nexisduplist2 <- duplist(Nexis)
  
  load("data/duplist_compare.RData")
  
  expect_equal(SPduplist2, SPduplist)
  expect_equal(SZduplist2, SZduplist)
  expect_equal(Nexisduplist2, Nexisduplist)
})
