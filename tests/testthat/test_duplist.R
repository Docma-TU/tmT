context("creating duplists")

test_that("duplist", {
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

  expect_true(is.duplist(SPduplist2))
  expect_true(is.duplist(SZduplist2))
  expect_true(is.duplist(Nexisduplist2))

  expect_false(is.duplist(pi))
  a <- pi
  class(a) <- "duplist"
  expect_false(is.duplist(a))
  a <- list(a=pi, b=1)
  class(a) <- "duplist"
  expect_false(is.duplist(a))
  a <- list(uniqueTexts=NULL, notDuplicatedTexts=NULL, idFakeDups=NULL, idRealDups=NULL,
    allTextDups=NULL, textOnlyDups=NULL, textMetaDups=NULL, textOthersDups=NULL)
  expect_false(is.duplist(a))

  expect_error(print.duplist("a"))
})
