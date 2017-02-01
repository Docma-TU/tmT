context("callTheJournalists")

test_that("callTheJournalists", {

    set.seed(24601)
    expect_equal(callTheJournalists(), 8245)
    expect_equal(callTheJournalists(), 8245)
    expect_equal(callTheJournalists(), 8228)
})
