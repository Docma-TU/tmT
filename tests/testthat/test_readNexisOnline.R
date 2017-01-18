context("read Nexis Online files")

test_that("readNexisOnline", {
NexisOnline2 <- readNexisOnline(path = paste0(getwd(),"/data/NexisOnline"))

load("data/NexisOnline_compare.Rdata")
expect_equal(NexisOnline2, NexisOnline)
})
