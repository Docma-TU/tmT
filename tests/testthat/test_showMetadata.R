context("showMetadata")

test_that("showMetadata", {

    meta <- data.frame(id=as.character(1:3), date=as.Date(c("1960-01-01","1987-06-25","2014-08-06")), title=c("Title 1", "Title 2", "Title 3"), page=c(24,60,1), stringsAsFactors=FALSE)

    expect_equal(showMetadata(meta=meta, file="test.csv", id = as.character(c(1,3)), cols = colnames(meta), csv.ger = FALSE), meta[c(1,3),])
    expect_equal(showMetadata(meta=meta, file="test.csv", cols = colnames(meta), csv.ger = TRUE), meta)
})
