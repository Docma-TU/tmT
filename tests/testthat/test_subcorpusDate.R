context("subcorpusDate")

test_that("subcorpusDate", {
text <- as.list(paste("text", 1:200))
names(text) <- paste("id", 101:300)
set.seed(24601)
meta <- data.frame(id=paste("id", 1:400), date=as.Date(1:400, origin="1990-10-03"), stringsAsFactors = FALSE)

expect_equal(text, subcorpusDate(text=text, meta=meta, s.date = min(meta$date),  e.date = max(meta$date)))
expect_equal(text[17:22], subcorpusDate(text=text, meta=meta, s.date = as.Date("1991-01-28"),  e.date = as.Date("1991-02-02")))
expect_equal(text, subcorpusDate(text=text, meta=meta, s.date = as.Date("1960-01-28"),  e.date = as.Date("1999-02-02")))

})
