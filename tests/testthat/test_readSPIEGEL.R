context("read SPIEGEL files")

test_that("readSPIEGEL", {
Sys.setlocale('LC_ALL','C')
SPIEGEL2 <- readSPIEGEL(path=paste0(getwd(),"/data/Spiegel"))
text2 <- readSPIEGEL(path=paste0(getwd(),"/data/Spiegel"), do.meta = FALSE, do.text = TRUE)
meta2 <- readSPIEGEL(path=paste0(getwd(),"/data/Spiegel"), do.meta = TRUE, do.text = FALSE)

load("data/SP_compare.RData")
expect_equal(SPIEGEL2, SPIEGEL)
expect_equal(text2, text)
expect_equal(meta2, meta)
})

## SPIEGEL <- SPIEGEL2
## text <- text2
## meta <- meta2
## save(SPIEGEL, text, meta, file="data/SP_compare.RData")
