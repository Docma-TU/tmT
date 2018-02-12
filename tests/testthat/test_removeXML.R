context("remove XML tags and change umlauts style")

test_that("removeXML", {
#Sys.setlocale('LC_ALL','C')

text <- c("aba<vcs>ab</vcs>caa", "ab&dgv;abc", "&auml;&Auml;&ouml;&Ouml;&uuml;&Uuml;&szlig;", "aa", "aab", "bc")

tmp <- c("e4","c4","f6","d6","fc","dc","df")
tmp <- paste0(sapply(tmp, function(x)eval(parse(text = paste0("'\\u", x, "'")))), collapse = "")
text2 <- c("aba<vcs>ab</vcs>caa", "ab&dgv;abc", tmp, "aa", "aab", "bc")

expect_equal(removeXML(x=text2), c("aba ab caa","ab&dgv;abc","\UE4\UC4\UF6\UD6\UFC\UDC\UDF","aa","aab","bc"))
expect_equal(removeHTML(x=text, symbolList = 1, dec=FALSE, hex=FALSE, delete = FALSE), text2)

umlauts <- c("aba ab caa","ab&dgv;abc","\UE4\UC4\UF6\UD6\UFC\UDC\UDF","aa","aab","bc")
exp1 <- c("aba ab caa", "ab&dgv;abc", "aeAeoeOeueUess", "aa", "aab", "bc")
expect_equal(removeUmlauts(x=umlauts), exp1)

})

# removeHTML(x, dec=TRUE, hex=TRUE, entity=TRUE, symbolList=1, delete=TRUE, symbols=FALSE)
# removeUmlautsfunction(x)
# removeXML(x)

## text <- "&Auml;&Ouml;"
## text <- gsub(pattern="&Auml;", replacement="\u00C4", x=text, useBytes=TRUE)
## text <- gsub(pattern="&Ouml;", replacement="\u00D6", x=text, useBytes=TRUE)
## text
## text=="\u00C4\u00D6"

## Encoding(text) <- "UTF-8"
## text
## text=="\u00C4\u00D6"


##         cat(eval(parse(text = paste0("'\\u", matchedEntities[i,2], "'"))))



## expect_equal(removeHTML(x=text, symbolList = 1, dec=FALSE, hex=FALSE, delete = FALSE), text2)
