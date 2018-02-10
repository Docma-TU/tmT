context("remove XML tags and change umlauts style")

test_that("removeXML", {
Sys.setlocale('LC_ALL','C')

text <- c("aba<vcs>ab</vcs>caa", "ab&dgv;abc", "&auml;&Auml;&ouml;&Ouml;&uuml;&Uuml;&szlig;", "aa", "aab", "bc")
text2 <- c("aba<vcs>ab</vcs>caa", "ab&dgv;abc", "\U000E4\U000C4\U000F6\U000D6\U000FC\U000DC\U000DF", "aa", "aab", "bc")

expect_equal(removeXML(x=text2), c("aba ab caa","ab&dgv;abc","\U000E4\U000C4\U000F6\U000D6\U000FC\U000DC\U000DF","aa","aab","bc"))
expect_equal(removeHTML(x=text, symbolList = 1, dec=FALSE, hex=FALSE, delete = FALSE), text2)

umlauts <- c("aba ab caa","ab&dgv;abc","\U000E4\U000C4\U000F6\U000D6\U000FC\U000DC\U000DF","aa","aab","bc")
exp1 <- c("aba ab caa", "ab&dgv;abc", "aeAeoeOeueUess", "aa", "aab", "bc")
expect_equal(removeUmlauts(x=umlauts), exp1)



})

# removeHTML(x, dec=TRUE, hex=TRUE, entity=TRUE, symbolList=1, delete=TRUE, symbols=FALSE)
# removeUmlautsfunction(x)
# removeXML(x)