context("remove XML tags and change umlauts style")

test_that("removeXML", {
Sys.setlocale('LC_ALL','C')

text <- c("aba<vcs>ab</vcs>caa", "ab&dgv;abc", "&auml;&Auml;&ouml;&Ouml;&uuml;&Uuml;&szlig;", "aa", "aab", "bc")
text2 <- c("aba<vcs>ab</vcs>caa", "ab&dgv;abc", "\U000E4\U000C4\U000F6\U000D6\U000FC\U000DC\U000DF", "aa", "aab", "bc")

removeXML(x=text2, xml=TRUE, umlauts = FALSE, u.type = c("normal", "html", "all"))

expect_equal(removeXML(x=text2, xml=TRUE, umlauts = FALSE), c("aba ab caa","ab&dgv;abc","\U000E4\U000C4\U000F6\U000D6\U000FC\U000DC\U000DF","aa","aab","bc"))
expect_equal(removeXML(x=text2, xml=FALSE, umlauts = FALSE), text2)

exp1 <- c("aba ab caa", "ab&dgv;abc", "aeAeoeOeueUess", "aa", "aab", "bc")
exp2 <- c("aba ab caa", "ababc", "aeAeoeOeueUess", "aa",  "aab", "bc")
expect_equal(removeXML(x=text2, umlauts = TRUE, u.type = "normal"), exp1)
expect_equal(removeXML(x=text , umlauts = TRUE, u.type = "html"), exp2)
expect_equal(removeXML(x=text2, umlauts = TRUE, u.type = "all"), exp2)
expect_equal(removeXML(x=text , umlauts = TRUE, u.type = "all"), exp2)
})


