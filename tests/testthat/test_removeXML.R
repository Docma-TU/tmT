context("remove XML tags and change umlauts style")

test_that("removeXML", {
Sys.setlocale('LC_ALL','C')

text <- list("aba<vcs>ab</vcs>caa", "ab&dgv;abc", c("&Auml; &Ouml; &Uuml; &auml; &ouml; &uuml; &szlig;"), c("aa", "aab", "bc"))
text2 <- list("aba<vcs>ab</vcs>caa", "ab&dgv;abc", c("הצüßײִÜ"), c("aa", "aab", "bc"))

expect_equal(removeXML(x=text2, umlaute = FALSE, u.type = c("normal", "html", "all")), list(c("aba ab caa"),c("ab&dgv;abc"),c("הצüßײִÜ"),c("aa",  "aab", "bc")))

expect_equal(removeXML(x=text2, umlaute = TRUE, u.type = c("normal", "html", "all")), list(c("aba ab caa"),c("ab&dgv;abc"),c("aeoeuessOeAeUe"),c("aa",  "aab", "bc")))


})
