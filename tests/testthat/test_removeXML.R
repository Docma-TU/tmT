context("remove XML tags and change umlauts style")

test_that("removeXML", {
text <- list("aba<vcs>ab</vcs>caa", "ab&dgv;abc", c("&Auml; &Ouml; &Uuml; &auml; &ouml; &uuml; &szlig;"), c("aa", "aab", "bc"))
text <- list("aba<vcs>ab</vcs>caa", "ab&dgv;abc", c("הצüßײִÜ"), c("aa", "aab", "bc"))

removeXML(x=text, umlaute = FALSE, u.type = c("normal", "html", "all"))
expect_equal(removeXML(x=text, umlaute = FALSE, u.type = c("normal", "html", "all")), list(c("aba ab caa"),c("ab&dgv;abc"),c("הצüßײִÜ"),c("aa",  "aab", "bc")))

})
