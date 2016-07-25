context("remove XML tags and change umlauts style")

test_that("removeXML", {
Sys.setlocale('LC_ALL','C')

text <- list("aba<vcs>ab</vcs>caa", "ab&dgv;abc", c("&auml;&ouml;&uuml;&szlig;&Ouml;&Auml;&Uuml;"), c("aa", "aab", "bc"))
text2 <- list("aba<vcs>ab</vcs>caa", "ab&dgv;abc", iconv(c("äöüßÖÄÜ"), from="latin1", to="UTF-8"), c("aa", "aab", "bc"))

expect_equal(removeXML(x=text2, umlaute = FALSE, u.type = c("normal", "html", "all")), list(c("aba ab caa"),c("ab&dgv;abc"),iconv(c("äöüßÖÄÜ"), from="latin1", to="UTF-8") ,c("aa",  "aab", "bc")))

exp1 <- list(c("aba ab caa"),c("ab&dgv;abc"),c("aeoeuessOeAeUe"),c("aa",  "aab", "bc"))
exp2 <- list(c("aba ab caa"),c("ababc"),c("aeoeuessOeAeUe"),c("aa",  "aab", "bc"))
expect_equal(removeXML(x=text2, umlaute = TRUE, u.type = "normal"), exp1)
expect_equal(removeXML(x=text , umlaute = TRUE, u.type = "html"), exp2)
expect_equal(removeXML(x=text2, umlaute = TRUE, u.type = "all"), exp2)
expect_equal(removeXML(x=text , umlaute = TRUE, u.type = "all"), exp2)

})
