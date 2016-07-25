context("make.clear: Remove ... tokenization")

test_that("make.clear", {

    text <- list("Lorem1 ipsum2 dolor3 sit4 amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. ",
                 "  Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. ", "Lorem ipsum dolor sit amet, consectetur adipisici elit, sed dass waehrend der die das eiusmod tempor incidunt ut labore et dolore magna aliqua. ", "", "")

    text2 <- list("Lorem1 ipsum2 dolor3 sit4 amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. ",
                 c("  Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. ", "Lorem ipsum dolor sit amet, consectetur adipisici elit, sed dass waehrend der die das eiusmod tempor incidunt ut labore et dolore magna aliqua. ", ""), "")

    mc1 <- list(c(
"lorem",      "ipsum",      "dolor",      "sit",        "amet",
"consectetur","adipisici",  "elit",       "sed",        "eiusmod",
"tempor",     "incidunt",   "ut",         "labore",     "et",
"dolore",     "magna",      "aliqua"),
c(
"ut",           "enim",         "ad",           "minim",
"veniam",       "quis",         "nostrud",      "exercitation",
"ullamco",      "laboris",      "nisi",         "ut",
"aliquid",      "ex",           "ea",           "commodi",
"consequat",    "quis",         "aute",         "iure",
"reprehenderit","voluptate",    "velit",        "esse",
"cillum",       "dolore",       "eu",           "fugiat",
"nulla",        "pariatur"),
c(
"lorem",      "ipsum",      "dolor",      "sit",        "amet",
"consectetur","adipisici",  "elit",       "sed",        "eiusmod",
"tempor",     "incidunt",   "ut",         "labore",     "et",
"dolore",     "magna",      "aliqua"))


mc2 <- list(list(c(
"lorem",      "ipsum",      "dolor",      "sit",        "amet",
"consectetur","adipisici",  "elit",       "sed",        "eiusmod",
"tempor",     "incidunt",   "ut",         "labore",     "et",
"dolore",     "magna",      "aliqua")),
list(c(
"ut",           "enim",         "ad",           "minim",
"veniam",       "quis",         "nostrud",      "exercitation",
"ullamco",      "laboris",      "nisi",         "ut",
"aliquid",      "ex",           "ea",           "commodi",
"consequat",    "quis",         "aute",         "iure",
"reprehenderit","voluptate",    "velit",        "esse",
"cillum",       "dolore",       "eu",           "fugiat",
"nulla",        "pariatur"),
c(
"lorem",      "ipsum",      "dolor",      "sit",        "amet",
"consectetur","adipisici",  "elit",       "sed",        "eiusmod",
"tempor",     "incidunt",   "ut",         "labore",     "et",
"dolore",     "magna",      "aliqua")))

expect_equal(make.clear(text, paragraph=FALSE), mc1)
expect_equal(make.clear(text2, paragraph=TRUE), mc2)
})
