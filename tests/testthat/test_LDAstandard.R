context("LDAstandard")

test_that("LDAstandard", {

load("data/LDAdoc_compare.RData")

expect_equal(lda1, LDAstandard(documents=LDAdoc, K = 3L, vocab=wordlist$words, num.iterations = 20L, burnin = 70L, seed=24601, folder=paste0(getwd(),"/test"), num.words = 10L, LDA = TRUE))
expect_equal(lda1, LDAstandard(documents=LDAdoc, K = 3L, vocab=wordlist$words, num.iterations = 20L, burnin = 70L, seed=24601, folder=paste0(getwd(),"/test"), num.words = 10L, LDA = FALSE))
expect_equal(lda1, LDAstandard(documents=LDAdoc, K = 3L, vocab=wordlist$words, num.iterations = 20L, burnin = 70L, seed=24601, num.words = 10L, LDA = FALSE))
})


## text <- list(A= "Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quet dolore magna aliqua. Ut enim ad minim veniamet dolore magna aliqua. Ut enim ad minim veniamis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
##              B= "Lorem ipsum dolor sit amet, consecteturofficia deserunt mollit officia deserunt mollit  adipisici eet dolore magna aliqua. Ut enim ad minim veniamet dolore magna aliqua. Ut enim ad minim veniamlit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud",
##              C="in voet dolore magna aliqua. Ut enim ad minim veniamluptate velit esse cillum dolore eu officia deserunt mollit officia deserunt mollit fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
##              D="Lorem ipsum dolor siofficia deserunt mollit officia deserunt mollit t amet, consectetur adipisici elit, sed eiusmod temporet dolore magna aliqua. Ut enim ad minim veniam incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud",
##              E="Lorem ipsum dolor sit amet,et dolore magna aliqua. Ut enim ad minim veniam consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. mollit anim id est laborum.",
##              F= "sunt in culpa qui officia deserunt mollit anim id est laborum.",
##              G= "Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minimet dolore magna aliqua. Ut enim ad minim veniamet dolore magna aliqua. Ut enim ad minim veniamet dolore magna aliqua. Ut enim ad minim veniam veniam, quis nostrud Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrudLorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud",
##              H="Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna  cupiditat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
##              I="iure reprehenderit iure reprehenderit in voluptate velit esse cillumofficia deserunt mollit officia deserunt mollit officia deserunt mollit  dolore eu fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, suntin culpa qui officiaiure reprehenderit  in culpa qui officiaiure reprehenderit  deserunt mollit anim id est laborum.")

## text <- makeClear(text)
## wordlist <- makeWordlist(text)
## LDAdoc <- docLDA(text, wordlist$words)

## lda1 <- LDAstandard(documents=LDAdoc, K = 3L, vocab=wordlist$words, num.iterations = 20L, burnin = 70L, seed=24601, folder=paste0(getwd(),"/test"), num.words = 10L, LDA = TRUE)

## setwd("//STORE/koppers/Textmining/tmT/tests/testthat")
## save(lda1, LDAdoc, wordlist ,file="data/LDAdoc_compare.RData")




