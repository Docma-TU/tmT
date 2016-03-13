make.wordlist <-
function(x, k=100000){
n <- length(x)
(print(n))
N <- 0:floor(n/k)
words <- NULL
for(i in N){
(print(i*k))
words <- c(words,unique(unlist(x[(i*k+1):(min(n,i*k+k))])))
}
words <- sort(unique(words))
(print("table"))
wordtable <- rep(0,length(words))
names(wordtable) <- words
for(i in N){
(print(i*k))
tmp <- table(unlist(x[(i*k+1):(min(n,i*k+k))]))
mtch <- match(names(tmp),names(wordtable))
wordtable[mtch] <- wordtable[mtch] + tmp
}
return(list(words=words, wordtable=wordtable))
}
