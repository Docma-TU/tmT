subcorpus.words <-
function(text, wordlist, counts=1, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE){
subid <- numeric(length(text))
    for(i in 1:length(wordlist)){
tmp <- NULL
    for(j in 1:length(wordlist[[i]])){
if(length(counts)==1){co <- counts}else{co <- counts[[i]][j]}
        tmp <- cbind(tmp,sapply(text, function(x)sum(grepl(pattern=wordlist[[i]][j], x=x, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))>=co))
}
subid <- subid + apply(tmp,1,prod)
}
subid <- subid>0
return(text[subid])
}
