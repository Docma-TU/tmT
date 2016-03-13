subcorpus.date <-
function(text, meta,s.date=min(meta$datum), e.date=max(meta$datum)){
mtch <- match(names(text),meta$id)
dateID <- which(meta$datum[mtch]>=s.date & meta$datum[mtch]<=e.date)
text <- text[dateID]
return(text)
}
