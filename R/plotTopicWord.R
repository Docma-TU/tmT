#' Plotting Counts of Topics-Words-Combination over Time (relative to Words)
#' 
#' Creates a plot of the counts/proportion of specified combination of topics
#' and words. It is important to keep in mind that the baseline for
#' proportions are the sums of words, not sums of topics.
#' There is an option to plot all curves in one plot or to create one plot for
#' every curve (see \code{pages}). In addition the plots can be written to a pdf
#' by setting \code{file}.
#'
#' @param object \code{\link{textmeta}} object with strictly tokenized
#' \code{text} component (\code{character} vectors) - like a result of
#' \code{\link{makeClear}}
#' @param docs object as a result of \code{\link{docLDA}} which was handed over
#' to \code{\link{LDAstandard}}
#' @param ldaresult the result of a function call \code{\link{LDAstandard}} with
#' \code{docs} as argument
#' @param ldaID \code{character} vector of IDs of the documents in
#' \code{ldaresult}
#' @param wordlist list of \code{character} vectors. Every list element is an 'or'
#' link, every \code{character} string in a vector is linked by the argument
#' \code{link}. If \code{wordlist} is only a \code{character} vector it will be
#' coerced to a list of the same length as the vector (see \code{\link{as.list}}),
#' so that the argument \code{link} has no effect. Each \code{character} vector
#' as a list element represents one curve in the outcoming plot
#' @param link \code{character} (default: \code{"and"}) should the (inner)
#' \code{character} vectors of each list element be linked by an \code{"and"}
#' or an \code{"or"}
#' @param select list of \code{integer} vectors (default: all topics as simple
#' \code{integer} vector) which topics - linked by an "or" every time -
#' should be take into account for plotting the word counts/proportion.
#' @param tnames \code{character} vector of same length as \code{select}
#' - labels for the topics (default are the first returned words of
#' @param wnames \code{character} vector of same length as \code{wordlist}
#' - labels for every group of 'and' linked words
#' \code{\link{top.topic.words}} from the \code{lda} package for each topic)
#' @param rel \code{logical} (default: \code{FALSE}) should counts
#' (\code{FALSE}) or proportion (\code{TRUE}) be plotted
#' @param mark \code{logical} (default: \code{TRUE}) should years be marked by
#' vertical lines
#' @param unit \code{character} (default: \code{"month"}) to which unit should
#' dates be floored
#' @param curves \code{character} (default: \code{"exact"}) should \code{"exact"},
#' \code{"smooth"} curve or \code{"both"} be plotted
#' @param smooth \code{numeric} (default: \code{0.05}) smoothing parameter
#' which is handed over to \code{\link{lowess}} as \code{f}
#' @param both.lwd graphical parameter for smoothed values
#' if \code{curves = "both"}
#' @param both.lty graphical parameter for smoothed values
#' if \code{curves = "both"}
#' @param main \code{character} graphical parameter
#' @param xlab \code{character} graphical parameter
#' @param ylab \code{character} graphical parameter
#' @param ylim graphical parameter
#' @param col graphical parameter, could be a vector. If \code{curves = "both"}
#' the function will for every wordgroup plot at first the exact and then the 
#' smoothed curve - this is important for your col order.
#' @param legend \code{character} (default: \code{"topright"},
#' \code{"onlyLast:topright"} for \code{pages = TRUE} respectively)
#' value(s) to specify the legend coordinates. If "none" no legend is plotted.
#' @param pages \code{logical} (default: \code{FALSE}) should all curves be
#' plotted in a single plot. In addtion you could set
#' \code{legend = "onlyLast:<argument>"} with \code{<argument>} as a
#' \code{character} \code{legend} argument
#' for only plotting a legend on the last plot of set.
#' @param natozero \code{logical} (default: \code{TRUE}) should NAs be coerced
#' to zeros
#' @param file \code{character} file path if a pdf should be created
#' @param \dots additional graphical parameters
#' @return A plot.
#' Invisible: A dataframe with columns \code{date} and \code{tnames: wnames}
#' with the counts/proportion of the selected combination of topics and words.
#' @keywords ~kwd1 ~kwd2
#' @examples
#' ##---- Should be DIRECTLY executable !! ----
#' @export plotTopicWord
#'

plotTopicWord <- function(object, docs, ldaresult, ldaID,
  wordlist = lda::top.topic.words(ldaresult$topics, 1), link = c("and", "or"),
  select = 1:nrow(ldaresult$document_sums),
  tnames, wnames, rel = FALSE, mark = TRUE, unit = "month",
  curves = c("exact", "smooth", "both"), smooth = 0.05,
  legend = ifelse(pages, "onlyLast:topright", "topright"),
  pages = FALSE, natozero = TRUE,
  file, main, xlab, ylab, ylim, both.lwd, both.lty, col, ...){
  
  wordsLen <- length(wordlist)
  topicLen <- length(select)
  # coerce list
  if(!is.list(wordlist)){
    if(wordsLen == 1) wordlist <- rep(wordlist, topicLen)
    wordlist <- as.list(wordlist)
  }
  if(!is.list(select)){
    if(topicLen == 1) select <- rep(select, wordsLen)
    select <- as.list(select)
  }
  if(missing(tnames)) tnames <- sapply(select, function(x)
    paste(paste0("T", x, ".", lda::top.topic.words(ldaresult$topics, 1)[x]),
      collapse = "."))
  if(missing(wnames)) wnames <- sapply(wordlist, paste, collapse = ".")
  # set x-label if missing
  if(missing(xlab)) xlab <- "date"
  # set y-label if missing
  if(missing(ylab)) ylab <- ifelse(rel, "proportion", "counts")
  # set "both" - graphical parameters if missing
  if(missing(both.lwd)) both.lwd <- 1
  if(missing(both.lty)) both.lty <- 1
  
  if(!missing(file)) pdf(file, width = 15, height = 8)
  if(pages){
    tmpMain <- paste(tnames, wnames, sep = ": ")
    mainP <- paste("Count of", tmpMain, "over time")
    if(rel) mainP <- paste("Proportion of", tmpMain,"over time")
    if(curves[1] == "both") colP <- c("grey", "black")
    else colP <- "black"
    stopifnot(length(tnames) == length(select),
      length(wnames) == length(select), length(wordlist) == length(select))
    for (i in seq_along(select))
      plotTopicWord(object = object, docs = docs, ldaresult = ldaresult,
        ldaID = ldaID, wordlist = wordlist[[i]], link = link, select = select[[i]],
        tnames = tnames[i], wnames = wnames[i], rel = rel, mark = mark,
        unit = unit, curves = curves, smooth = smooth,
        main = ifelse(missing(main), mainP[i], main), col = colP,
        legend = legend, both.lwd = both.lwd, both.lty = both.lty,
        xlab = xlab, ylab = ylab, pages = FALSE, natozero = natozero, ...)
  }
  
  stopifnot(is.textmeta(object), is.list(ldaresult),
    is.matrix(ldaresult$topics), is.list(ldaresult$assignment),
    is.matrix(ldaresult$document_sums), is.character(ldaID), is.list(select),
    all(as.integer(unlist(select)) == unlist(select)), is.list(wordlist),
    is.character(unlist(wordlist)), is.list(docs), length(docs) == length(ldaID),
    ncol(ldaresult$document_sums) == length(ldaID), all(link %in% c("and", "or")),
    length(tnames) == length(select), length(wnames) == length(select),
    length(wordlist) == length(select), is.character(tnames), is.character(wnames),
    is.logical(rel), is.logical(mark), length(rel) == 1, length(mark) == 1,
    is.character(unit), length(unit) == 1, is.numeric(smooth),
    length(smooth) == 1, all(curves %in% c("exact", "smooth", "both")),
    is.character(xlab), is.character(ylab), is.numeric(both.lwd),
    is.numeric(both.lty), length(xlab) == 1, length(ylab) == 1,
    length(both.lty) == 1, length(both.lwd) == 1)
  
  meta <- object$meta$date
  names(meta) <- object$meta$id
  ldaVocab <- colnames(ldaresult$topics)
  uniqueWords <- unique(unlist(wordlist))
  uniqueWordsID <- match(uniqueWords, ldaVocab)-1
  if(any(is.na(uniqueWordsID)))
    cat("NOTE: At least one word from wordlist is not included in the vocabulary of LDA")
  uniqueWordsID[is.na(uniqueWordsID)] <- -1 #set to a number which does not appear
  wordcount <- do.call(cbind, lapply(uniqueWordsID, function(wordX)
    sapply(docs, function(x) sum(x[1, ] == wordX))))
  tmp1 <- wordcount
  wordcount <- do.call(cbind, lapply(wordlist,
    function(x) rowSums(as.matrix(wordcount[, uniqueWords %in% x]))))
  
  selectInd <- rep(1:length(select), lengths(wordlist))
  allWords <- unlist(wordlist)
  allWordsID <- match(allWords, ldaVocab)-1
  summingIterator <- c(0, cumsum(lengths(wordlist)))
    
  topicwordcount <- do.call(cbind, lapply(1:length(allWordsID),
    function(wordindex) sapply(1:length(ldaresult$assignment),
      function(docindex)
        sum((ldaresult$assignment[[docindex]] %in% (select[[selectInd[wordindex]]]-1)) &
            (docs[[docindex]][1, ] == allWordsID[wordindex])))))
  tmp2 <- topicwordcount
  topicwordcount <- do.call(cbind, lapply(2:length(summingIterator),
    function(x) rowSums(as.matrix(
      topicwordcount[, (summingIterator[x-1]+1):summingIterator[x]]))))

  ######### fuer evtl bald plotWordTopic!!! :
  #uniqueSelect = unique(unlist(select))
  # zaehlt aktuell nicht die woerter in den topics sondern lediglich die topics
  # for baseline topics use plotWordTopic (TBD)
  #topiccount <- do.call(cbind, lapply(uniqueSelect, function(topic)
  #  sapply(ldaresult$assignment, function(x) sum(x == topic-1))))
  #topiccount <- do.call(cbind, lapply(select,
  #  function(x) rowSums(as.matrix(topiccount[, uniqueSelect %in% x]))))
  
  if (link[1] == "and"){
    # identifying articles where not all words of an wordlist component
    # are represented (per wordlist component)
    tmp1 <- do.call(cbind, lapply(wordlist,
      function(x) apply(as.matrix(tmp1[, uniqueWords %in% x]), 1,
        function(y) all(y == 0))))
    # set these counts to zero
    wordcount[tmp1] <- 0
    tmp2 <- do.call(cbind, lapply(2:length(summingIterator),
      function(x) apply(as.matrix(tmp2[,
        (summingIterator[x-1]+1):summingIterator[x]]), 1,
        function(y) all(y == 0))))
    # set these counts to zero
    topicwordcount[tmp2] <- 0
  }
  rm(tmp1, tmp2)
  tmpdate <- meta[match(rownames(wordcount), names(meta))]
  tmpdate <- lubridate::floor_date(tmpdate, unit = unit)
  if (mark) markYears <- seq(from = lubridate::floor_date(
    min(tmpdate), unit = "year"), to = lubridate::ceiling_date(
      max(tmpdate), unit = "year"), by = "year")
  else markYears <- NA
  splt1 <- apply(wordcount, 2, function(x) sapply(split(x, tmpdate), sum))
  splt2 <- apply(topicwordcount, 2, function(x) sapply(split(x, tmpdate), sum))
  
  if(rel){
    tab <- cbind(as.Date(rownames(splt2)),
      data.frame(splt2 / splt1[match(rownames(splt2), rownames(splt1)), ]))
    if(missing(main)) main <- "Proportion of Topic-Words over Time"
  }
  else{
    tab <- cbind(as.Date(rownames(splt2)), data.frame(splt2))
    if(missing(main)) main <- "Counts of Topic-Words over Time"
  }
  
  # identify levels to add as zeros
  levs <-
    unique(lubridate::floor_date(seq(from = min(tab[, 1]),
      to = max(tab[, 1]), by = "day"), unit = unit))
  zerosToAdd <- !(levs %in% tab[, 1])
  if(any(zerosToAdd)){
    matrixAdd <- matrix(0, nrow = sum(zerosToAdd), ncol = ncol(tab)-1)
    zerosToAdd <- data.frame(levs[zerosToAdd], matrixAdd)
    colnames(zerosToAdd) <- colnames(tab)
    tab <- rbind(tab, zerosToAdd)
  }
  row.names(tab) <- 1:nrow(tab)
  colnames(tab) <- c("date", paste(tnames, wnames, sep = ": "))
  # order tab
  tab <- tab[order(tab$date),]
  if(natozero) tab[is.na(tab)] <- 0
  if(missing(ylim)) ylim <- c(0, max(tab[, 2:length(tab)], na.rm = TRUE))
  
  plot(tab$date, tab[, 2], type = "n",
    main = main, xlab = xlab, ylab = ylab, ylim = ylim, ...)
  abline(v = markYears, lty = 3)
  switch(curves[1],
    exact = {
      # set colors if missing
      if (missing(col)) col <- RColorBrewer::brewer.pal(8, "Dark2")
      col <- rep(col, length.out = length(tnames))
      for (i in 1:(ncol(tab)-1))
        lines(tab$date, tab[, i+1], col = col[i], ...)
    },
    smooth = {
      # set colors if missing
      if(missing(col)) col <- RColorBrewer::brewer.pal(8, "Dark2")
      col <- rep(col, length.out = length(tnames))
      for (i in 1:(ncol(tab)-1))
        lines(lowess(tab$date, tab[, i+1], f = smooth), col = col[i], ...)
    },
    both = {
      # set colors if missing
      if (missing(col)) col <- RColorBrewer::brewer.pal(12, "Paired")
      col <- rep(col, length.out = 2*length(tnames))
      # plot both curves
      for (i in 1:(ncol(tab)-1)){
        lines(tab$date, tab[, i+1], col = col[2*i-1], ...)
        lines(lowess(tab$date, tab[, i+1], f = smooth), col = col[2*i],
          lwd = both.lwd, lty = both.lty)
      }
      # reduce col-vector for legend
      col <- col[seq_along(col) %% 2 == 0]
    })
  if1 <- all(legend != "none", !grepl("onlyLast:", legend))
  if2 <- grepl("onlyLast:", legend) && pages
  if(if1 || if2)
    legend(gsub("onlyLast:", "", x = legend),
      legend = paste(tnames, wnames, sep = ": "), col = col, pch = 20)
  
  if(!missing(file)) dev.off()
  invisible(tab)
}