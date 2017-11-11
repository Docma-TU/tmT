#' Plotting Counts of Topics over Time (relative to Corpus)
#' 
#' Creates a plot of the counts/proportion of specified topics of a result of
#' \code{\link{LDAstandard}}. There is an option to plot all curves in one plot
#' or to create one plot for every curve (see \code{pages}).
#' In addition the plots can be written to a pdf by setting \code{file}.
#' 
#' @param object \code{\link{textmeta}} object with strictly tokenized
#' \code{text} component (\code{character} vectors) - like a result of
#' \code{\link{makeClear}}
#' @param ldaresult the result of a function call \code{\link{LDAstandard}}
#' @param ldaID \code{character} vector of IDs of the documents in
#' \code{ldaresult}
#' @param select \code{integer} vector (default: all topics) which topics of
#' \code{ldaresult} should be plotted
#' @param tnames \code{character} vector of same length as \code{select}
#' - labels for the topics (default are the first returned words of
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
#' the function will for every topicgroup plot at first the exact and then the 
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
#' to zeros. Only has effect if \code{rel = TRUE}.
#' @param file \code{character} file path if a pdf should be created
#' @param ... additional graphical parameters 
#' @return A plot.
#' Invisible: A dataframe with columns \code{date} and \code{tnames} with the
#' counts/proportion of the selected topics.
#' @keywords ~kwd1 ~kwd2
#' @examples ##
#' @export plotTopic

plotTopic <- function(object, ldaresult, ldaID,
  select = 1:nrow(ldaresult$document_sums), tnames, rel = FALSE, mark = TRUE,
  unit = "month", curves = c("exact", "smooth", "both"), smooth = 0.05,
  main, xlab, ylim, ylab, both.lwd, both.lty, col,
  legend = ifelse(pages, "onlyLast:topright", "topright"),
  pages = FALSE, natozero = TRUE, file, ...){
  
  if(missing(tnames)) tnames <- paste0("T", select, ".",
    lda::top.topic.words(ldaresult$topics, 1)[select])
  # set x-label if missing
  if(missing(xlab)) xlab <- "date"
  # set y-label if missing
  if(missing(ylab)) ylab <- ifelse(rel, "proportion", "counts")
  # set "both" - graphical parameters if missing
  if(missing(both.lwd)) both.lwd <- 1
  if(missing(both.lty)) both.lty <- 1
  
  if(!missing(file)) pdf(file, width = 15, height = 8)
  if(pages){
    mainP <- paste("Count of topic", tnames, "over time")
    if(rel) mainP <- paste("Proportion of topic", tnames,"over time")
    if(curves[1] == "both") colP <- c("grey", "black")
    else colP <- "black"
    for (i in seq_along(select))
      plotTopic(object = object, ldaresult = ldaresult, ldaID = ldaID,
        select = select[i], tnames = tnames[i], rel = rel, mark = mark,
        unit = unit, curves = curves, smooth = smooth,
        main = ifelse(missing(main), mainP[i], main), col = colP,
        legend = legend, both.lwd = both.lwd, both.lty = both.lty,
        xlab = xlab, ylab = ylab, pages = FALSE, ...)
  }
  
  stopifnot(is.textmeta(object), is.list(ldaresult),
    is.matrix(ldaresult$document_sums), is.character(ldaID),
    all(as.integer(select) == select), length(tnames) == length(select),
    is.character(tnames), is.logical(rel), is.logical(mark), length(rel) == 1,
    length(mark) == 1,
    is.character(unit), length(unit) == 1, is.numeric(smooth),
    length(smooth) == 1, all(curves %in% c("exact", "smooth", "both")),
    is.character(xlab), is.character(ylab), is.numeric(both.lwd),
    is.numeric(both.lty), length(xlab) == 1, length(ylab) == 1,
    length(both.lty) == 1, length(both.lwd) == 1)
  
  indMeta <- match(ldaID, object$meta$id)
  indText <- match(ldaID, names(object$text))
  
  # generate x-values date (non-unique at this point)
  dates <- lubridate::floor_date(object$meta$date[indMeta], unit)
  # generate markers on every beginning year
  if (mark) markYears <- seq(from = lubridate::floor_date(
    min(dates, na.rm = TRUE), unit = "year"), to = lubridate::ceiling_date(
      max(dates, na.rm = TRUE), unit = "year"), by = "year")
  else markYears <- NA
  
  # columns: selected topics, rows: documents
  docTopic <- data.frame(t(ldaresult$document_sums)[, select])
  # sum words to unit
  docTopic <- aggregate(docTopic, by = list(date = dates), FUN = sum)
  
  if (rel){
    # sum words to unit for normalization
    normsums <- aggregate(
      lengths(ldaresult$assignments),
      by = list(date = lubridate::floor_date(object$meta$date[indMeta], unit)),
      FUN = sum)
    normsums <- normsums[match(docTopic$date, normsums$date),]
    docTopic[, 2:length(docTopic)] <- docTopic[, 2:length(docTopic)] / normsums$x
    if(missing(main)) main <- "Proportion of topics over time"
  }
  else if(missing(main)) main <- "Count of topics over time"
  if(missing(ylim)) ylim <- c(0, max(docTopic[, 2:length(docTopic)]))
  names(docTopic) <- c("date", tnames)
  
  # identify levels to add as zeros
  levs <-
    unique(lubridate::floor_date(seq(from = min(docTopic$date),
      to = max(docTopic$date), by = "day"), unit = unit))
  zerosToAdd <- !(levs %in% docTopic$date)
  if(any(zerosToAdd)){
    matrixAdd <- matrix(0, nrow = sum(zerosToAdd), ncol = ncol(docTopic)-1)
    #if(rel) matrixAdd <- cbind(matrixAdd,
    #  matrix(NA, nrow = sum(zerosToAdd), ncol = ncol(docTopic)-1))
    zerosToAdd <- data.frame(levs[zerosToAdd], matrixAdd)
    names(zerosToAdd) <- names(docTopic)
    docTopic <- rbind(docTopic, zerosToAdd)
  }
  # order docTopic
  docTopic <- docTopic[order(docTopic$date),]
  if(natozero) docTopic[is.na(docTopic)] <- 0
  row.names(docTopic) <- 1:nrow(docTopic)
  
  plot(docTopic$date, docTopic[, 2], type = "n",
    main = main, xlab = xlab, ylab = ylab, ylim = ylim, ...)
  abline(v = markYears, lty = 3)
  switch(curves[1],
    exact = {
      # set colors if missing
      if (missing(col)) col <- RColorBrewer::brewer.pal(8, "Dark2")
      col <- rep(col, length.out = length(tnames))
      for (i in 1:(ncol(docTopic)-1))
        lines(docTopic$date, docTopic[, i+1], col = col[i], ...)
    },
    smooth = {
      # set colors if missing
      if(missing(col)) col <- RColorBrewer::brewer.pal(8, "Dark2")
      col <- rep(col, length.out = length(tnames))
      for (i in 1:(ncol(docTopic)-1))
        lines(lowess(docTopic$date, docTopic[, i+1], f = smooth), col = col[i], ...)
    },
    both = {
      # set colors if missing
      if (missing(col)) col <- RColorBrewer::brewer.pal(12, "Paired")
      col <- rep(col, length.out = 2*length(tnames))
      # plot both curves
      for (i in 1:(ncol(docTopic)-1)){
        lines(docTopic$date, docTopic[, i+1], col = col[2*i-1], ...)
        lines(lowess(docTopic$date, docTopic[, i+1], f = smooth), col = col[2*i],
          lwd = both.lwd, lty = both.lty)
      }
      # reduce col-vector for legend
      col <- col[seq_along(col) %% 2 == 0]
    })
  # plot legend
  if(all(legend != "none", !grepl("onlyLast:", legend)) ||
      (grepl("onlyLast:", legend) && pages))
    legend(gsub("onlyLast:", "", x = legend), legend = tnames, col = col, pch = 20)
  if(!missing(file)) dev.off()
  
  invisible(docTopic)
}