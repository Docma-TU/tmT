#' Plotting Counts of Documents or Words over Time (relative to Corpus)
#' 
#' Creates a plot of the countsorproportion of documentsorwords in the subcorpus,
#' which could be specified by \code{ids}.
#' 
#' @param object \code{\link{textmeta}} object - with strictly tokenized
#' \code{text} component (\code{character} vectors) if \code{type = "words"}
#' @param ids \code{character} vector (default: \code{object$meta$id}) which IDs
#' specify the subcorpus
#' @param type \code{character} (default: \code{"docs"}) should counts/proportion
#' of documents \code{"docs"} or words \code{"words"} be plotted
#' @param rel \code{logical} (default: \code{FALSE}) should counts
#' (\code{FALSE}) or proportion (\code{TRUE}) be plotted
#' @param mark \code{logical} (default: \code{TRUE}) should years be marked by
#' vertical lines
#' @param main \code{character} graphical parameter
#' @param xlab \code{character} graphical parameter
#' @param ylim (default if \code{rel = TRUE}: \code{c(0, 1)}) graphical parameter
#' @param ... additional graphical parameters 
#' @return A plot.
#' Invisible: A dataframe with columns \code{date} and \code{counts},
#' respectively \code{proportion}
#' @keywords ~kwd1 ~kwd2
#' @examples ##
#' @export plotScot

plotScot = function(object, ids = object$meta$id, type = c("docs", "words"),
  rel = FALSE, mark = TRUE, main, xlab, ylim, ...){
  
  # set x-label if missing
  if (missing(xlab)) xlab <- "date"
  # match ids with ids which appears in object$text
  if (type[1] == "words"){
    ids <- names(object$text)[names(object$text) %in% ids]
    insert <- "words"
  }
  else insert <- "documents"
  # generate x-values date (non-unique at this point)
  dates <- lubridate::floor_date(
    object$meta$date[match(ids, object$meta$id)], "month")
  # generate markers on every beginning year
  rangeYears <- lubridate::year(range(dates, na.rm = TRUE))
  if (mark) markYears <- as.Date(lubridate::floor_date(
    as.Date(as.character(rangeYears[1]:rangeYears[2]), format = "%Y"), "year"))
  else markYears <- NA
  # compute counts (of wordsordocuments)
  if (type[1] == "words"){
    docLengths <- lengths(object$text[match(ids, names(object$text))])
    counts <- sapply(split(docLengths, dates), sum)
  }
  else counts <- table(dates)
  
  if (rel){
    # compute normalisation
    if (type[1] == "words"){
      allDates <- lubridate::floor_date(
        object$meta$date[match(names(object$text), object$meta$id)], "month")
      allCounts <- sapply(split(lengths(object$text), allDates), sum)
    }
    else{
      allDates <- lubridate::floor_date(object$meta$date, "month")
      allCounts <- table(allDates)
    }
    # compute proportions
    proportion <- counts / allCounts[match(names(counts), names(allCounts))]
    # some preparation for plotting
    dateNames <- as.Date(names(proportion))
    proportion <- as.vector(proportion)
    # set main and ylim if missing
    if (missing(main)) main <- paste("Proportion of", insert, "over time")
    if (missing(ylim)) ylim <- c(0, 1)
    plot(dateNames, proportion, type = "l",
      main = main, xlab = xlab, ylim = ylim, ...)
    abline(v = markYears, lty = 2)
    tab = data.frame(date = dateNames, proportion = proportion)
  }
  else{
    # some preparation for plotting
    dateNames <- as.Date(names(counts))
    counts <- as.vector(counts)
    # set main if missing
    if (missing(main)) main <- paste("Count of", insert, "over time")
    plot(dateNames, counts, type = "l",
      main = main, xlab = xlab, ...)
    abline(v = markYears, lty = 2)
    tab = data.frame(date = dateNames, counts = counts)
  }
  invisible(tab)
}
