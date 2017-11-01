#' Plotting Topics over Time relative to Corpus
#' 
#' Creates a pdf including a heat map. For each topic, the heat map shows the deviation of 
#' its current share from its mean share. Shares can be calculated on corpus level or on subcorpus level concerning LDA vocabulary.
#' Shares can be calculated in absolute deviation from the mean or relative to the mean of the topic to account for different topic strengths.
#' 
#' @param object \code{\link{textmeta}} object with strictly tokenized \code{text}
#' component (calculation of proportion on document lengths) or
#' \code{\link{textmeta}} object which contains only the \code{meta} component
#' (calculation of proportion on count of words out of the LDA vocabukary in each
#' document)
#' @param select Numeric vector containing the numbers of the topics to be plotted. Defaults to all topics.
#' @param ldaresult LDA result object.
#' @param ldaID Character vector containing IDs of the texts.
#' @param norm Logical. Should the values be normalized by the mean topic share to account for differently sized topics? Defaults to FALSE.
#' @param file Character vector containing the path and name for the pdf output file.
#' @param tnames Character vector with labels for the topics.
#' @param date_breaks (default: \code{1}) how many labels should be shown on the x axis.
#' If is \code{5} every fifth label is drawn.
#' @param unit \code{character} (default: \code{"year"}) to which unit should
#' dates be floored
#' @return A pdf.
#' @author Kira Schacht (<kira.schacht@@tu-dortmund.de>)
#' @keywords ~kwd1 ~kwd2
#' @examples ##
#' @export totHeat

totHeat <- function(object, ldaresult, ldaID,
  select = 1:nrow(ldaresult$document_sums), tnames,
  norm = FALSE, file, date_breaks = 1, unit = "year"){
  
  stopifnot(is.textmeta(object), is.character(ldaID),
    all(as.integer(select) == select), min(select) > 0,
    max(select) <= nrow(ldaresult$document_sums))
  
  if(missing(tnames)) tnames <- paste0("T", select, ".",
    lda::top.topic.words(ldaresult$topics, 1)[select])
  
  #create data frame. rows: documents, columns: topics
  tmp <- data.frame(t(ldaresult$document_sums))
  
  #get dates for all documents to be visualized and
  #round to years, respectively unit
  tmpdate <- lubridate::floor_date(object$meta$date[
    match(ldaID, object$meta$id)], unit = unit)
  #sum document-levels values to months, respectively unit
  tmp <- aggregate(tmp, by = list(date = tmpdate), FUN = sum)
  
  ### Prepare normalization data ###
  #get dates of every document in the corpus and
  #count words for every document
  if(is.null(object$text)){
    normdates <- tmpdate
    normsums <- aggregate(lengths(ldaresult$assignments),
      by = list(date = normdates), FUN = sum)
  }
  else{
    normdates <- lubridate::floor_date(object$meta$date[
      match(names(object$text), object$meta$id)], unit = unit)
    normsums <- aggregate(lengths(object$text),
      by = list(date = normdates), FUN = sum)
  }
  #tidy up
  rm(normdates)
  
  ### Normalize data ###
  normsums <- normsums[match(tmp$date, normsums$date),]
  tmp[,2:length(tmp)] <- apply(tmp[,2:length(tmp)],2,function(y) y/normsums$x)
  #cell values are now shares in document x of topic y
  
  #filter for topics to be plotted
  tmp <- tmp[, c(1,select+1)]
  
  #get mean for each topic over entire time: column means
  tmeans <- apply(tmp[2:length(tmp)], 2, mean)
  #calculate absolute distance to mean. normalize distance with mean if specified
  for(i in 1:nrow(tmp)){
    tmp[i,2:length(tmp)] <- tmp[i,2:length(tmp)] - tmeans
    if(norm == TRUE){
      tmp[i,2:length(tmp)] <- tmp[i,2:length(tmp)] / tmeans
    }
  }
  
  breaks <- character(length(tmp$date))
  ind <- c(seq(1, length(breaks), by = date_breaks), length(breaks))
  breaks[ind] <- as.character(tmp$date[ind])
  
  #plot heat map
  if(!missing(file)) pdf(file, width = 56/(3+(0.1*nrow(ldaresult$topics))))
  gplots::heatmap.2(t(as.matrix(tmp[-1])), Colv = NA, dendrogram = 'row',
    #enable legend, disable histogram and deviation traces
    trace = 'none', density.info = 'none', key = T,
    #colours
    col=colorRampPalette(c("#0571b0", "#ffffff","#ca0020"))(50),
    #settings for legend
    keysize=1, key.par=list(mar=c(3,0,3,7), bty="n", fg="white"), key.xlab = NA,
    #layout: title, then legend, then dendrogram and heat map
    lmat=rbind(c(0,3,3,3), c(0,5,4,5), c(2,1,1,1)), lhei=c(0.13,0.18,0.65), lwid=rep(1, 4),
    #separate heat map cells with white space
    rowsep = 1:(ncol(tmp)-1), colsep = 1:nrow(tmp),
    #configure labels for heat map
    labRow = tnames, labCol = breaks, margins = c(8,12),
    cexRow = 1.2, cexCol = 1.2, srtCol = 45,
    main = ifelse(norm == T,
      "Normalized Deviation of Topic Shares from Mean Topic Share",
      "Absolute Deviation of Topic Shares from Mean Topic Share"))
  if(!missing(file)) dev.off()
  names(tmp)[2:(length(select)+1)] <- tnames
  invisible(tmp)
}