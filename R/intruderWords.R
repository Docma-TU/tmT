#' Function to fit LDA model
#'
#' tba
#'
#' @param beta A matrix of word-probabilities or frequency table for the topics. Each row is a topic, each column a word. The rows will be divided by the row sums, if they are not 1.
#' @param byScore logical should the score of \code{top.topic.words} from the \code{lda} package be used?
#' @param numTopwords the number of topwords to be used for the intruder words.
#' @param numIntruder intended number of intruder words. If \code{numIntruder} is a integer vector, the number would be sampled for each topic.
#' @param numOutwords integer. Number of words per topic, including the intruder words.
#' @param noTopic logical. Is \code{x} input allowed to mark nonsense topics?
#' @param printSolution tba
#' @param oldResult result object from an unfinished run of \code{intruderWords}. If oldResult is used, all other parameter will be ignored.
#' @param test logical enables test mode
#' @param testinput input for function tests
#' @return tba
#' @seealso tba
#' @references tba
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export intruderWords

intruderWords <- function(beta=NULL, byScore = TRUE, numTopwords = 30L, numIntruder = 1L, numOutwords = 5L, noTopic=TRUE, printSolution = FALSE, oldResult=NULL, test=FALSE, testinput=NULL){
    if(is.null(beta) & is.null(oldResult))stop("beta or oldResult needs to be specified")
    if((!is.null(beta) & (!is.matrix(beta) | !is.numeric(beta))))stop("beta needs to be a numeric matrix")
    if(!is.null(oldResult)){beta <- oldResult$beta
                            byScore  <- oldResult$byScore
                            numTopwords  <- oldResult$numTopwords
                            numIntruder  <- oldResult$numIntruder
                            numOutwords  <- oldResult$numOutwords
                            noTopic <- oldResult$noTopic
                            cat(paste("parameter from old result used \nbyScore = ", byScore, "\nnumTopwords = ", numTopwords, "\nnumIntruder = ", paste(numIntruder, collapse=" "), "\nnumOutwords = ", numOutwords, "\nnoTopic = ", noTopic, "\n \n", sep=""))}
                            numTopwords  <- as.integer(numTopwords)
                            numIntruder  <- as.integer(numIntruder)
                            numOutwords  <- as.integer(numOutwords)
                            stopifnot(is.logical(byScore), is.integer(numTopwords), is.integer(numIntruder), is.integer(numOutwords), is.logical(noTopic), length(byScore)==1, length(numTopwords)==1, length(numOutwords)==1, length(noTopic)==1)
    if(numTopwords<numOutwords)stop("numTopwords needs to be greater then numOutwords")
    if(max(numIntruder) > 0.25 * numOutwords)stop("Too many intruder")

    if(any(beta==0)) beta <- beta #+ 1e-05
    if(!all(rowSums(beta)==1)) beta <- beta / rowSums(beta)
    if(byScore){scores <- apply(beta, 2, function(x) x *
                                     (log(x + 1e-05) - sum(log(x + 1e-05))/length(x)))}else{scores <- beta}
    topwords <- apply(scores, 1, function(x) colnames(scores)[order(x, decreasing = TRUE)[1:numTopwords]])
    topwords2 <- unlist(topwords)
    result <- matrix(NA, nrow(beta),3)
    if(!is.null(oldResult)) result <- oldResult$result
colnames(result) <- c("numIntruder", "missIntruder", "falseIntruder")
    nonNAtopics <- which(is.na(result[,1]))
    for(i in sample(nonNAtopics)){
        cat(paste("counter", nrow(result) - sum(is.na(result[,1])) +1, "\n"))
        likelywords <- names(scores[i,order(scores[i,], decreasing=TRUE)[1:(ncol(scores)*0.25)]])
        intruder <- sample(setdiff(topwords2,likelywords), size=if(length(numIntruder)==1){numIntruder}else{sample(numIntruder,1)})
        outwords <- sample(c(intruder, topwords[1:(numOutwords - length(intruder)),i]))
        outwords2 <- paste(seq_along(outwords), outwords)
            repeat{
            cat(c(paste(outwords2, collapse= "\n"), "\n"))
            if(!test[1]){input <- readline(prompt = "Input:")}else{input <- testinput[1]; testinput <- testinput[-1]}
            if(input=="q"){break}#exit
            if(input=="h"){cat(paste("h for help \nq for quit \nx for no topic identifiable \n \nbyScore = ", byScore, "\nnumTopwords = ", numTopwords, "\nnumIntruder = ", numIntruder, "\nnumOutwords = ", numOutwords, "\nnoTopic", noTopic, "\n \n", sep="")); next}#exit
            if(input=="x" & noTopic){result[i,] <- c(0,NA,NA); break}#exit
            input <- as.numeric(strsplit(input, " ")[[1]])
            if(any(is.na(input)) | any(!(input %in% c(0,seq_along(outwords)))) | length(input)==0){cat("Only space seperated input of line number or 0 \n") ; next}
            break}
        if(input[1]=="q"){break}#exit
        if(input[1]=="x"){next}#exit
        result[i,] <- c(length(intruder), length(intruder) - sum(intruder %in% outwords[input]), sum(!(outwords[input] %in% intruder)))
        if(printSolution) cat(paste("True Intruder:", paste(intruder, collapse=" "), "\n"))
    }
    result <- list(result=result, beta=beta, byScore=byScore, numTopwords=numTopwords, numIntruder=numIntruder, numOutwords=numOutwords, noTopic = noTopic)
    class(result) <- "IntruderWords"
return(result)
}

#' @export
print.IntruderWords <- function(x, ...){
    print(data.frame("byScore"=x$byScore, "numTopwords"=x$numTopwords,  "numIntruder"=paste(x$numIntruder, collapse=" "), "numOutwords"=x$numOutwords, "noTopic"=x$noTopic))
    cat("\n Results: \n")
print.default(x$result)
}

#' @export
summary.IntruderWords <- function(object, ...){
    cat("Non interpretable Topics:",
        sum(is.na(object$result[,"missIntruder"])) - sum(is.na(object$result[,"numIntruder"])),
        "\nNon evaluated Topics:",
        sum(is.na(object$result[,"numIntruder"])), "\n\n")
    print(data.frame("byScore"=object$byScore, "numTopwords"=object$numTopwords,
                     "numIntruder"=paste(object$numIntruder, collapse=" "),
                     "numOutwords"=object$numOutwords, "noTopic"=object$noTopic))
    if(sum(is.na(object$result[,"numIntruder"])) == 0){
        cat("\n",
            "Number of meaningful topics:",
            nrow(object$result) - sum(is.na(object$result[,"missIntruder"])), "out of", nrow(object$result),
            paste0("(", round(100*(1-sum(is.na(object$result[,"missIntruder"]))/nrow(object$result)),2), "%)"),
            "\n",
            "Correct topics:",
            sum(object$result[,"missIntruder"]==0 & object$result[,"falseIntruder"]==0, na.rm=TRUE),
paste0("(", round(100*(sum(object$result[,"missIntruder"]==0 & object$result[,"falseIntruder"]==0, na.rm=TRUE) / nrow(object$result)),2), "%)"),
            "\n\n")
        cat("\n", "Table of Intruders")
        missIntTable <- table(object$result[,"numIntruder"])
        print(missIntTable)
        cat("\n",
            "Mean of number of missed Intruder:", mean(object$result[,"missIntruder"], na.rm=TRUE),
            "\n", "Table of missing Intruders")
        missIntTable <- table(object$result[,"missIntruder"])
        print(missIntTable)
        cat("\n",
            "Mean of number of false Intruder:", mean(object$result[,"falseIntruder"], na.rm=TRUE),
            "\n", "Table of false Intruders")
        missIntTable <- table(object$result[,"falseIntruder"])
        print(missIntTable)
    }

}
