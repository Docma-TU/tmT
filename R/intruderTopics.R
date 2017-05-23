#' Function to fit LDA model
#'
#' tba
#'
#' @param text tba
#' @param beta A matrix of word-probabilities or frequency table for the topics. Each row is a topic, each column a word. The rows will be divided by the row sums, if they are not 1.
#' @param theta tba
#' @param id tba
#' @param numIntruder intended number of intruder words. If \code{numIntruder} is a integer vector, the number would be sampled for each topic.
#' @param numOuttopics tba integer. Number of words per topic, including the intruder words.
#' @param byScore logical should the score of \code{top.topic.words} from the \code{lda} package be used?
#' @param minWords tba
#' @param minOuttopics tba
#' @param stopTopics tba
#' @param printSolution tba
#' @param oldResult result object from an unfinished run of \code{intruderWords}. If oldResult is used, all other parameter will be ignored.
#' @param test logical enables test mode
#' @param testinput input for function tests
#' @return tba
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @seealso tba
#' @references tba
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' ##---- Should be DIRECTLY executable !! ----
#' @export intruderTopics

intruderTopics <- function(text= NULL, beta=NULL, theta=NULL, id=NULL, numIntruder=1, numOuttopics=4, byScore=TRUE, minWords=0L, minOuttopics=0L, stopTopics=NULL, printSolution=FALSE, oldResult=NULL, test=FALSE, testinput=NULL){
  if((is.null(beta) | is.null(theta)) & is.null(oldResult))stop("beta and theta needs to be specified")
    if(is.null(beta) & is.null(oldResult))stop("beta and theta or oldResult needs to be specified")
    if((!is.null(beta) & (!is.matrix(beta) | !is.numeric(beta))))stop("beta needs to be a numeric matrix")
    if((!is.null(theta) & (!is.matrix(theta) | !is.numeric(theta))))stop("theta needs to be a numeric matrix")
    if(!is.null(oldResult)){beta <- oldResult$beta
                            theta <- oldResult$theta
                            byScore  <- oldResult$byScore
                            id  <- oldResult$id
                            numIntruder  <- oldResult$numIntruder
                            numOuttopics  <- oldResult$numOuttopics
                            minWords <- oldResult$minWords
                            minOuttopics <- oldResult$minOuttopics
                            stopTopics <- oldResult$stopTopics
                            cat(paste("parameter from old result used \nbyScore = ", byScore, "\nnumIntruder = ", paste(numIntruder, collapse=" "), "\nnumOuttopics = ", numOuttopics, "\nminWords = ", minWords, "\nminOuttopics = ", minOuttopics, "\n \n", sep=""))}
    if(is.null(oldResult)){
        if(!is.null(id)) colnames(theta) <- id
        if(minWords)theta <- theta[,colSums(theta)>=minWords]
        if(!is.null(stopTopics)) theta <- theta[-stopTopics,]
        if(minOuttopics){theta <- theta[,apply(theta,2,function(x)x[order(x, decreasing=TRUE)[numOuttopics-min(numIntruder)]]>=minOuttopics)]}
    id <- colnames(theta)}
    idmatch <- match(colnames(theta), names(text))
    if(any(is.na(idmatch)))stop("Missing texts")
    if(length(idmatch)==0)stop("Missing id's in id or colnames(theta) or wrong texts")
    if(!all(rowSums(beta)==1)) beta <- beta / rowSums(beta)
    if(byScore){scores <- apply(beta, 2, function(x) x *
                                    (log(x + 1e-05) - sum(log(x + 1e-05))/length(x)))}else{scores <- beta}
    topwords <- apply(scores, 1, function(x) colnames(scores)[order(x, decreasing = TRUE)[1:10]])
    if(!is.null(stopTopics)) topwords <- topwords[,-stopTopics]
    if(!all(rowSums(theta)==1)) theta <- t(t(theta) / colSums(theta))
    input <- 0
    if(is.null(oldResult)){
        result <- data.frame(id=character(), numIntruder=integer(), missIntr=integer(), falseIntr=integer(), stringsAsFactors=FALSE)
        unusedID <- id
    }else{
    result <- oldResult$result
    unusedID <- oldResult$unusedID
}

    while(!(input[1]=="q" | length(id)==0)){
        cat(paste("counter = ",nrow(result)+1, "\n"))
        sID <- sample(unusedID,1)
        numIntruderS <- sample(numIntruder,1)
        possibleIntruder <- which(theta[,sID]==0)
        if(length(possibleIntruder)==0){ unusedID <- unusedID[-which(unusedID==sID)]; next}
        toptopics <- topwords[,order(theta[,sID], decreasing=TRUE)[1:(numOuttopics-numIntruderS)]]
        intruder <- topwords[,sample(possibleIntruder,numIntruderS)]
        posIntruder <- sample(numOuttopics, numIntruderS)
        toptopics2 <- matrix(NA, 10, numOuttopics)
        toptopics2[,posIntruder] <- intruder
        toptopics2[,-posIntruder] <- toptopics[,sample(ncol(toptopics))]
        if(length(posIntruder)==0) toptopics2 <- toptopics[,sample(ncol(toptopics))]
        toptopics2 <- rbind(1:ncol(toptopics2), toptopics2)
        toptopics2 <- apply(toptopics2,2,paste, collapse=" ")

        repeat{
            htmltools::html_print(htmltools::HTML(c("<h2>Document: ", sID, "</h2><p>", paste(text[[sID]], "<p>"))))
            cat(c(paste(toptopics2, collapse= "\n"), "\n"))
            if(!test[1]){input <- readline(prompt = "Input:")}else{input <- testinput[1]; testinput <- testinput[-1]}
            if(input=="q"){break}#exit
            if(input=="h"){cat(paste("h for help \nq for quit \n \nbyScore = ", byScore, "\nnumIntruder = ", numIntruder, "\nnumOuttopics = ", numOuttopics, "\n \n", sep="")); next}#exit
            input <- as.numeric(strsplit(input, " ")[[1]])
            if(any(is.na(input)) | any(!(input %in% 0:numOuttopics)) | length(input)==0){cat("Only space seperated input of line number or 0 \n \n") ; next}
            break}

        if(input[1]=="q"){break}#exit
        if(length(posIntruder)==0){result <- rbind(result,data.frame(id=sID, numIntruder=numIntruderS, missIntr=numIntruderS - sum(input %in% posIntruder), falseIntr=sum(input %in% (1:numOuttopics)), stringsAsFactors=FALSE))}else{
        result <- rbind(result,data.frame(id=sID, numIntruder=numIntruderS, missIntr=numIntruderS - sum(input %in% posIntruder), falseIntr=sum(input %in% (1:numOuttopics)[-posIntruder]), stringsAsFactors=FALSE))}
        unusedID <- unusedID[-which(unusedID==sID)]
        if(printSolution) cat(paste("True Intruder:", paste(sort(posIntruder), collapse=" "), "\n"))
        cat(paste(length(unusedID), "left\n"))
    }
    result <- list(result=result, beta=beta, theta= theta, id=id, byScore=byScore, numIntruder=numIntruder, numOuttopics=numOuttopics, minWords=minWords, minOuttopics=minOuttopics, unusedID = unusedID, stopTopics = stopTopics)
    class(result) <- "IntruderTopics"
return(result)
}


#' @export
print.IntruderTopics <- function(x, ...){
    print(data.frame("byScore"=x$byScore, "numIntruder"=paste(x$numIntruder, collapse=" "), "numOuttopics"=x$numOuttopics, minWords=x$minWords, minOuttopics=x$minOuttopics, stopTopics=paste(x$stopTopics, collapse=" ")))
    cat("\n Results: \n")
print.default(x$result)
}

#' @export
summary.IntruderTopics <- function(object, ...){
cat("ToDo \n")
}
