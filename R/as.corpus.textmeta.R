#' Transform textmeta to corpus
#'
#' Transfers data from a \code{\link{textmeta}} object to a
#' \code{\link[quanteda]{corpus}} object -  the way text data is stored in the
#' package \code{\link[quanteda]{quanteda}}.
#'
#' @param object \code{\link{textmeta}} object
#' @param docnames Character: string with the column of object$meta
#' which should be kept as \code{\link[quanteda]{docnames}}.
#' @param docvars Character: vector with columns of object$meta which
#' should be kept as \code{\link[quanteda]{docvars}}.
#' @param metadoc Character: vector with columns of object$meta which
#' should be kept as \code{\link[quanteda]{metadoc}}.
#' @param ... Additional parameters like \code{compress} for \code{\link[quanteda]{corpus}}.
#' @return \code{\link[quanteda]{corpus}} object
#' @keywords manip
#' @export as.corpus.textmeta

as.corpus.textmeta <- function(object, docnames = "id",
  docvars = setdiff(colnames(object$meta), "id"), metadoc = character(), ...){

  # stop if parameters set wrong
  stopifnot(tosca::is.textmeta(object), is.character(docnames), length(docnames) == 1,
    is.character(docvars), is.character(metadoc),
    all(union(union(docnames, docvars), metadoc) %in% colnames(object$meta)))

  texts <- sapply(object$text, paste, collapse = "\n\n")
  id <- object$meta[,docnames]
  vars <- as.data.frame(object$meta[,docvars], stringsAsFactors = FALSE)
  colnames(vars) <- docvars
  meta <- as.data.frame(object$meta[,metadoc], stringsAsFactors = FALSE)
  colnames(meta) <- metadoc

  corp <- corpus(x = texts, docnames = id, docvars = vars, ...)
  metadoc(corp) <- meta

  return(corp)
}
