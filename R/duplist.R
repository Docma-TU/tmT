#' Creating List of Duplicates
#'
#' Creates a List of different types of Duplicates in a textmeta-object.
#'
#' This function helps to identify different types of Duplicates and gives the
#' ability to exclude these for further Analysis (e.g. LDA).
#'
#' @param object A textmeta-object.
#' @param paragraph Logical: Should be set to \code{TRUE} if the article is a
#' list of character strings, representing the paragraphs.
#' @return Named List:
#' \item{uniqueTexts}{ character}
#' \item{notDuplicatedTexts}{ character}
#' \item{idFakeDups}{ list}
#' \item{idRealDups}{ list}
#' \item{allTextDups}{ list}
#' \item{textOnlyDups}{ list}
#' \item{textMetaDups}{ list}
#' \item{textOthersDups}{ character}
#'
#' @keywords manip
#' @examples
#' texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
#' Teach a Man To Fish, and You Feed Him for a Lifetime",
#' A="A fake duplicate",
#' B="So Long, and Thanks for All the Fish",
#' B="So Long, and Thanks for All the Fish",
#' C="A very able manipulative mathematician, Fisher enjoys a real mastery
#' in evaluating complicated multiple integrals.",
#' C="A very able manipulative mathematician, Fisher enjoys a real mastery
#' in evaluating complicated multiple integrals.")
#'
#' corpus <- textmeta(meta=data.frame(id=c("A", "A", "B", "B", "C", "C"),
#' title=c("Fishing", "Fake duplicate", "Don't panic!", "towel day", "Sir Ronald", "Sir Ronald"),
#' date=c("1885-01-02", "1885-01-03", "1979-03-04", "1979-03-05", "1951-05-06", "1951-05-06"),
#' stringsAsFactors=FALSE), text=texts)
#'
#' duplicates <- deleteAndRenameDuplicates(object=corpus)
#' duplist(object=duplicates, paragraph = FALSE)
#' @export duplist

# Es existieren keine doppelten IDs durch deleteAndRenameDuplicates in read!
# Ausgabe soll (disjunkte!) Liste sein mit
# 1. uniqueTexts: Jeder vorkommende Text genau einmal
#    a) notDuplicatedTexts: Texte, fuer die keine identischen IDs oder Texte
#                       im Korpus vorkommen
# 2. idFakeDups: Texte mit gleicher ID und unterschiedlichem Text ("Reste" aus 3.)
# 3. idRealDups: Texte mit gleicher ID und gleichem Text (Meta differs)
# 4. (DIE EIGENTLICHE AUFGABE DIESER FUNKTION) Texte, deren Text gleich ist
#    allTextDups: mit allen Text-Duplikaten
#    a) textOnlyDups: mit unterschiedlichen Meta-Daten (insbesondere existiert kein
#                     Duplikat mit identischen Meta-Informationen)
#    b) textMetaDups: mit exakt gleichen Meta-Daten
#    c) textOthersDups: mit Duplikaten, die in allTextDups vorkommen, aber nicht
#                       in textMetaDups oder textOnlyDups.

duplist <- function(object, paragraph = FALSE){
  stopifnot(is.textmeta(object), is.logical(paragraph), length(paragraph) == 1)
  # help-function to create lists of IDs:
  foo_makeList <- function(dupType, to_replace){
    if (length(dupType) < 1) return(list())
    sorted_dups <- sort(names(object$text[dupType]))
    temp <- grep(to_replace, sorted_dups)
    dups_names <- gsub(pattern = to_replace, replacement = "", sorted_dups[temp])
    temp <- cbind(temp, c(temp[-1], length(sorted_dups) + 1) - 1)
    dupType <- apply(temp, 1, function(x) sorted_dups[x[1]:x[2]])
    if (is.matrix(dupType)){
      dupType <- as.list(as.data.frame(dupType, stringsAsFactors = FALSE))
    }
    names(dupType) <- dups_names
    return(dupType)
  }
  
  # 2. idFakeDups 3. idRealDups
  message("ID-Fake-Dups... ", appendLF = FALSE)
  idFakeDups <- foo_makeList(dupType = grep("_IDFakeDup", names(object$text)),
    to_replace = "_IDFakeDup1")
  message("next Step\nID-Real-Dups... ", appendLF = FALSE)
  idRealDups <- foo_makeList(dupType = grep("_IDRealDup", names(object$text)),
    to_replace = "_IDRealDup1")
  message("next Step\nUnique (and Not-Duplicated) Texts... ", appendLF = FALSE)
  
  # 1. uniqueTexts a) allUniqueTexts:
  if (paragraph == TRUE){
    textvek <- unlist(lapply(object$text, paste, collapse = " "))
  }
  else textvek <- unlist(object$text)
  text_same <- duplicated(textvek)
  text_same_fromLast <- duplicated(textvek, fromLast = TRUE)
  # 1) uniqueTexts:
  if (any(!text_same)) uniqueTexts <- names(object$text)[!text_same]
  else uniqueTexts <- character(0)
  # 1a) allUniqueTexts:
  ind = text_same | text_same_fromLast
  if (any(!ind)) allUniqueTexts <- names(object$text)[!ind]
  else allUniqueTexts <- character(0)
  
  message("next Step\nSame Texts... ", appendLF = FALSE)
  # 4. Same text, but different IDs:
  if (any(ind)){
    ind <- which(ind)
    # allTextDups:
    allTextDups_names <- names(object$text)[ind]
    allTextDups <- lapply(na.omit(unique(textvek[ind])),
      function(x) allTextDups_names[which(textvek[ind] == x)])
    # b) textMetaDups:
    doNotTestID <- colnames(object$meta)[colnames(object$meta) != "id"]
    meta_same <- duplicated(object$meta[ind,doNotTestID])|
      duplicated(object$meta[ind,doNotTestID], fromLast = TRUE)
    if (any(meta_same)){
      textMetaDups_names <- allTextDups_names[meta_same]
      textMetaDups <- lapply(na.omit(unique(textvek[ind[meta_same]])),
        function(x) textMetaDups_names[which(textvek[ind[meta_same]] == x)])
    }
    else textMetaDups <- list()
    if (any(!meta_same)){
      # remaining Indices for a) and c):
      ind <- ind[!meta_same]
      allTextDups_names <- allTextDups_names[!meta_same]
      # a) textOnlyDups:
      text_same <- duplicated(textvek[ind]) | duplicated(textvek[ind], fromLast = TRUE)
      if (any(text_same)){
        textOnlyDups_names <- allTextDups_names[text_same]
        textOnlyDups <- lapply(na.omit(unique(textvek[ind[text_same]])),
          function(x) textOnlyDups_names[which(textvek[ind[text_same]] == x)])
      }
      else textOnlyDups <- list()
      # c) textOthersDups:
      if (any(!text_same)) textOthersDups <- allTextDups_names[!text_same]
      else textOthersDups <- character(0)
    }
    else {
      textOnlyDups <- list()
      textOthersDups <- character(0)
    }
  }
  else {
    allTextDups <- list()
    textOnlyDups <- list()
    textMetaDups <- list()
    textOthersDups <- character(0)
  }
  message("Success")
  res <- list(uniqueTexts = uniqueTexts, notDuplicatedTexts = allUniqueTexts,
    idFakeDups = idFakeDups, idRealDups = idRealDups,
    allTextDups = allTextDups, textOnlyDups = textOnlyDups,
    textMetaDups = textMetaDups, textOthersDups = textOthersDups)
  class(res) <- "duplist"
  summary(res)
  return(res)
}

#' @rdname duplist
#' @param x an R Object.
#' @export
is.duplist <- function(x){
  if(class(x) != "duplist"){
    message("Object is not of class \"duplist\".")
    return(FALSE)
  }
  if(!is.list(x)){
    message("Object is not a list.")
    return(FALSE)
  }
  if(!all(c("uniqueTexts", "notDuplicatedTexts", "idFakeDups", "idRealDups",
    "allTextDups", "textOnlyDups", "textMetaDups", "textOthersDups") %in% names(x))){
    message("Listnames incorrect.")
    return(FALSE)
  }
  if(!all(is.character(x$uniqueTexts), is.character(x$notDuplicatedTexts),
    is.list(x$idFakeDups), is.list(x$idRealDups), is.list(x$allTextDups),
    is.list(x$textOnlyDups), is.list(x$textMetaDups), is.character(x$textOthersDups))){
    message("Structure of List incorrect.")
    return(FALSE)
  }
  return(TRUE)
}

#' @rdname duplist
#' @param ... further arguments for print and summary. Not implemented.
#' @export
print.duplist <- function(x, ...){
  if(!is.duplist(x)){
    print.default(x)
  }
  else{
  cat("Duplist, List of (Lists of) IDs with Names:
 \"uniqueTexts\", \"notDuplicatedTexts\", \"idFakeDups\", \"idRealDups\",
 \"allTextDups\", \"textOnlyDups\", \"textMetaDups\", \"textOthersDups\".\n")
  invisible(x)
  }
}

#' @rdname duplist
#' @export
summary.duplist <- function(object, ...){
  stopifnot(is.duplist(object))
  print(object)
  cat("Calculate Numbers of IDs and Texts...\n")
  cat(" Number of Unique Texts:", length(object$uniqueTexts), "\n")
  cat(" Number of Not-Duplicated Texts:", length(object$notDuplicatedTexts), "\n")
  cat(" Number of Fake-Dup IDs:", length(object$idFakeDups), "\n")
  cat("  Number of Texts with Fake-Dup IDs:", sum(lengths(object$idFakeDups)), "\n")
  cat(" Number of Real-Dup IDs:", length(object$idRealDups), "\n")
  cat("  Number of Texts with Real-Dup IDs:", sum(lengths(object$idRealDups)), "\n")
  cat(" Number of different Text-Dups:", length(object$allTextDups), "\n")
  cat("  Number of all Text-Dups:", sum(lengths(object$allTextDups)), "\n")
  cat("   Number of different Text-Dups with identical Meta (except ID):", length(object$textMetaDups), "\n")
  cat("    Number of all Text-Meta-Dups:", sum(lengths(object$textMetaDups)), "\n")
  cat("   Number of Text-Dups which do not pass criteria above:", length(object$textOthersDups), "\n")
  invisible(object)
}
