#' Delete And Rename Articles with the same ID
#'
#' Deletes Articles with the same ID and same text. Renames the ID of Articles
#' with the same ID but different text-component (e.g. _FakeDup, _DateDup, _RealDup).
#'
#' @param object A textmeta-object as a result of a read-function.
#' @param paragraph Logical: Should be set to \code{TRUE} if the article is a
#' list of character strings, representing the paragraphs.

# Gleiche IDs sollen bereits in den read-Funktionen aufgeloest werden:
# 1. Artikel-IDs, deren IDs gleich, der Text aber unterschiedlich ist
#     -> hier _FakeDup1 ... _FakeDupn anhaengen
#    (hier keine Option fuer gleiche Meta-Daten, da Text unterschiedlich,
#     also sind gleiche Meta-Daten nicht zu erwarten)
# 2. Artikel mit identischer ID und Text (aber unterschiedlichen Meta-Daten)
#    a) Meta-Datum Date ist identisch
#     -> hier _DateDup1 ... _DateDupn anhaengen
#    b) Meta-Datum Date ist unterschiedlich
#     -> hier _RealDup1 ... _RealDupn anheangen
# 3. Artikel mit komplett identischen ID, Text, Meta werden geloescht!!

deleteAndRenameDuplicates <- function(object, paragraph = FALSE){
  if (is.null(object$meta)){ #if do.meta == FALSE:
    ind <- which(duplicated(names(object$text)) | duplicated(names(object$text),
                                                            fromLast = TRUE))
    if (length(ind) < 1) return(object)
    if (paragraph == TRUE){
      textvek <- unlist(lapply(object$text[ind], paste, collapse = " "))
    }
    else textvek <- unlist(object$text[ind])
    # Delete duplicates of ID !and! text:
    to_del <- ind[duplicated(textvek)]
    if (length(to_del) > 0){
      cat(paste("Delete Duplicates:", length(to_del)))
      object$text <- object$text[-to_del]
      cat("  next Step\n")
    }
    # Rename if text differs:
    to_rename <- which(duplicated(names(object$text)) | duplicated(names(object$text),
                                                                   fromLast = TRUE))
    if (length(to_rename) > 0){
      ind_loop = logical(length(names(object$text)))
      ind_loop[to_rename] = TRUE
      cat(paste("Rename Fake-Duplicates:", length(to_rename)))
      for (i in unique(names(object$text)[to_rename])){
        to_rename_loop <- (names(object$text) == i) & ind_loop
        names(object$text)[to_rename_loop] <- paste0(names(object$text)[to_rename_loop],
                                                     "_FakeDup", 1:sum(to_rename_loop))
      }
      cat("  next Step\n")
    }
    cat("Success\n")
    return(object)
  }
  # Ansonsten existieren text und meta:
  # 3. Artikel mit komplett identischen ID, Text, Meta werden geloescht:
  ind <- which(duplicated(names(object$text)) | duplicated(names(object$text),
                                                          fromLast = TRUE))
  if (length(ind) < 1){
    cat("Success\n")
    return(object)
  }
  if (paragraph == TRUE){
    textvek <- unlist(lapply(object$text[ind], paste, collapse = " "))
  }
  else textvek <- unlist(object$text[ind])
  to_del <- ind[duplicated(object$meta[ind,]) & duplicated(textvek)]
  if (length(to_del) > 0){
    cat(paste("Delete Duplicates:", length(to_del)))
    object$text <- object$text[-to_del]
    object$meta <- object$meta[-to_del,]
    cat("  next Step\n")
    ind <- which(duplicated(names(object$text)) | duplicated(names(object$text),
                                                            fromLast = TRUE))
  }
  
  # 1. Artikel-IDs, deren IDs gleich, der Text aber unterschiedlich ist:
  if (length(ind) < 1){
    cat("Success\n")
    return(object)
  }
  if (paragraph == TRUE){
    textvek <- unlist(lapply(object$text[ind], paste, collapse = " "))
  }
  else textvek <- unlist(object$text[ind])
  text_same <- duplicated(textvek) | duplicated(textvek, fromLast = TRUE)
  to_rename <- ind[!text_same]
  if (length(to_rename) > 0){
    ind_loop = logical(length(names(object$text)))
    ind_loop[to_rename] = TRUE
    cat(paste("Rename Fake-Duplicates:", length(to_rename)))
    for (i in unique(names(object$text)[to_rename])){
      to_rename_loop <- names(object$text) == i & ind_loop
      new_ids <- paste0(names(object$text)[to_rename_loop], "_FakeDup",
                        1:sum(to_rename_loop))
      names(object$text)[to_rename_loop] <- new_ids
      object$meta$id[to_rename_loop] <- new_ids
    }
    cat("  next Step\n")
    ind <- ind[text_same]
  }
  
  # 2. Artikel mit identischer ID und Text (aber unterschiedlichen Meta-Daten)
  # a) Date ist identisch:
  if (length(ind) < 1){
    cat("Success\n")
    return(object)
  }
  date_same <- duplicated(object$meta$date[ind]) | duplicated(object$meta$date[ind],
                                                             fromLast = TRUE)
  to_rename <- ind[date_same]
  if (length(to_rename) > 0){
    ind_loop = logical(length(names(object$text)))
    ind_loop[to_rename] = TRUE
    cat(paste("Rename Duplicates with different Meta-Information, but same Date:",
              length(to_rename)))
    for (i in unique(names(object$text)[to_rename])){
      to_rename_loop <- names(object$text) == i & ind_loop
      new_ids <- paste0(names(object$text)[to_rename_loop], "_DateDup",
                        1:sum(to_rename_loop))
      names(object$text)[to_rename_loop] <- new_ids
      object$meta$id[to_rename_loop] <- new_ids
    }
    cat("  next Step\n")
  }
  # Fuer die restlichen Artikel gilt b) Date ist unterschiedlich:
  to_rename <- ind[!date_same]
  if (length(to_rename) > 0){
    cat(paste("Rename Duplicates with different Date-Information:",
              length(to_rename)))
    for (i in unique(names(object$text)[to_rename])){
      to_rename_loop <- which(names(object$text) == i)
      new_ids <- paste0(names(object$text)[to_rename_loop], "_RealDup",
                        1:sum(to_rename_loop))
      names(object$text)[to_rename_loop] <- new_ids
      object$meta$id[to_rename_loop] <- new_ids
    }
    cat("  next Step\n")
  }
  cat("Success\n")
  return(object)
}