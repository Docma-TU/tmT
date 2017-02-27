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

deleteAndRenameDuplicates = function(object, paragraph = FALSE){
  if (is.null(object$meta)){ #if do.meta == FALSE:
    ind = which(duplicated(names(object$text)) | duplicated(names(object$text),
                                                            fromLast = TRUE))
    textvek = ifelse(paragraph == TRUE,
                     unlist(lapply(object$text, paste, collapse = " ")),
                     unlist(object$text[ind]))
    # Delete duplicates of ID !and! text:
    to_del = ind[duplicated(textvek)]
    if (length(to_del) > 0){
      object$text = object$text[-to_del]
    }
    # Rename if text differs:
    to_rename = ind[!duplicated(textvek)]
    if (length(to_rename) > 0){
      names(object$text)[to_rename] = paste0(names(object$text)[to_rename],
                                             "_Dup", 1:length(to_rename))
    }
    return(object)
  }
  # Ansonsten existieren text und meta:
  # 3. Artikel mit komplett identischen ID, Text, Meta werden geloescht:
  ind = which(duplicated(names(object$text)) | duplicated(names(object$text),
                                                          fromLast = TRUE))
  textvek = ifelse(paragraph == TRUE,
                   unlist(lapply(object$text, paste, collapse = " ")),
                   unlist(object$text[ind]))
  to_del = ind[duplicated(object$meta[ind,]) & duplicated(textvek)]
  if (length(to_del) > 0){
    object$text = object$text[-to_del]
    object$meta = object$meta[-to_del,]
    ind = which(duplicated(names(object$text)) | duplicated(names(object$text),
                                                            fromLast = TRUE))
  }
  
  # 1. Artikel-IDs, deren IDs gleich, der Text aber unterschiedlich ist:
  textvek = ifelse(paragraph == TRUE,
                   unlist(lapply(object$text, paste, collapse = " ")),
                   unlist(object$text[ind]))
  text_same = duplicated(textvek) | duplicated(textvek, fromLast = TRUE)
  to_rename = ind[!text_same]
  if (length(to_rename) > 0){
    new_ids = paste0(names(object$text)[to_rename], "_FakeDup", 1:length(to_rename))
    names(object$text)[to_rename] = new_ids
    object$meta$id[to_rename] = new_ids
    ind = ind[text_same]
  }
  
  # 2. Artikel mit identischer ID und Text (aber unterschiedlichen Meta-Daten)
  # a) Date ist identisch:
  date_same = duplicated(object$meta$date[ind]) | duplicated(object$meta$date[ind],
                                                             fromLast = TRUE)
  to_rename = ind[date_same]
  if (length(to_rename) > 0){
    new_ids = paste0(names(object$text)[to_rename], "_DateDup", 1:length(to_rename))
    names(object$text)[to_rename] = new_ids
    object$meta$id[to_rename] = new_ids
  }
  # Fuer die restlichen Artikel gilt b) Date ist unterschiedlich:
  to_rename = ind[!date_same]
  if (length(to_rename) > 0){
    new_ids = paste0(names(object$text)[to_rename], "_RealDup", 1:length(to_rename))
    names(object$text)[to_rename] = new_ids
    object$meta$id[to_rename] = new_ids
  }
  return(object)
}