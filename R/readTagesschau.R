#' Read Tagesschau files
#'
#' Reads XML-files from Tagesschau and separates the text and meta data.
#'
#' @param path Character: string with path where the data files are.
#' @param file Character: string with names of the XML files.
#' @param encoding encoding of the input files.
#' @param type Character: string whether to get one text with paragraphs
#' per show (condensed) or one text per comment/message/report (detail).
#' @return \code{\link{textmeta}} object
#' @author Jonas Rieger (<jonas.rieger@@tu-dortmund.de>)
#' @keywords manip
#'
#' @export readTagesschau
#'
readTagesschau <- function(path = getwd(),
  file = list.files(path = path, pattern = "*.xml$",
    full.names = FALSE, recursive = TRUE), encoding = "utf-8",
  type = c("condensed", "detail")){

  stopifnot(is.character(file), is.character(path), is.character(encoding),
    length(path) == 1, length(encoding) == 1, is.character(type),
    all(type %in% c("condensed", "detail")))
  if(type[1] == "condensed"){
    return(readTagessschau.condensed(path = path, file = file, encoding = encoding))
  }

  text <- NULL
  meta <- NULL
  metamult <- NULL
  for (i in seq_along(file)) {
    cat(paste(file[i]), "\n")
    sendung <- readLines(con = paste(path,file[i], sep="/"), encoding = encoding)

    # get show parameter uuid, duration, name
    uuidline <- grep(pattern = "<uuid>", sendung)
    uuidShow <- trimws(gsub(pattern = "</?uuid>", replacement = "", sendung[uuidline], perl = TRUE))
    durationShow <- as.numeric(trimws(gsub(pattern = "</?duration>", replacement = "", sendung[uuidline+1], perl = TRUE)))
    nameShow <- trimws(gsub(pattern = "</?name>", replacement = "", sendung[uuidline+2], perl = TRUE))

    # helper for easy get-parts
    getPart <- function(from, pattern){
      tmp <- stringr::str_extract(from, paste0("<", pattern, ">(.*?)</", pattern, ">"))
      tmp <- gsub(pattern = paste0("</?", pattern, ">"), replacement = "", tmp, perl = TRUE)
      return(tmp)
    }

    # get text and some additional meta info per clip
    textlines <- grep(pattern = "</marker>", sendung)
    textlines <- cbind(c(1, textlines[-length(textlines)]), textlines)
    cliptext <- apply(textlines, 1, function(x) paste(sendung[x[1]:x[2]], collapse = " "))
    cliptext <-  stringr::str_extract(cliptext, "<marker>(.*?)</marker>")
    # get text itself
    text_new <- stringr::str_extract(cliptext, "<comment>(.*?)</comment>")
    text_new <- gsub(pattern = "</?comment>", replacement = "", text_new, perl = TRUE)

    # get some meta info per clip
    cliplines <- grep(pattern = "</clipitem>", sendung)
    cliplines <- cbind(c(1, cliplines[-length(cliplines)]), cliplines)
    clipinfo <- apply(cliplines, 1, function(x) paste(sendung[x[1]:x[2]], collapse = " "))
    if(!(length(clipinfo) / length(cliptext)) %in% c(1, 2)){
      warning("meta data could be false associated")
    }
    clipinfo <- clipinfo[seq_along(cliptext)]
    # get id, masterclipid, name, enabled, duration, in, out, start, end, rate, file
    clipinfo <-  stringr::str_extract(clipinfo, "<clipitem id=(.*?)</clipitem>")
    # get id
    id <- stringr::str_extract(clipinfo, "<clipitem id=(.*?)>")
    id <- gsub(pattern = "<clipitem id=\"", replacement = "", id)
    id <- gsub(pattern = "\">", replacement = "", id)
    # get name -> title
    title <- stringr::str_extract(clipinfo, "<name>(.*?)</name>")
    title <- gsub(pattern = "</?name>", replacement = "", title)
    # get date through title
    date <- stringr::str_extract(title, "[0-9]{2}[.][0-9]{2}[.][1-2][0-9]{3}")
    date <- as.Date(date, format = "%d.%m.%Y")
    # get file
    fileid <- stringr::str_extract(clipinfo, "<file id=(.*?)>")
    fileid <- gsub(pattern = "<file id=\"", replacement = "", fileid)
    fileid <- gsub(pattern = "\"/?>", replacement = "", fileid)

    newid <- paste(uuidShow, id, sep = ".")
    names(text_new) <- newid
    text <- as.list(c(text, text_new))

    mData <- data.frame(
      id = newid,
      date,
      title,
      #uuidShow,
      durationShow,
      #nameShow,
      #fileid,
      #clipid = id,
      #masterclipid = getPart(clipinfo, "masterclipid"),
      #enabled = as.logical(getPart(clipinfo, "enabled")),
      #duration = as.numeric(getPart(clipinfo, "duration")),
      #"in" = as.numeric(getPart(clipinfo, "in")),
      #out = as.numeric(getPart(clipinfo, "out")),
      start = as.numeric(getPart(clipinfo, "start")),
      end = as.numeric(getPart(clipinfo, "end")),
      rate = as.integer(getPart(getPart(clipinfo, "rate"), "timebase")),
      #nametext = getPart(cliptext, "name"),
      #intext = as.numeric(getPart(cliptext, "in")),
      #outtext = as.numeric(getPart(cliptext, "out")),
      stringsAsFactors = FALSE)
    meta <- rbind(meta, mData)
  }
  res <- list("meta" = meta, "text" = text, "metamult" = metamult)
  class(res) <- "textmeta"
  res <- deleteAndRenameDuplicates(res, paragraph = FALSE)
  summary(res)
}

readTagessschau.condensed <- function(path, file, encoding){
  text <- NULL
  meta <- NULL
  metamult <- NULL
  for (i in seq_along(file)) {
    cat(paste(file[i]), "\n")
    sendung <- readLines(con = paste(path,file[i], sep="/"), encoding = encoding)

    # get show parameter uuid, duration, name, rate
    uuidline <- grep(pattern = "<uuid>", sendung)
    uuidShow <- trimws(gsub(pattern = "</?uuid>", replacement = "", sendung[uuidline], perl = TRUE))
    durationShow <- as.numeric(trimws(gsub(pattern = "</?duration>", replacement = "", sendung[uuidline+1], perl = TRUE)))
    nameShow <- trimws(gsub(pattern = "</?name>", replacement = "", sendung[uuidline+2], perl = TRUE))
    rate <- as.numeric(trimws(gsub(pattern = "</?timebase>", replacement = "", sendung[uuidline+4], perl = TRUE)))

    # get date through nameShow
    date <- stringr::str_extract(nameShow, "[0-9]{2}[.][0-9]{2}[.][1-2][0-9]{3}")
    date <- as.Date(date, format = "%d.%m.%Y")

    # get text
    textlines <- grep(pattern = "</marker>", sendung)
    textlines <- cbind(c(1, textlines[-length(textlines)]), textlines)
    cliptext <- apply(textlines, 1, function(x) paste(sendung[x[1]:x[2]], collapse = " "))
    cliptext <-  stringr::str_extract(cliptext, "<marker>(.*?)</marker>")
    # get text itself
    text_new <- stringr::str_extract(cliptext, "<comment>(.*?)</comment>")
    text_new <- gsub(pattern = "</?comment>", replacement = "", text_new, perl = TRUE)
    text_new <- list(text_new)

    names(text_new) <- uuidShow
    text <- as.list(c(text, text_new))

    mData <- data.frame(
      id = uuidShow,
      date,
      title = nameShow,
      duration = durationShow,
      rate,
      stringsAsFactors = FALSE)
    meta <- rbind(meta, mData)
  }
  res <- list("meta" = meta, "text" = text, "metamult" = metamult)
  class(res) <- "textmeta"
  res <- deleteAndRenameDuplicates(res, paragraph = FALSE)
  summary(res)
}

