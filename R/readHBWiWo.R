#' Read the HB WiWo Corpus
#'
#' Reads the XML-files from the HB WiWo corpus and seperates the text and meta
#' data.
#'
#' @param path Path where the data files are.
#' @param file Names of the XML files.
#' @param do.meta Logical: Should the algorithm collect meta data?
#' @param do.text Logical: Should the algorithm collect text data?
#' @return \item{meta}{ id quelle datum titel abstract UB dachzeile}
#' \item{text}{ Text} \item{metamult}{ person firma industrie land autor rubrik
#' klassifikation (mehrere moeglich) thema sachgruppe serie}
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#' @examples
#'
#' @export readHBWiWo
#'
readHBWiWo <- function(path = getwd(), file = list.files(path=path, pattern="*.xml", full.names=F, recursive=TRUE), do.meta = TRUE, do.text = TRUE){
  stopifnot(is.character(file),
            is.logical(do.meta), is.logical(do.text),
            length(do.meta) == 1, length(do.text) == 1)
  text <- NULL
  meta <- NULL
  metamult <- NULL
  for(i in 1:length(file)){
    (print(file[i]))
    article <- readLines(con = paste(path,file[i], sep="/"), encoding = "latin1")
    article <- gsub(pattern = "&quot;", replacement = "\"",article)
    article <- gsub(pattern = "&amp;", replacement = "&",article)
    article <- gsub(pattern = "&apos;", replacement = "\'",article)
    lines <- grep(pattern = "</Dokument>|</Document>", article)
    lines <- cbind(c(1,lines[-length(lines)]),lines)
    article <- apply(lines, 1, function(x)paste(article[x[1]:x[2]], collapse = " "))
    id <- stringr::str_extract(article, "ID=\"(.*?)\"")
    id <- gsub(pattern="ID=|\"", replacement="", x=id)

    if(do.meta){
        source <- stringr::str_extract(article, "<Source>(.*?)</Source>|<Quelle>(.*?)</Quelle>")
        source <- removeTAG(source)
        date <- stringr::str_extract(article, "<Date>(.*?)</Date>|<Datum>(.*?)</Datum>")
        date <- as.Date(removeTAG(date))
        title <- stringr::str_extract(article, "<Title>(.*?)</Title>|<Titel>(.*?)</Titel>")
        title <- removeTAG(title)
        abstract <- stringr::str_extract(article, "<Abstract>(.*?)</Abstract>")
        abstract <- removeTAG(abstract)
        ub <- stringr::str_extract(article, "<UB>(.*?)</UB>")
        ub <- removeTAG(ub)
        dachzeile <- stringr::str_extract(article, "<DZ>(.*?)</DZ>")
        dachzeile <- removeTAG(dachzeile)

        company <- stringr::str_extract_all(article, "<Company>(.*?)</Company>|<Firma>(.*?)</Firma>")
        names(company) <- id
        tmp <- rep(names(company), lengths(company))
        company <- unlist(company)
        names(company) <- tmp
        company <- trimws(gsub(pattern="<Company>|</Company>|<Firma>|</Firma>", replacement="", x=company))

        country <- stringr::str_extract_all(article, "<Country>(.*?)</Country>|<Land>(.*?)</Land>")
        names(country) <- id
        tmp <- rep(names(country), lengths(country))
        country <- unlist(country)
        names(country) <- tmp
        country <- trimws(gsub(pattern="<Country>|</Country>|<Land>|</Land>", replacement="", x=country))

        industry <- stringr::str_extract_all(article, "<Industry>(.*?)</Industry>|<Industrie>(.*?)</Industrie>")
        names(industry) <- id
        tmp <- rep(names(industry), lengths(industry))
        industry <- unlist(industry)
        names(industry) <- tmp
        industry <- trimws(gsub(pattern="<Industry>|</Industry>|<Industrie>|</Industrie>", replacement="", x=industry))

        author <- stringr::str_extract_all(article, "<Author>(.*?)</Author>|<Autor>(.*?)</Autor>")
        names(author) <- id
        tmp <- rep(names(author), lengths(author))
        author <- unlist(author)
        names(author) <- tmp
        author <- trimws(gsub(pattern="<Author>|</Author>|<Autor>|</Autor>", replacement="", x=author))

        category <- stringr::str_extract_all(article, "<Category>(.*?)</Category>|<Rubrik>(.*?)</Rubrik>")
        names(category) <- id
        tmp <- rep(names(category), lengths(category))
        category <- unlist(category)
        names(category) <- tmp
        category <- trimws(gsub(pattern="<Category>|</Category>|<Rubrik>|</Rubrik>", replacement="", x=category))

        klassifikation <- stringr::str_extract_all(article, "<Klassifikation>(.*?)</Klassifikation>")
        names(klassifikation) <- id
        tmp <- rep(names(klassifikation), lengths(klassifikation))
        klassifikation <- unlist(klassifikation)
        names(klassifikation) <- tmp
        klassifikation <- trimws(gsub(pattern="<Klassifikation>|</Klassifikation>", replacement="", x=klassifikation))

        thema <- stringr::str_extract_all(article, "<Thema>(.*?)</Thema>")
        names(thema) <- id
        tmp <- rep(names(thema), lengths(thema))
        thema <- unlist(thema)
        names(thema) <- tmp
        thema <- trimws(gsub(pattern="<Thema>|</Thema>", replacement="", x=thema))

        sachgruppe <- stringr::str_extract_all(article, "<Sachgruppe>(.*?)</Sachgruppe>")
        names(sachgruppe) <- id
        tmp <- rep(names(sachgruppe), lengths(sachgruppe))
        sachgruppe <- unlist(sachgruppe)
        names(sachgruppe) <- tmp
        sachgruppe <- trimws(gsub(pattern="<Sachgruppe>|</Sachgruppe>", replacement="", x=sachgruppe))

        serie <- stringr::str_extract_all(article, "<Serie>(.*?)</Serie>")
        names(serie) <- id
        tmp <- rep(names(serie), lengths(serie))
        serie <- unlist(serie)
        names(serie) <- tmp
        serie <- trimws(gsub(pattern="<Serie>|</Serie>", replacement="", x=serie))

        person <- stringr::str_extract_all(article, "<Person>(.*?)</Person>")
        names(person) <- id
        tmp <- rep(names(person), lengths(person))
        person <- unlist(person)
        names(person) <- tmp
        person <- trimws(gsub(pattern="<Person>|</Person>", replacement="", x=person))

        mData <- data.frame(id, source, date, title, abstract, ub, dachzeile,
                            stringsAsFactors = FALSE)
        meta <- rbind(meta, mData)
        metamult$person <- c(metamult$person, person)
        metamult$company <- c(metamult$company, company)
        metamult$industry <- c(metamult$industry, industry)
        metamult$country <- c(metamult$country, country)
        metamult$author <- c(metamult$author, author)
        metamult$category <- c(metamult$category, category)
        metamult$klassifikation <- c(metamult$klassifikation, klassifikation)
        metamult$thema <- c(metamult$thema, thema)
        metamult$sachgruppe <- c(metamult$sachgruppe, sachgruppe)
        metamult$serie <- c(metamult$serie, serie)
    }
    if(do.text){
        text_new <- stringr::str_extract(article, "<Text>(.*?)</Text>")
        text_new <- trimws(gsub(pattern="<Text>|</Text>", replacement="", x=text_new))
        names(text_new) <- id
        text <- as.list(c(text, text_new))
    }
}
#  text[is.na(text)] <- meta$abstract[is.na(text)]
  res <- list("meta" = meta, "text" = text, "metamult" = metamult)
  class(res) <- "textmeta"
  summary(res)
}
