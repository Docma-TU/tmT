#' Read the HB WiWo Corpus
#'
#' Reads the XML-files from the HB WiWo corpus and seperates the text and meta
#' data.
#'
#'
#' @param file Names of the XML files.
#' @param en Logical: Are there english descriptions in the XML structure? Must
#' have same length like \code{file}.
#' @param do.meta Logical: Should the algorithm collect meta data?
#' @param do.text Logical: Should the algorithm collect text data?
#' @param test logical. other directory
#' @return \item{meta}{ id quelle datum titel abstract UB dachzeile}
#' \item{text}{ Text} \item{metamult}{ person firma industrie land autor rubrik
#' klassifikation (mehrere moeglich) thema sachgruppe serie}
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#' @examples
#'
#' @export readHBWiWo
#'
readHBWiWo <-
function(file , en, do.meta=TRUE, do.text=TRUE, test=FALSE){
text <- NULL
meta <- NULL
metamult <- NULL
for(i in 1:length(file)){
(print(file[i]))
if(test){openfile <- file(file[i], open = "rt")}else{
openfile <- file(paste("Daten/",file[i], sep=""), open = "rt")}
lines <- readLines(con = openfile)
close(openfile)

lines <- gsub(pattern="&quot;", replacement="\"",lines)
lines <- gsub(pattern="&amp;", replacement="&",lines)
lines <- gsub(pattern="&apos;", replacement="\'",lines)

if(en[i]){artikel <- strsplit(paste(lines, collapse=" "), "</Document>")[[1]]}else{
          artikel <- strsplit(paste(lines, collapse=" "), "</Dokument>")[[1]]}
artikel <- artikel[-length(artikel)]

id <- strsplit(artikel, "ID=\"")
id <- sapply(id,function(x){x[2]})
id <- strsplit(id, "\"")
id <- sapply(id,function(x){x[1]})
if(do.meta){

if(en[i]){
quelle <- strsplit(artikel, "<Source>")
quelle <- sapply(quelle,function(x){x[2]})
quelle <- strsplit(quelle, "</Source>")
quelle <- sapply(quelle,function(x){x[1]})
datum <- strsplit(artikel, "<Date>")
datum <- sapply(datum,function(x){x[2]})
datum <- strsplit(datum, "</Date>")
datum <- as.character(sapply(datum,function(x){x[1]}))
datum <- as.Date(datum, format = "%Y-%m-%d")
titel <- strsplit(artikel, "<Title>")
titel <- sapply(titel,function(x){x[2]})
titel <- strsplit(titel, "</Title>")
titel <- sapply(titel,function(x){x[1]})

firma <- strsplit(artikel, "<Company>")
firma <- sapply(firma,function(x){x[-1]})
firma <- sapply(firma,function(x)unlist(strsplit(x, "</Company>")))
firma <- sapply(firma,function(x){x[-seq(0,length(x),by=2)]})
names(firma) <- id
land <- strsplit(artikel, "<Country>")
land <- sapply(land,function(x){x[-1]})
land <- sapply(land,function(x)unlist(strsplit(x, "</Country>")))
land <- sapply(land,function(x){x[-seq(0,length(x),by=2)]})
names(land) <- id
industrie <- strsplit(artikel, "<Industry>")
industrie <- sapply(industrie,function(x){x[-1]})
industrie <- sapply(industrie,function(x)unlist(strsplit(x, "</Industry>")))
industrie <- sapply(industrie,function(x){x[-seq(0,length(x),by=2)]})
names(industrie) <- id
autor <- strsplit(artikel, "<Author> ")
autor <- sapply(autor,function(x){x[-1]})
autor <- sapply(autor,function(x)unlist(strsplit(x, " </Author>")))
autor <- sapply(autor,function(x){x[-seq(0,length(x),by=2)]})
names(autor) <- id
rubrik <- strsplit(artikel, "<Category> ")
rubrik <- sapply(rubrik,function(x){x[-1]})
rubrik <- sapply(rubrik,function(x)unlist(strsplit(x, " </Category>")))
rubrik <- sapply(rubrik,function(x){x[-seq(0,length(x),by=2)]})
names(rubrik) <- id
}else{
quelle <- strsplit(artikel, "<Quelle>")
quelle <- sapply(quelle,function(x){x[2]})
quelle <- strsplit(quelle, "</Quelle>")
quelle <- as.numeric(sapply(quelle,function(x){x[1]}))
datum <- strsplit(artikel, "<Datum>")
datum <- sapply(datum,function(x){x[2]})
datum <- strsplit(datum, "</Datum>")
datum <- as.character(sapply(datum,function(x){x[1]}))
datum <- as.Date(datum, format = "%Y-%m-%d")
titel <- strsplit(artikel, "<Titel>")
titel <- sapply(titel,function(x){x[2]})
titel <- strsplit(titel, "</Titel>")
titel <- sapply(titel,function(x){x[1]})
firma <- strsplit(artikel, "<Firma>")
firma <- sapply(firma,function(x){x[-1]})
firma <- sapply(firma,function(x)unlist(strsplit(x, "</Firma>")))
firma <- sapply(firma,function(x){x[-seq(0,length(x),by=2)]})
names(firma) <- id
land <- strsplit(artikel, "<Land>")
land <- sapply(land,function(x){x[-1]})
land <- sapply(land,function(x)unlist(strsplit(x, "</Land>")))
land <- sapply(land,function(x){x[-seq(0,length(x),by=2)]})
names(land) <- id
industrie <- strsplit(artikel, "<Industrie>")
industrie <- sapply(industrie,function(x){x[-1]})
industrie <- sapply(industrie,function(x)unlist(strsplit(x, "</Industrie>")))
industrie <- sapply(industrie,function(x){x[-seq(0,length(x),by=2)]})
names(industrie) <- id
autor <- strsplit(artikel, "<Autor> ")
autor <- sapply(autor,function(x){x[-1]})
autor <- sapply(autor,function(x)unlist(strsplit(x, " </Autor>")))
autor <- sapply(autor,function(x){x[-seq(0,length(x),by=2)]})
names(autor) <- id
rubrik <- strsplit(artikel, "<Rubrik> ")
rubrik <- sapply(rubrik,function(x){x[-1]})
rubrik <- sapply(rubrik,function(x)unlist(strsplit(x, " </Rubrik>")))
rubrik <- sapply(rubrik,function(x){x[-seq(0,length(x),by=2)]})
names(rubrik) <- id
}
abstract <- strsplit(artikel, "<Abstract>")
abstract <- sapply(abstract,function(x){x[2]})
abstract <- strsplit(abstract, "</Abstract>")
abstract <- sapply(abstract,function(x){x[1]})
UB <- strsplit(artikel, "<UB>")
UB <- sapply(UB,function(x){x[2]})
UB <- strsplit(UB, "</UB>")
UB <- sapply(UB,function(x){x[1]})
dachzeile <- strsplit(artikel, "<DZ>")
dachzeile <- sapply(dachzeile,function(x){x[2]})
dachzeile <- strsplit(dachzeile, "</DZ>")
dachzeile <- sapply(dachzeile,function(x){x[1]})
klassifikation <- strsplit(artikel, "<Klassifikation> ")
klassifikation <- sapply(klassifikation,function(x){x[-1]})
klassifikation <- sapply(klassifikation,function(x)unlist(strsplit(x, " </Klassifikation>")))
klassifikation <- sapply(klassifikation,function(x){x[-seq(0,length(x),by=2)]})
names(klassifikation) <- id
thema <- strsplit(artikel, "<Thema> ")
thema <- sapply(thema,function(x){x[-1]})
thema <- sapply(thema,function(x)unlist(strsplit(x, " </Thema>")))
thema <- sapply(thema,function(x){x[-seq(0,length(x),by=2)]})
names(thema) <- id
sachgruppe <- strsplit(artikel, "<Sachgruppe> ")
sachgruppe <- sapply(sachgruppe,function(x){x[-1]})
sachgruppe <- sapply(sachgruppe,function(x)unlist(strsplit(x, " </Sachgruppe>")))
sachgruppe <- sapply(sachgruppe,function(x){x[-seq(0,length(x),by=2)]})
names(sachgruppe) <- id
serie <- strsplit(artikel, "<Serie> ")
serie <- sapply(serie,function(x){x[-1]})
serie <- sapply(serie,function(x)unlist(strsplit(x, " </Serie>")))
serie <- sapply(serie,function(x){x[-seq(0,length(x),by=2)]})
names(serie) <- id
person <- strsplit(artikel, "<Person>")
person <- sapply(person,function(x){x[-1]})
person <- sapply(person,function(x)unlist(strsplit(x, "</Person>")))
person <- sapply(person,function(x){x[-seq(0,length(x),by=2)]})
names(person) <- id
mData <- data.frame(id,quelle,datum,titel,abstract,UB,dachzeile, stringsAsFactors = FALSE)
meta <- rbind(meta,mData)
metamult$person <- c(metamult$person,person)
metamult$firma <- c(metamult$firma,firma)
metamult$industrie <- c(metamult$industrie,industrie)
metamult$land <- c(metamult$land,land)
metamult$autor <- c(metamult$autor,autor)
metamult$rubrik <- c(metamult$rubrik,rubrik)
metamult$klassifikation <- c(metamult$klassifikation,klassifikation)
metamult$thema <- c(metamult$thema,thema)
metamult$sachgruppe <- c(metamult$sachgruppe,sachgruppe)
metamult$serie <- c(metamult$serie,serie)
}
if(do.text){
text_neu <- strsplit(artikel, "<Text>")
text_neu <- sapply(text_neu,function(x){x[2]})
text_neu <- strsplit(text_neu, "</Text>")
text_neu <- lapply(text_neu,function(x){x[1]})
names(text_neu) <- id
text <- c(text,text_neu)
}}
return(list("meta"=meta,"text"=text,"metamult"=metamult))}
