#' Read the SPIEGEL Corpus
#'
#' Reads the XML-files from the SPIEGEL corpus and seperates the text and meta
#' data.
#'
#'
#' @param year Vector of years that should be read.
#' @param do.meta Logical: Should the algorithm collect meta data?
#' @param do.text Logical: Should the algorithm collect text data?
#' @return \item{meta}{ id jahr nummer datum seiteS seiteE seitentitel
#' kurztitel rubrik ressort dokumentenmerkmal dachzeile titel vorspann}
#' \item{text}{ Text (Paragraphenweise)} \item{metamult}{ person koerperschaft
#' firma inkl. Kategorie(n)}
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords manip
#' @examples
#' ##---- Should be DIRECTLY executable !! ----
#' @export readSPIEGEL
readSPIEGEL <-
function(year=c(1947:2014) , do.meta=TRUE, do.text=TRUE){
text <- NULL
meta <- NULL
metamult <- NULL
for(i in year){
(print(i))
openfile <- file(paste("Spiegel-Daten/",i,".xml", sep=""), open = "rt")
lines <- readLines(con = openfile)
close(openfile)
artikel <- strsplit(paste(lines, collapse=" "), "<artikel>")[[1]][-1]
id <- strsplit(artikel, "<artikel-id>")
id <- sapply(id,function(x){x[2]})
id <- strsplit(id, "</artikel-id>")
id <- sapply(id,function(x){x[1]})
if(do.meta){
jahr <- strsplit(artikel, "<jahrgang>")
jahr <- sapply(jahr,function(x){x[2]})
jahr <- strsplit(jahr, "</jahrgang>")
jahr <- as.numeric(sapply(jahr,function(x){x[1]}))
nummer <- strsplit(artikel, "<nummer>")
nummer <- sapply(nummer,function(x){x[2]})
nummer <- strsplit(nummer, "</nummer>")
nummer <- as.numeric(sapply(nummer,function(x){x[1]}))
datum <- strsplit(artikel, "<datum>")
datum <- sapply(datum,function(x){x[2]})
datum <- strsplit(datum, "</datum>")
datum <- as.character(sapply(datum,function(x){x[1]}))
datum <- as.Date(datum, format = "%d%m%Y")
seiteS <- strsplit(artikel, "<seite-start>")
seiteS <- sapply(seiteS,function(x){x[2]})
seiteS <- strsplit(seiteS, "</seite-start>")
seiteS <- as.numeric(sapply(seiteS,function(x){x[1]}))
seiteE <- strsplit(artikel, "<seite-ende>")
seiteE <- sapply(seiteE,function(x){x[2]})
seiteE <- strsplit(seiteE, "</seite-ende>")
seiteE <- as.numeric(sapply(seiteE,function(x){x[1]}))
seitentitel <- strsplit(artikel, "<seitentitel>")
seitentitel <- sapply(seitentitel,function(x){x[2]})
seitentitel <- strsplit(seitentitel, "</seitentitel>")
seitentitel <- sapply(seitentitel,function(x){x[1]})
kurztitel <- strsplit(artikel, "<kurztitel>")
kurztitel <- sapply(kurztitel,function(x){x[2]})
kurztitel <- strsplit(kurztitel, "</kurztitel>")
kurztitel <- sapply(kurztitel,function(x){x[1]})
rubrik <- strsplit(artikel, "<rubrik>")
rubrik <- sapply(rubrik,function(x){x[2]})
rubrik <- strsplit(rubrik, "</rubrik>")
rubrik <- sapply(rubrik,function(x){x[1]})
ressort <- strsplit(artikel, "<ressort>")
ressort <- sapply(ressort,function(x){x[2]})
ressort <- strsplit(ressort, "</ressort>")
ressort <- sapply(ressort,function(x){x[1]})
dokumentmerkmal <- strsplit(artikel, "<dokumentmerkmal>")
dokumentmerkmal <- sapply(dokumentmerkmal,function(x){x[2]})
dokumentmerkmal <- strsplit(dokumentmerkmal, "</dokumentmerkmal>")
dokumentmerkmal <- sapply(dokumentmerkmal,function(x){x[1]})
dachzeile <- strsplit(artikel, "<dachzeile>")
dachzeile <- sapply(dachzeile,function(x){x[2]})
dachzeile <- strsplit(dachzeile, "</dachzeile>")
dachzeile <- sapply(dachzeile,function(x){x[1]})
titel <- strsplit(artikel, "<titel>")
titel <- sapply(titel,function(x){x[2]})
titel <- strsplit(titel, "</titel>")
titel <- sapply(titel,function(x){x[1]})
vorspann <- strsplit(artikel, "<vorspann>")
vorspann <- sapply(vorspann,function(x){x[2]})
vorspann <- strsplit(vorspann, "</vorspann>")
vorspann <- sapply(vorspann,function(x){x[1]})
signatur <- strsplit(artikel, "<signatur>  ")
signatur <- sapply(signatur,function(x){x[-1]})
signatur <- sapply(signatur,function(x)unlist(strsplit(x, "</signatur>")))
signatur <- sapply(signatur,function(x){x[-seq(0,length(x),by=2)]})
names(signatur) <- id
person <- strsplit(artikel, "<person>  ")
person <- sapply(person,function(x){x[-1]})
person <- sapply(person,function(x)unlist(strsplit(x, "</person>")))
person <- sapply(person,function(x){x[-seq(0,length(x),by=2)]})
names(person) <- id
koerperschaft <- strsplit(artikel, "<koerperschaft>  ")
koerperschaft <- sapply(koerperschaft,function(x){x[-1]})
koerperschaft <- sapply(koerperschaft,function(x)unlist(strsplit(x, "</koerperschaft>")))
koerperschaft <- sapply(koerperschaft,function(x){x[-seq(0,length(x),by=2)]})
names(koerperschaft) <- id
firma <- strsplit(artikel, "<firma>  ")
firma <- sapply(firma,function(x){x[-1]})
firma <- sapply(firma,function(x)unlist(strsplit(x, "</firma>")))
firma <- sapply(firma,function(x){x[-seq(0,length(x),by=2)]})
names(firma) <- id

mData <- data.frame(id,jahr,nummer,datum,seiteS,seiteE,seitentitel,kurztitel,rubrik,ressort, dokumentmerkmal,dachzeile,titel,vorspann, stringsAsFactors = FALSE)
meta <- rbind(meta,mData)

metamult$signatur <- c(metamult$signatur,signatur)
metamult$person <- c(metamult$person,person)
metamult$koerperschaft <- c(metamult$koerperschaft,koerperschaft)
metamult$firma <- c(metamult$firma,firma)
}
if(do.text){
cData <- list(NULL)
for(j in 1:length(artikel)){
absatz <- strsplit(artikel[j], "<absatz>")[[1]][-1]
if(length(absatz)==0){
cData[[j]] <- NA
next}
cData[[j]] <- list(NULL)
absatz2 <- strsplit(absatz, "</absatz>")
cData[[j]] <- sapply(absatz2,function(x){x[1]})
}
names(cData) <- id
text <- c(text,cData)
}}
return(list("meta"=meta,"text"=text, "metamult"=metamult))}
