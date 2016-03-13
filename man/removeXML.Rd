\encoding{latin1}
\name{removeXML}
\alias{removeXML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Removes XML tags and umlauts
}
\description{
Removes XML tags and changes umlauts to a standardized form.

}
\usage{
removeXML(x, umlaute = FALSE, u.type = c("normal", "html", "all"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
List of texts.
}
  \item{umlaute}{
Logical: Should umlauts be changed?
}
  \item{u.type}{
Type of umlaut changing: \code{normal} for normal umlauts (ü -> ue), \code{html}
for html representation of umlauts (&uuml; -> ue) and \code{all} for both.
}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
\value{
Adjusted corpus
}
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{
Lars Koppers (<koppers@statistik.tu-dortmund.de>)
}
%% \note{
%% %%  ~~further notes~~
%% }

%% %% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
removeXML <- function(x,umlaute=FALSE, u.type=c("normal", "html", "all")){
(print("<[^>]*>"))
x <- lapply(x,function(x){gsub(pattern="<[^>]*>", replacement=" ",x, perl=TRUE)})
if(umlaute){
if(u.type[1]=="html" || u.type[1]=="all"){
(print("\U00E4"))
x <- lapply(x,function(x){gsub(pattern="&auml;", replacement="ae",x)})
(print("\U00C4"))
x <- lapply(x,function(x){gsub(pattern="&Auml;", replacement="Ae",x)})
(print("\U00F6"))
x <- lapply(x,function(x){gsub(pattern="&ouml;", replacement="oe",x)})
(print("\U00D6"))
x <- lapply(x,function(x){gsub(pattern="&Ouml;", replacement="Oe",x)})
(print("\U00FC"))
x <- lapply(x,function(x){gsub(pattern="&uuml;", replacement="ue",x)})
(print("\U00DC"))
x <- lapply(x,function(x){gsub(pattern="&Uuml;", replacement="Ue",x)})
(print("\U00DF"))
x <- lapply(x,function(x){gsub(pattern="&szlig;", replacement="ss",x)})
(print("&;"))
x <- lapply(x,function(x){gsub(pattern="&[^;]*;", replacement="",x)})}
if(u.type=="normal" || u.type[1]=="all"){
(print("\U00E4"))
x <- lapply(x,function(x){gsub(pattern="\U00E4", replacement="ae",x)})
(print("\U00C4"))
x <- lapply(x,function(x){gsub(pattern="\U00C4", replacement="Ae",x)})
(print("\U00F6"))
x <- lapply(x,function(x){gsub(pattern="\U00F6", replacement="oe",x)})
(print("\U00D6"))
x <- lapply(x,function(x){gsub(pattern="\U00D6", replacement="Oe",x)})
(print("\U00FC"))
x <- lapply(x,function(x){gsub(pattern="\U00FC", replacement="ue",x)})
(print("\U00DC"))
x <- lapply(x,function(x){gsub(pattern="\U00DC", replacement="Ue",x)})
(print("\U00DF"))
x <- lapply(x,function(x){gsub(pattern="\U00DF", replacement="ss",x)})
}}
return(x)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

