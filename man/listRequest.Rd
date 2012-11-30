\name{listRequest}
\alias{listRequest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
listRequest(dwei = getDataStream(), DSCode, Expression = "", startDate, endDate = Sys.Date(), frequency = "D", verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dwei}{
%%     ~~Describe \code{dwei} here~~
}
  \item{DSCode}{
%%     ~~Describe \code{DSCode} here~~
}
  \item{Expression}{
%%     ~~Describe \code{Expression} here~~
}
  \item{startDate}{
%%     ~~Describe \code{startDate} here~~
}
  \item{endDate}{
%%     ~~Describe \code{endDate} here~~
}
  \item{frequency}{
%%     ~~Describe \code{frequency} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dwei = getDataStream(), DSCode, Expression = "", startDate, 
    endDate = Sys.Date(), frequency = "D", verbose = FALSE) 
{
    DSCode <- gsub("~", "~~", DSCode)
    instrumentCode <- paste(toupper(DSCode), "~LIST~=", toupper(Expression), 
        "~", format(startDate, format = "\%Y-\%m-\%d"), "~AA~NA=NaN~#", 
        dwei$User, sep = "")
    ud <- new("UserData", Username = paste("DS:", dwei$User, 
        sep = ""), Password = dwei$Pass)
    rd <- new("RequestData", Source = "Datastream", Instrument = instrumentCode, 
        Fields = as(c(""), "ArrayOfString"))
    urrf <- new("User.Request.RequestFlags", User = ud, Request = rd, 
        RequestFlags = 0L)
    dweObject <- dwei$dwe
    resultsXML <- dweObject@functions$RequestRecordAsXml(parameters = urrf, 
        .convert = FALSE)
    docXML = xmlRoot(xmlTreeParse(resultsXML$content))
    fieldsXML <- docXML[["Body"]][["RequestRecordAsXmlResponse"]][["RequestRecordAsXmlResult"]][["Record"]][["Fields"]]
    valuesXML <- fieldsXML[[Expression]]
    values <- xmlSApply(valuesXML, function(node) xmlSApply(node, 
        xmlValue))
    return(values)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
