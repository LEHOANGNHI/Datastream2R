\name{timeSeriesListRequest}
\alias{timeSeriesListRequest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
timeSeriesListRequest(dwei = getDataStream(), DSCode, Instrument, startDate, endDate = Sys.Date(), frequency = "D", sStockList, aTimeSeries, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dwei}{
%%     ~~Describe \code{dwei} here~~
}
  \item{DSCode}{
%%     ~~Describe \code{DSCode} here~~
}
  \item{Instrument}{
%%     ~~Describe \code{Instrument} here~~
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
  \item{sStockList}{
%%     ~~Describe \code{sStockList} here~~
}
  \item{aTimeSeries}{
%%     ~~Describe \code{aTimeSeries} here~~
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
function (dwei = getDataStream(), DSCode, Instrument, startDate, 
    endDate = Sys.Date(), frequency = "D", sStockList, aTimeSeries, 
    verbose = FALSE) 
{
    constituents <- listRequest(dwei = dwei, DSCode = DSCode, 
        Expression = "MNEM", startDate = startDate, verbose = verbose)
    ret <- timeSeriesRequest(dwei = dwei, DSCodes = constituents, 
        Instrument = Instrument, startDate = startDate, endDate = endDate, 
        frequency = frequency, sStockList = sST, aTimeSeries = aTS, 
        verbose = verbose)
    eval.parent(substitute(sStockList <- sST))
    eval.parent(substitute(aTimeSeries <- aTS))
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
