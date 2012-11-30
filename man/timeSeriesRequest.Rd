\name{timeSeriesRequest}
\alias{timeSeriesRequest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
timeSeriesRequest(dwei = getDataStream(), DSCodes = "", Instrument = "", startDate = Sys.Date(), endDate = Sys.Date(), frequency = "D", sStockList, aTimeSeries, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dwei}{
%%     ~~Describe \code{dwei} here~~
}
  \item{DSCodes}{
%%     ~~Describe \code{DSCodes} here~~
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
function (dwei = getDataStream(), DSCodes = "", Instrument = "", 
    startDate = Sys.Date(), endDate = Sys.Date(), frequency = "D", 
    sStockList, aTimeSeries, verbose = FALSE) 
{
    require(SSOAP, quietly = TRUE)
    require(XMLSchema, quietly = TRUE)
    require(XML, quietly = TRUE)
    require(zoo, quietly = TRUE)
    require(xts, quietly = TRUE)
    ud <- new("UserData", Username = paste("DS:", dwei$User, 
        sep = ""), Password = dwei$Pass)
    Instrument <- toupper(Instrument)
    if (verbose == TRUE) {
        cat(paste("Instrument is", Instrument, "\n"))
    }
    if (Instrument == "") {
        if (verbose == TRUE) {
            cat("Option: Instrument is blank", "\n")
        }
        instrumentCode <- lapply(DSCodes, function(x) paste(toupper(x), 
            "~", format(startDate, format = "\%Y-\%m-\%d"), "~:", 
            format(endDate, format = "\%Y-\%m-\%d"), "~", frequency, 
            "~AA~NA=NaN~#", dwei$User, sep = ""))
    }
    else {
        Instrument <- gsub("~", "~~", Instrument)
        if (grepl(pattern = "XXXX", x = Instrument, fixed = TRUE) == 
            FALSE) {
            if (verbose == TRUE) {
                cat("Option: instrument is datatypes", "\n")
            }
            instrumentCode <- lapply(DSCodes, function(x) paste(toupper(x), 
                "~=", Instrument, "~", format(startDate, format = "\%Y-\%m-\%d"), 
                "~:", format(endDate, format = "\%Y-\%m-\%d"), "~", 
                frequency, "~AA~NA=NaN~#", dwei$User, sep = ""))
        }
        else {
            if (verbose == TRUE) {
                cat("Option: instrument is an expression", "\n")
            }
            codes <- lapply(DSCodes, function(x) gsub(pattern = "XXXX", 
                replacement = x, x = Instrument, fixed = TRUE))
            instrumentCode <- lapply(codes, function(x) paste(toupper(x), 
                "~", format(startDate, format = "\%Y-\%m-\%d"), 
                "~:", format(endDate, format = "\%Y-\%m-\%d"), "~", 
                frequency, "~AA~NA=NaN~#", dwei$User, sep = ""))
        }
    }
    rd <- lapply(instrumentCode, function(x) new("RequestData", 
        Source = "Datastream", Instrument = x, Fields = as(c(""), 
            "ArrayOfString")))
    instrumentCodeMap <- list(code = as.character(DSCodes), instruments = as.character(instrumentCode))
    if (verbose == TRUE) {
        cat("Instrument code map\n")
    }
    if (verbose == TRUE) {
        cat(format(instrumentCodeMap))
    }
    if (verbose == TRUE) {
        cat("\n")
    }
    urrf <- new("User.Requests.RequestFlags", User = ud, Requests = new("ArrayOfRequestData", 
        rd), RequestFlags = 0L)
    if (verbose == TRUE) {
        cat("Make request\n")
    }
    dweObject <- dwei$dwe
    response <- dweObject@functions$RequestRecordsAsXml(parameters = urrf, 
        .convert = FALSE)
    if (verbose == TRUE) {
        cat("Get message content\n")
    }
    resultsXML <- response$content
    rm(response)
    gc()
    if (verbose == TRUE) {
        cat("Process content\n")
    }
    ourBranches <- function() {
        seriesNames <- new.env()
        tS <- new.env()
        tS[["count"]] <- 0
        Record <- function(x, ...) {
            status <- xmlValue(x[["StatusCode"]])
            if (verbose == TRUE) {
                cat("Status of response ", status)
            }
            instrument <- xmlValue(x[["Instrument"]])
            code <- getCodeFromInstrument(instrument, instrumentCodeMap)
            if (verbose == TRUE) {
                cat(" and processing ", instrument, " with code ", 
                  code, "\n")
            }
            if (status == 0) {
                value <- xmlValue(x[["Fields"]][["DISPNAME"]])
                seriesNames[[code]] <- value
                rm(instrument)
                rm(value)
                datesXML <- x[["Fields"]][["DATE"]]
                tmpdts <- as.Date(xmlSApply(datesXML, getNodesValue))
                q <- match(names(xmlChildren(x[["Fields"]])), 
                  c("CCY", "DATE", "DISPNAME", "FREQUENCY", "SYMBOL"))
                valNode <- max(is.na(q) * seq(along = q))
                rm(q)
                pricesXML <- x[["Fields"]][[valNode]]
                rm(valNode)
                tmpval <- as.numeric(xmlSApply(pricesXML, function(node) xmlSApply(node, 
                  xmlValue)))
                t <- xts(tmpval, tmpdts)
                names(t) <- code
                if (verbose == TRUE) {
                  cat("Extracted timeseries\n")
                }
                if (verbose == TRUE) {
                  cat(paste0("Class ", class(t), "\n"))
                }
                if (verbose == TRUE) {
                  print(head(t))
                }
                if (verbose == TRUE) {
                  cat("\n")
                }
                rm(tmpval)
                rm(tmpdts)
                rm(pricesXML)
                rm(datesXML)
            }
            else {
                seriesNames[[code]] <- code
                rm(instrument)
                if (verbose == TRUE) {
                  cat("No data returned for ", code, "\n")
                }
                rm(code)
                t <- xts(NA, startDate)
                names(t) <- code
            }
            stockCount <- tS[["count"]]
            stockCount <- stockCount + 1
            z <- tS[["ts"]]
            z[[stockCount]] <- t
            tS[["ts"]] <- z
            tS[["count"]] <- stockCount
            rm(code, t, z)
            rm(stockCount)
            if (verbose == TRUE) {
                cat(" size ", object.size(tS[["ts"]]), "\n")
            }
            if (verbose == TRUE) {
                cat("Memory size", memory.size(), "\n")
            }
            gc()
        }
        getSeriesNames <- function() as.list(seriesNames)
        getTimeSeries <- function() {
            if (verbose == TRUE) {
                cat("Getting timeseries array...")
            }
            m <- do.call(merge, tS[["ts"]])
            if (verbose == TRUE) {
                cat("...done\n")
            }
            return(m)
        }
        getCount <- function() as.integer(tS[["count"]])
        getTS <- function() return(tS[["ts"]])
        free <- function() {
            rm(list = ls(name = tS), envir = tS)
            rm(list = ls(name = seriesNames), envir = seriesNames)
        }
        list(Record = Record, getSeriesNames = getSeriesNames, 
            getTimeSeries = getTimeSeries, getCount = getCount, 
            getTS = getTS, free = free)
    }
    branches <- ourBranches()
    if (verbose == TRUE) {
        cat("Parsing response\n")
    }
    doc <- invisible(xmlEventParse(resultsXML, handlers = list(), 
        branches = branches, useTagName = FALSE, addContext = FALSE, 
        asText = TRUE))
    if (verbose == TRUE) {
        cat("Returning timeseries\n")
    }
    eval.parent(substitute(sStockList <- branches$getSeriesNames()))
    eval.parent(substitute(aTimeSeries <- branches$getTimeSeries()))
    tS <- branches$getTS()
    if (verbose == TRUE) {
        cat("Releasing objects\n")
    }
    branches$free()
    rm("branches")
    rm(resultsXML)
    gc()
    return(instrumentCodeMap)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
