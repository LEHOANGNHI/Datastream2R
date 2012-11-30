\name{timeSeriesRequest}
\alias{timeSeriesRequest}
\title{\code{timeSeriesRequest} Function that returns a timeseries from Datastream
parameters are}
\usage{
  timeSeriesRequest(dwei = getDataStream(), DSCodes = "",
    Instrument = "", startDate = Sys.Date(),
    endDate = Sys.Date(), frequency = "D", sStockList,
    aTimeSeries, verbose = FALSE)
}
\arguments{
  \item{dwei}{- A Datastream Client Interface object
  created with getDataStream}

  \item{DSCodes}{- one or more codes to return, eg "MKS" or
  c("MKS","SAB")}

  \item{Instrument}{- the instrument or expression to
  return eg PCH#(XXXX,1M)}

  \item{startDate}{- the start date of the timeseries}

  \item{endDate}{- the end date of the timeseries}

  \item{frequency}{- the frequency of the request}

  \item{verbose}{- whether to give messages during the
  request}
}
\value{
  whether the request has been successful in sStockList: a
  list a two element vector of the displayname and symbol
  for each timeseries in aTimeseries: a list of class xts
  with the requested timeseries information
}
\description{
  \code{timeSeriesRequest} Function that returns a
  timeseries from Datastream parameters are
}

