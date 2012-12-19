\name{timeSeriesListRequest}
\alias{timeSeriesListRequest}
\title{\code{timeSeriesListRequest} Function that returns a timeseries from Datastream constituent list
parameters are}
\usage{
  timeSeriesListRequest(dwei = getDataStream(), DSCode,
    Instrument, startDate, endDate = Sys.Date(),
    frequency = "D", sStockList, aTimeSeries,
    verbose = FALSE)
}
\arguments{
  \item{dwei}{- A Datastream Client Interface object
  created with getDataStream}

  \item{DSCode}{- the constituent list requested eg
  'LFTSE100'}

  \item{Instrument}{- the expression to return for each
  member of constituent list}

  \item{startDate}{- the start date of the timeseries}

  \item{endDate}{- the end date of the timeseries}

  \item{frequency}{- the frequency of the request}

  \item{verbose}{- whether to give messages during the
  request}
}
\value{
  whether the request has been successful , but also in
  sStockList: a list a two element vector of the
  displayname and symbol for each timeseries in
  aTimeseries: a list of class xts with the requested
  timeseries information
}
\description{
  \code{timeSeriesListRequest} Function that returns a
  timeseries from Datastream constituent list parameters
  are
}

