\name{listRequest}
\alias{listRequest}
\title{\code{listRequest} Function that returns a the value of Expression for the instrument list in DSCode from Datastream
parameters are}
\usage{
  listRequest(dwei = getDataStream(), DSCode,
    Expression = "", startDate, endDate = Sys.Date(),
    frequency = "D", verbose = FALSE)
}
\arguments{
  \item{dwei}{- A Datastream Client Interface object
  created with getDataStream}

  \item{DSCode}{- the constituent list for the request eg
  LDJSTOXX}

  \item{Expression}{- the data to return eg MNEM or NAME}

  \item{startDate}{- the start date of the timeseries}

  \item{endDate}{- the end date of the timeseries}

  \item{frequency}{- the frequency of the request}

  \item{verbose}{- whether to give messages during the
  request}
}
\value{
  returns an array of the requested information
}
\description{
  \code{listRequest} Function that returns a the value of
  Expression for the instrument list in DSCode from
  Datastream parameters are
}

