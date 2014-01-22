\name{getDataStream}
\alias{getDataStream}
\title{\code{getDataStream} initialises the connection with the Datatream DWE server}
\usage{
  getDataStream(dweURLwsdl = "http://dataworks.thomson.com/Dataworks/Enterprise/1.0/webServiceClient.asmx?WSDL",
    User = as.character("USERNAME"),
    Pass = as.character("PASSWORD"))
}
\arguments{
  \item{dweURLwsdl}{The URL of the server}

  \item{User}{The username for Datastream.  If not provided
  it will use the username from the windows registry}

  \item{Pass}{The password for Datastream.  Also sourced
  from Registry}
}
\description{
  \code{getDataStream} initialises the connection with the
  Datatream DWE server
}

