\name{getDataStream}
\alias{getDataStream}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getDataStream(dweURLwsdl = "http://dataworks.thomson.com/Dataworks/Enterprise/1.0/webServiceClient.asmx?WSDL", User = as.character(tcl("registry", "get", "HKEY_CURRENT_USER\\Software\\Datastream\\DSAccess\\Login1", "USERID")), Pass = as.character(tcl("registry", "get", "HKEY_CURRENT_USER\\Software\\Datastream\\DSAccess\\Login1", "PASSWORD")))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dweURLwsdl}{
%%     ~~Describe \code{dweURLwsdl} here~~
}
  \item{User}{
%%     ~~Describe \code{User} here~~
}
  \item{Pass}{
%%     ~~Describe \code{Pass} here~~
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
function (dweURLwsdl = "http://dataworks.thomson.com/Dataworks/Enterprise/1.0/webServiceClient.asmx?WSDL", 
    User = as.character(tcl("registry", "get", "HKEY_CURRENT_USER\\Software\\Datastream\\DSAccess\\Login1", 
        "USERID")), Pass = as.character(tcl("registry", "get", 
        "HKEY_CURRENT_USER\\Software\\Datastream\\DSAccess\\Login1", 
        "PASSWORD"))) 
{
    require(SSOAP, quietly = TRUE)
    require(XMLSchema, quietly = TRUE)
    require(XML, quietly = TRUE)
    require(zoo, quietly = TRUE)
    require(xts, quietly = TRUE)
    require(tcltk, quietly = TRUE)
    dweObject <- suppressWarnings(genSOAPClientInterface(def = processWSDL(dweURLwsdl, 
        verbose = FALSE), verbose = FALSE))
    return(c(dwe = dweObject, User = User, Pass = Pass))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
