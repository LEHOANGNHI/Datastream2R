Datastream2R
============

This is a series of functions for downloading data from the Thomson Reuters DataStream DWE server, which provides XML access to the Datstream database of economic and financial information.


Datastream is a paid-for service that provides financial and economic data.

This class recreates the Static, Timeseries and Timeseries List requests that are available in Excel with the 'Datastream for Office' application.

This package uses SSOAP, which is not available from CRAN and must be downloaded and installed using:

       install.packages("XMLSchema", repos = "http://www.omegahat.org/R",
                        dependencies = TRUE,
                        type = "source")
       install.packages("SSOAP", repos = "http://www.omegahat.org/R",
                        dependencies = TRUE,
                        type = "source")


