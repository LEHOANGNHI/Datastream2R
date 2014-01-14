#This file contains a list of test cases for the the Datastream2R package
#At some point this code be put into the documentation
#

dwei<-getDataStream()
timeSeriesRequest(dwei=dwei, 
                  DSCodes=c("LN#(S&PCOMZ(RI)/LAG#(S&PCOMZ(RI),1M))*1.0000"),
                  startDate=as.Date("1975-01-31"),
                  endDate=Sys.Date(),
                  frequency="M",
                  sStockList=sName,
                  aTimeSeries = aEquityIndex,
                  verbose=FALSE)

timeSeriesRequest(dwei=dwei, 
                  DSCodes=c("S&PCOMZ(DY)"),
                  startDate=as.Date("1975-01-31"),
                  endDate=Sys.Date(),
                  frequency="M",
                  sStockList=sName,
                  aTimeSeries = aEquityYield,
                  verbose=FALSE)


timeSeriesRequest(dwei=dwei, 
                  DSCodes=c("H:DE(DY)"),
                  startDate=as.Date("2000-01-31"),
                  endDate=Sys.Date(),
                  frequency="M",
                  sStockList=sName,
                  aTimeSeries = aTS,
                  verbose=FALSE)

b<-listRequest(dwei = dwei, 
            DSCode = "LFTSE100", 
            Expression = "MNEM", 
            startDate = Sys.Date())
print(b)

timeSeriesListRequest(dwei = dwei, 
                           DSCode = "LFTSE100", 
                           frequency="M",
                           Instrument = "XXXX(PE)", 
                           startDate=as.Date("2000-01-31"),
                           endDate=Sys.Date(),
                           sStockList = stn, 
                           aTimeSeries = stpr, 
                           verbose=FALSE)

