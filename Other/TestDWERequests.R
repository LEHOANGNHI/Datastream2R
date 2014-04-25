##############################################################################################
#
#
#
#
##############################################################################################
dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)

stock <- c("MKS")
datatype = "MV"
startDate <- as.Date("31/12/2011", "%d/%m/%Y")
endData <- Sys.Date()
#endDate <- as.Date('24/01/2012', '%d/%m/%Y')

timeSeriesRequest(dwei = dwei, DSCode = stock, 
    Instrument = datatype, startDate = startDate, sStockList = stn, aTimeSeries = stpr)
plot(stpr, main = names(stn[1]))
#
#
##############################################################################################
dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)
stock <- c("MKS","RIO")
datatype = "MV"
startDate <- as.Date("31/12/2011", "%d/%m/%Y")
endData <- Sys.Date()
#endDate <- as.Date('24/01/2012', '%d/%m/%Y')

c<-timeSeriesRequest(dwei = dwei, DSCode = stock, 
                  Instrument = datatype, startDate = startDate, sStockList = stn, aTimeSeries = stpr)
plot(stpr, ylab = stn[[1]][1])
#
#
#
#######################################################################################
dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)
Instrument <- c("PCH#(USCONPRCF,12M)")

startDate <- as.Date("31/12/1960", "%d/%m/%Y")
endData <- Sys.Date()
#endDate <- as.Date('24/01/2012', '%d/%m/%Y')

a <- timeSeriesRequest(dwei = dwei, DSCode = Instrument, 
    startDate = startDate, frequency = "M", sStockList = stn, aTimeSeries = stpr)
plot(stpr[,1], main = names(stn[1]))
#
#
#
#
#########################################################################################
dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)
stockList <- "LFTSE100"
sCode <- "MNEM"
b <- listRequest(dwei = dwei, DSCode = stockList, 
    Expression = sCode, startDate = startDate)
print(b)
#
#
#
#
##############################################################################
dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)
startDate <- as.Date("31/12/2011", "%d/%m/%Y")
endData <- Sys.Date()
stockList <- c("SAB", "RIO", "MKS")
sCode <- "P"
c <- timeSeriesRequest(dwei = dwei, DSCode = stockList, 
    Instrument = sCode, startDate = startDate, sStockList = stn, aTimeSeries = stpr)
plot(stpr[,1], main = names(stn[1]))
plot(stpr[,2], main = names(stn[2]))
#
#
#
#
####################################################################################
dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)
startDate <- as.Date("31/12/2011", "%d/%m/%Y")
endData <- Sys.Date()
stockList <- "LFTSE100"
sCode <- "P"
c <- timeSeriesListRequest(dwei = dwei, DSCode = stockList, 
    Instrument = sCode, startDate = startDate, sStockList = stn, aTimeSeries = stpr)
plot(stpr[,1], main = names(stn[1]))
plot(stpr[,2], main = names(stn[2]))
#
#
#
#
####################################################################################
dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)
startDate <- as.Date("31/12/1990", "%d/%m/%Y")
endData <- Sys.Date()
stockList <- "LS&PCOMP"
sCode <- "P"
c <- timeSeriesListRequest(dwei = dwei, DSCode = stockList , frequency="D",
                           Instrument = sCode, startDate = startDate, sStockList = stn, aTimeSeries = stpr)
plot(stpr[,1], main = names(stn[1]))
plot(stpr[,2], main = names(stn[2]))
#
#
#
#
#####################################################################################



timeSeriesRequest(dwei=dwei, 
                  DSCodes=c("J:NKSJ"),
                  startDate=as.Date("2009-12-14"),
                  endDate=as.Date("2012-12-14"),
                  frequency="W",
                  sStockList=sName,
                  aTimeSeries = aTS,
                  verbose=TRUE)

#############################################################
# Test of requesting the same thing twice
dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)

stock <- c("MKS", "MKS")
datatype = "MV"
startDate <- as.Date("31/12/2011", "%d/%m/%Y")
endData <- Sys.Date()
#endDate <- as.Date('24/01/2012', '%d/%m/%Y')

timeSeriesRequest(dwei = dwei, DSCode = stock, 
                  Instrument = datatype, startDate = startDate, sStockList = stn, aTimeSeries = stpr, verbose=TRUE)


#############################################################
# Test of requesting the same thing twice
dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)

stock <- c("MKS", "D:BASF")
datatype = "MV"
startDate <- as.Date("31/12/2011", "%d/%m/%Y")
endData <- Sys.Date()
#endDate <- as.Date('24/01/2012', '%d/%m/%Y')

timeSeriesRequest(dwei = dwei, DSCode = stock, 
                  Instrument = datatype, startDate = startDate, sStockList = stn, aTimeSeries = stpr)


