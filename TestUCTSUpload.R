source("R\\UCTSUpload.R")
# Regression tests for th UCTSUpload routine
#
# Test the password encryption function which should return "134060072035020251227029" for "A1B2c3d5"
#
# 
stopifnot(EncryptPassword("A1B2c3d5") == "134060072035020251227029")

# Test the post string generation code

testData <- xts(x=c(1, 2.2, 3.12345, 4.5), order.by = as.Date(c("2014-04-22","2014-04-23","2014-04-24","2014-04-25")))


#Try uploading a real dataset
sPost <- UCTSUpload(TSCode="TSTEST01", 
                     MGMTGroup="TEST", 
                     freq = "D",
                     seriesName="Automatic Upload Test",
                     Units="par",
                     Decimals=2,
                     ActPer="Y",
                     freqConversion="END",
                     Alignment="MID",
                     Carry="NO",
                     PrimeCurr="U$",
                     tsData=testData,
                     strUsername=options()$Datastream.Username,
                     strPassword=options()$Datastream.Password)


sExpected <-  TRUE

stopifnot(sPost == sExpected)

message("Unit tests passed")


# Test a dataset with an NaN in it

testData <- xts(x=c(1, 2.2, 3.12345, 14.5, NaN), order.by = as.Date(c("2013-01-01","2013-02-01","2013-03-01","2013-04-01","2013-05-01")))

sPost <- getTimeseries(testData,"M",2,"NA")

sExpected <- "1.00,2.20,3.12,14.50,NA,"
stopifnot(sPost == sExpected)


#Try uploading a real dataset
sPost <- UCTSUpload(TSCode="TSTEST01", 
                    MGMTGroup="TEST", 
                    freq = "M",
                    seriesName="Automatic Upload Test",
                    Units="par",
                    Decimals=2,
                    ActPer="Y",
                    freqConversion="END",
                    Alignment="MID",
                    Carry="NO",
                    PrimeCurr="U$",
                    tsData=testData,
                    strUsername=options()$Datastream.Username,
                    strPassword=options()$Datastream.Password)


sExpected <-  TRUE

stopifnot(sPost == sExpected)

#Test the daily data

#Load some test data
load("Other\\f.RData")
fTest<-head(f$First,10)

# Test getTimeseries for the first 10 points
tData <- getTimeseries(Data=fTest, freq="D", digits=4, NA_VALUE="NA")
tDataExpected <- "0.8559,NA,NA,NA,0.8579,0.8512,0.8599,NA,NA,0.8596,NA,0.8393,0.8406,0.8274,0.8505,0.8444,"
stopifnot(tData == tDataExpected)

#Try a round trip and check if data is the same
sPost <- UCTSUpload(TSCode="TSTEST01", 
                    MGMTGroup="TEST", 
                    freq = "D",
                    seriesName="Automatic Upload Test",
                    Units="",
                    Decimals=3,
                    ActPer="Y",
                    freqConversion="END",
                    Alignment="END",
                    Carry="NO",
                    PrimeCurr="",
                    tsData=fTest,
                    strUsername=options()$Datastream.Username,
                    strPassword=options()$Datastream.Password)
stopifnot(sPost == TRUE)  #Failed to upload

#Now lets download the data
dwei <- getDataStream(User=options()$Datastream.Username, Pass=options()$Datastream.Password)
sGet <- timeSeriesRequest(dwei = dwei, 
                          DSCodes = "TSTEST01", 
                          Instrument = "",
                          startDate = index(first(fTest)), 
                          endDate = index(last(fTest)), 
                          frequency = "D",
                          sStockList = sTest, 
                          aTimeSeries = aTS, 
                          verbose = FALSE)

#So success is aTS is the same as f$First

xResult <- cbind(round(fTest,digits=3),aTS)  # Need to round to the same number of digits as in upload
colnames(xResult) <- c("Sent","Got")
stopifnot(!FALSE %in% as.vector(xResult$Sent==xResult$Got))


message("Unit tests passed")