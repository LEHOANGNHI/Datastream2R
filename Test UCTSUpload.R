require(Datastream2R)
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

message("Unit tests passed")