# Create the message that will be uploaded onto the server
getPostData <- function(Code, 
                        MGMTGroup="ABC", 
                        startDate, 
                        endDate, 
                        freq = c("D","W","M","Q","Y"),
                        seriesName,
                        Units="",
                        Decimals=2,
                        ActPer=c("N","Y"),
                        freqConversion= c("ACT","SUM","AVG","END"),
                        Alignment=c("1ST","MID","END"),
                        Carry=c("YES","NO","PAD"),
                        PrimeCurr="",
                        tsData,
                        strPassword){
   #Check inputs are valid
   if(!freq %in% c("D","W","M","Q","Y")){
      stop("freq is not an allowed value")
   }
   
   # Limit decimals a number in range to the range 0-9
   if(!is.numeric(Decimals)) Decimals <- 2L
   Decimals <- as.integer(Decimals)
   if(Decimals < 0) Decimals <- 0
   if(Decimals > 9) Decimals <- 9
   
   
   # Trim any excess for units
   Units <- substr(Units,0,12)
   
   # At the moment everything will be a full update, and a hard coded NA value
   NA_VALUE <- "NA"
   bPartialUpdate <- FALSE
   
   
   # Now to build the request string
   
   # Add the call type to the string
   strPostData <- "CallType=Upload";
   strPostData <- paste0(strPostData , "&TSMnemonic=" , toupper(Code))
   
   # Add MLM to string
   strPostData <- paste0(strPostData , "&TSMLM=" , toupper(MGMTGroup))
   
   # Add Start Date to string - make sure it is in DD/MM/YY format
   # dtDate = ConvertDateToString(Cells(Range("Start_Date").Row, SelCell.Column), Cells(Range("Date_Align.").Row, SelCell.Column).Value)    
   strPostData <- paste0(strPostData , "&TSStartDate=" , format(startDate,format="%d/%m/%Y"))
   
   
   # Add End Date to string - make sure it is in DD/MM/YY format
   # dtDate = ConvertDateToString(Cells(Range("End_Date").Row, SelCell.Column), Cells(Range("Date_Align.").Row, SelCell.Column).Value)
   strPostData <- paste0(strPostData , "&TSEndDate=" , format(endDate,format="%d/%m/%Y"))
   
   # Add Frequency to string
   strPostData <- paste0(strPostData , "&TSFrequency=" , freq)
   
   #Add Title to string
   strPostData <- paste0(strPostData , "&TSTitle=" , curlPercentEncode(seriesName))
   
   # Add "Units" value to string
   strPostData <- paste0(strPostData , "&TSUnits=" , Units)
   
   # Add "Decimal Places" value to string
   strPostData <- paste0(strPostData , "&TSDecPlaces=" , Decimals)
   
   # Add "Actual/Percentage" value to string
   strPostData <- paste0(strPostData , "&TSAsPerc=" , ActPer)
   
   # Add "Frequency Conversion" value to string
   strPostData <- paste0(strPostData , "&TSFreqConv=" , freqConversion)
   
   # Add "Alignment" value to string
   strPostData <- paste0(strPostData , "&TSAlignment=" , Alignment)
   
   # Add "Carry Indicator" value to string
   strPostData <- paste0(strPostData , "&TSCarryInd=" , Carry)
   
   # Add "Prime Currency" value to row
   strPostData <- paste0(strPostData , "&TSPrimeCurr=" , PrimeCurr)
   
   
   # no longer use Underlying Currency, but need to pass up a null value as the mainframe is expecting it
   strPostData <- paste0(strPostData , "&TSULCurr=")
   
   # If the Update Type is "Partial", the upload will be submitted as an amendment
   if (bPartialUpdate) { 
      strPostData <- paste0(strPostData , "&AmendFlag=Y")
   }
   
   
   strPostData <- paste0(strPostData , "&ForceUpdateFlag1=Y")
   # Now set ForceUpdateFlag2...
   # I guess the original piece of code was just getting the date of the first piece of data
   
   dtSeriesStartDate <- index(first(tsData))
   
   #Not yet sure what this is doing so set to a very early date
   dtDateRangeStartDate <- as.Date("1600-01-01")
   
   if (dtSeriesStartDate >= dtDateRangeStartDate)
   {
      strPostData <- paste0(strPostData , "&ForceUpdateFlag2=Y")
   }
   else
   {
      strPostData <- paste0(strPostData , "&ForceUpdateFlag2=")
   }
   
   
   
   # Add Start Date for values - make sure it is in DD/MM/YY format
   #CMC actually the function returns a dd/MM/yyyy format post Y2K
   # convert to xts object
   xtsData <- as.xts(tsData)
   dtDate <- index(first(xtsData))
   #TODO adjust this date according to the frequency of the data VBA function AdjustDateTo1st
   strPostData <- paste0(strPostData , "&TSValsStart=" , format(dtDate,format="%d/%m/%Y"))
   
   strPostData <- paste0(strPostData , "&NAValue=" , curlPercentEncode(NA_VALUE))
   
   #Now add the datapoints - the date element of the series is discarded here, with obvious risks
   strPostData <- paste0(strPostData , "&TSValues=", getTimeseries(xtsData, 
                                                                   freq= freq, 
                                                                   digits=Decimals, 
                                                                   NA_VALUE))
   
   #Add the encrypted password to the post data
   strPostData <- paste0(strPostData , "&UserOption=" , EncryptPassword(strPassword))
   
   
   return(strPostData)
}
