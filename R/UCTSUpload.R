#' @import RCurl
#' @import xts
#' 
#' 
#' @title Encrypt the Datastream password
#' @details This is a port of the VBA code
#' 
#' @param strPassword the password to be encrypted
#' @return an encrypted password
#' 

EncryptPassword <- function(strPassword=""){
   
   
   iSeed <- as.raw(199L) # arbitrary number
   strCrypted <- ""
   bBytes <- charToRaw(strPassword)
   
   for(b in bBytes)
   {
      iCryptedByte <- as.raw(xor(b , iSeed))
      strCrypted <- paste0(strCrypted, formatC(as.integer(iCryptedByte),digits=3,width=3,flag="0"))
      # add previous byte, XOR with arbitrary value
      iSeed <- xor(as.raw((as.integer(iSeed) + as.integer(iCryptedByte)) %% 255L), as.raw(67L))
   }
   
   return(strCrypted)
}




#' @title convert xts timeseries into a string that can be sent to the Datastream server
#' 
#' @param Data the xts timeseries to be converted 
#' @param freq the frequency of the data
#' @param digits the number of decimal places to round the data to
#' @param NA_VALUE the string to replace NA data with
#' 
#' @return A string of the core data of Data
#' 
#' 
#' @import RCurl
#' @import xts

getTimeseries <- function(Data, freq, digits, NA_VALUE){
   
   if (freq == "D")
   {
      
      # We have a daily frequency, which means we need to do more work matching up the dates as 
      # Datastream assumes that they are in weekday order.  The loaded timeseries might have gaps or weekend
      # measures
      # the xts .indexwday gives the day of the week with 0=Sunday and 6=Saturday
      
      # We need to make sure there are no blanks in the data
      startDate <- index(first(Data))
      endDate <- index(last(Data))
      NADates <- seq(from=startDate, to=endDate, by="days") 
      NAData <- zoo(c(NA), order.by=NADates)
      #merge and fill missing rows with NAs
      wData <- merge(Data,NAData, fill=NA)
      
      
      # This only picks the weeksdays from the original series
      #wData <- Data[which(.indexwday(Data) %in% 1:5),1]
      wData <- wData[which(.indexwday(wData) %in% 1:5),1]
   }else{
      wData <- Data
      #If we do not have a daily frequency then we can just load up the datapoints, with the implicit
      #assumption that they are in the right frequency
   }
   sFormattedData <- format(wData,digits=0,nsmall=digits,trim=TRUE)
   #We need to make sure that any missing data is replaced with the 
   # the correct symbol

   #sFormattedData[which(is.na(sFormattedData))] <- NA_VALUE
   sFormattedData[which(sFormattedData=="NaN")] <- NA_VALUE
   #Collapse the array into a string
   sData<-paste0(sFormattedData,collapse=",")     
   sData<-paste0(sData,",")

   return(sData)
}



#' @title Upload a UCTS timeseries into Datastream
#'
#' @param tsData - an xts (or timeseries object that can be converted to one) to be uploaded. 
#' @param TSCode  The mnemonic of the target UCTS
#' @param MGMTGroup Must have managment group.  Only the first characters will be used.
#' @param freq The frequency of the data to be uploaded
#' @param seriesName the name of the series
#' @param Units Units of the data - can be no more than 12 characters - excess will be trimmed to that length
#' @param Decimals Number of Decimals in the data - a number between 0 and 9 - if outside that range then trimmed
#' @param ActPer Whether the values are percentages ("N") or actual numbers ("Y")
#' @param freqConversion How to do any FX conversions
#' @param Alignment Alignment of the data within periods
#' @param Carry whether to carry data over missing dates
#' @param PrimeCurr the currency of the timeseries
#' @param strUsername your Datastream username
#' @param strPassword your Datastream Password
#' @param strServerName URL of the Datastream server
#' @param strServerPage page on the datastream server
#'
#' @export
#' 
#' @import RCurl
#' @import xts
#' 
UCTSUpload <- function(tsData,
                       TSCode="", 
                       MGMTGroup="ABC", 
                       freq = c("D","W","M","Q","Y"),
                       seriesName,
                       Units="",
                       Decimals=2,
                       ActPer=c("N","Y"),
                       freqConversion= c("ACT","SUM","AVG","END"),
                       Alignment=c("1ST","MID","END"),
                       Carry=c("YES","NO","PAD"),
                       PrimeCurr="",
                       strUsername=options()$Datastream.Username,
                       strPassword=options()$Datastream.Password,
                       strServerName="http://product.datastream.com",
                       strServerPage="/UCTS/UCTSMaint.asp"){

   #Check inputs are valid
   if(!freq %in% c("D","W","M","Q","Y")){
      stop("freq is not an allowed value")
   }

   if(!ActPer %in% c("N","Y")){
      stop("ActPer is not an allowed value")
   }
   
   if(!freqConversion %in% c("ACT","SUM","AVG","END")){
      stop("freqConversion is not an allowed value")
   }
   
   if(!Alignment %in% c("1ST","MID","END")){
      stop("Alignment is not an allowed value")
   }
   
   if(!Carry %in% c("YES","NO","PAD")){
      stop("Carry is not an allowed value")
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

   
   # Add Start Date for values - make sure it is in DD/MM/YY format
   #CMC actually the function returns a dd/MM/yyyy format post Y2K
   # convert to xts object
   xtsData <- as.xts(tsData)
   startDate <- index(first(xtsData)) 
   endDate <- index(last(xtsData))
   
   # Now create the URL to post the form to
   dsURL <- paste0(strServerName , strServerPage , "?UserID=" , strUsername) 

   
   # Create a list of the parameters to be uploaded
   # We have not included the pair  AmendFlag="Y", so all these will be full updates
   
   dsParams <- list(CallType="Upload",
                    TSMnemonic=toupper(TSCode),
                    TSMLM=toupper(MGMTGroup),
                    TSStartDate=format(startDate,format="%d/%m/%Y"),
                    TSEndDate=format(endDate,format="%d/%m/%Y"),
                    TSFrequency=freq,
                    TSTitle=seriesName,
                    TSUnits=Units,
                    TSDecPlaces=Decimals,
                    TSAsPerc=ActPer,
                    TSFreqConv=freqConversion,              # Add "Frequency Conversion" 
                    TSAlignment=Alignment,                  # Add "Alignment"
                    TSCarryInd=Carry,                       # Add "Carry Indicator"
                    TSPrimeCurr=PrimeCurr,                  # Add "Prime Currency"
                    TSULCurr="",                            # no longer use Underlying Currency, but need to pass up a null value as the mainframe is expecting it
                    ForceUpdateFlag1="Y",
                    ForceUpdateFlag2="Y",                   # We have ignored some logic in the original UCTS VBA code
                    TSValsStart=format(startDate,format="%d/%m/%Y"),  #TODO adjust this date according to the frequency of the data VBA function AdjustDateTo1st
                    NAValue=NA_VALUE,
                    TSValues=getTimeseries(xtsData, 
                                           freq= freq, 
                                           digits=Decimals, 
                                           NA_VALUE),           #Now add the datapoints - the date element of the series is discarded here, with obvious risks
                    UserOption=EncryptPassword(strPassword)
   )
   
   
   # Now post the form
   # We will give it three tries
   iCounter <- 1
   retValue <- ""
   
   while(iCounter <3 && retValue != "*OK*"){
   retValue <- postForm(uri=dsURL, 
                        .params=dsParams,
                        style="POST",
                        .encoding="utf-8",
                        .contentEncodeFun=curlPercentEncode)
   iCounter <- iCounter + 1 
   }
   if(retValue[1]=="*OK*"){
      return(TRUE)
   }
   else{
      return(retValue[1])
   }
}