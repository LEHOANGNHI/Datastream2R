#' 
#' 
#' 
#' @title change currency ISO code into Datastream version
#' 
#' @param isoCurrency The iso currency code to be converted to the Datastream version
#' @return the Datastream code for the currency
#' 
#' @export
iso2DatastreamCurrency <- function(isoCurrency){
   # load the table of conversions
 
   data(currencyDS2ISO)  
 #Return the matching entry
 return( first(currencyDS2ISO$dsCode[which(currencyDS2ISO$isoCode==isoCurrency)])) 
 
}

#' @title change currency ISO code into Datastream version
#' 
#' @description The use of the multiplier parameter alongs minor divisions to be obtained eg pence and cents
#' 
#' @param isoCurrency The iso currency code to be converted to the Datastream version
#' @param multiplier An addition multiplier to key using
#' @return the Datastream code for the currency
#' 
#' @export
#' 
iso2DatastreamCurrency <- function(isoCurrency,multiplier=1){
   # load the table of conversions
   
   data(currencyDS2ISO)  
   
   
   #Return the matching entry
   df <- currencyDS2ISO$dsCode[which(currencyDS2ISO$isoCode==isoCurrency & currencyDS2ISO$Multiplier==multiplier)]
   return( first(df)) 
   
}

#' @title change currency datastream code into iso version
#' 
#' @param dsCurrency The datastream currency code
#' @return the iso code for the currency
#' 
#' @export
#' 
ds2ISOCurrency <- function(dsCurrency){
   # load the table of conversions
   
   data(currencyDS2ISO)  

   #Return the matching entry
   df <- currencyDS2ISO$isoCode[which(currencyDS2ISO$dsCode==dsCurrency)]
   return( first(df)) 
   
}