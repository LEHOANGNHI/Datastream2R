
#  Functions are:
#     getDWERequestVersion - returns the version of this package
#     getDatastream - returns a connection to the DWE server and generate S3 objects
#     listRequest - Function that returns a the value of Expression for the instrument list in DSCode
#                In Datastream AFo this is Static request on a List eg LS&PCOMP
#     timeSeriesListRequest - Function that returns a set of timeseries for an instrument list eg LS&PCOMP
#     timeSeriesRequest - Function that returns a series of timeseries for a list of instruments
#
#
#
#Release history
#
# v0.86
# Added a default option that the username and password are taken from the Windows Registry
# v0.87
# Fixed bug for when no data is returned from Datastream
# v0.88
# Tidied up code and submitted it to GitHub
# Made a default that it requests a Datastream SOAP if not provided
# v0.89
# getTimeSeriesListRequest can now handle lower case instrument codes
# v0.90
# Fixed bug for when no data returned from Datastream Return code 2 for H:DE(DY)
# Experimental version that does not have Tcl in it

#'\code{getDWERequestVersion} returns the version number of the package
getDWERequestVersion <- function(){return("Version 0.90, dated 12 Dec 2012 - No Tcl/Tk")}


##############################################################################################

#'\code{getDataStream} initialises the connection with the Datatream DWE server
#'@param dweURLwsdl The URL of the server
#'@param User The username for Datastream.  If not provided it will use the username from the windows registry 
#'@param Pass The password for Datastream.  Also sourced from Registry
getDataStream <- function(dweURLwsdl = "http://dataworks.thomson.com/Dataworks/Enterprise/1.0/webServiceClient.asmx?WSDL",
                          User=as.character("USERNAME"),
                          Pass=as.character("PASSWORD")
){
   require(SSOAP,quietly=TRUE)
   require(XMLSchema,quietly=TRUE)
   require(XML,quietly=TRUE)
   require(zoo,quietly=TRUE)
   require(xts,quietly=TRUE)
   
   dweObject <- suppressWarnings(genSOAPClientInterface(
      def= processWSDL(dweURLwsdl, verbose=FALSE)
      ,verbose=FALSE,force=TRUE))
   return(c(dwe=dweObject,User=User,Pass=Pass))
}


##############################################################################################
#'\code{listRequest} Function that returns a the value of Expression for the instrument list in DSCode from Datastream
#' parameters are 
#'@param dwei - A Datastream Client Interface object created with getDataStream
#'@param  DSCode - the constituent list for the request eg LDJSTOXX
#'@param  Expression - the data to return eg MNEM or NAME
#'@param   startDate - the start date of the timeseries
#'@param  endDate - the end date of the timeseries
#'@param  frequency - the frequency of the request
#'@param   verbose - whether to give messages during the request
#'
#'@return   returns an array of the requested information

listRequest <- function (dwei=getDataStream(), 
                         DSCode,
                         Expression="",
                         startDate,
                         endDate=Sys.Date(),
                         frequency="D",
                         verbose=FALSE) {

   #
   #    TODO:  
   #      1) Error checking of invalid input parameters
   #      2) Enforce type of parameters
   
   #Create the request objects
   #first replace the '~' character with '~~'
   DSCode<-gsub("~","~~",DSCode)
   instrumentCode <- paste(toupper(DSCode),"~LIST~=",toupper(Expression),
                           "~",format(startDate,format="%Y-%m-%d"),
                           "~AA~NA=NaN~#",dwei$User, sep="")
   ud <-new("UserData", Username = paste("DS:",dwei$User,sep=""), Password = dwei$Pass)
   rd <- new("RequestData", Source = "Datastream", Instrument = instrumentCode,
             Fields = as(c(""), "ArrayOfString"))
   urrf <- new("User.Request.RequestFlags", User = ud, Request = rd, RequestFlags = 0L) 
   
   # Make the request from Datastream DWE
   dweObject <- dwei$dwe
   #SSOAP has been improved in 0.9.0 to take named parameters
#    resultsXML <- dweObject@functions$RequestRecordAsXml(User=ud,
#                                                         Request=rd,
#                                                         RequestFlags=0L,
#                                                          .convert = FALSE)
   resultsXML <- dweObject@functions$RequestRecordAsXml(parameters = urrf, .convert = FALSE)
   
   # Process the response
   docXML = xmlRoot(xmlTreeParse(resultsXML$content))
   fieldsXML <- docXML[["Body"]][["RequestRecordAsXmlResponse"]][["RequestRecordAsXmlResult"]][["Record"]][["Fields"]]
   valuesXML <-  fieldsXML[[Expression]]
   
   # Extract the data into an array
   values <- xmlSApply(valuesXML,function(node) xmlSApply(node,xmlValue))
   
   # Return the data
   return(values)
}


##############################################################################################
#'\code{timeSeriesListRequest} Function that returns a timeseries from Datastream constituent list
#' parameters are 
#'@param   dwei - A Datastream Client Interface object created with getDataStream
#'@param   DSCode - the constituent list requested eg 'LFTSE100'
#'@param    Instrument - the expression to return for each member of constituent list
#'@param    startDate - the start date of the timeseries
#'@param    endDate - the end date of the timeseries
#'@param    frequency - the frequency of the request
#'@param    verbose - whether to give messages during the request
#'
#'@return   whether the request has been successful
#'    , but also
#'    in sStockList: a list a two element vector of the displayname and symbol for each timeseries
#'    in aTimeseries: a list of class xts with the requested timeseries information

timeSeriesListRequest <- function (dwei=getDataStream(), 
                                   DSCode,
                                   Instrument,
                                   startDate,
                                   endDate=Sys.Date(),
                                   frequency="D",
                                   sStockList,
                                   aTimeSeries,
                                   verbose=FALSE) {

   
   constituents <- listRequest(dwei=dwei,
                               DSCode=DSCode,
                               Expression="MNEM",
                               startDate=startDate,
                               verbose=verbose)
   
   ret <- timeSeriesRequest(dwei=dwei,
                            DSCodes=constituents,
                            Instrument=Instrument,
                            startDate=startDate,
                            endDate=endDate,
                            frequency=frequency,
                            sStockList=sST,
                            aTimeSeries=aTS,
                            verbose=verbose)
   
   eval.parent(substitute(sStockList <- sST))
   eval.parent(substitute(aTimeSeries <- aTS))
   return(ret)
}


###############################################################################################
#'\code{timeSeriesRequest} Function that returns a timeseries from Datastream
#' parameters are 
#'@param    dwei - A Datastream Client Interface object created with getDataStream
#'@param    DSCodes - one or more codes to return, eg "MKS" or c("MKS","SAB")
#'@param    Instrument - the instrument or expression to return eg PCH#(XXXX,1M) 
#'@param    startDate - the start date of the timeseries
#'@param    endDate - the end date of the timeseries
#'@param    frequency - the frequency of the request
#'@param    verbose - whether to give messages during the request
#'

#'@return    whether the request has been successful
#'    in sStockList: a list a two element vector of the displayname and symbol for each timeseries
#'    in aTimeseries: a list of class xts with the requested timeseries information
timeSeriesRequest <- function (dwei=getDataStream(), 
                               DSCodes="",
                               Instrument="",
                               startDate=Sys.Date(),
                               endDate=Sys.Date(),
                               frequency="D",
                               sStockList,
                               aTimeSeries,
                               verbose=FALSE) {

   # Check the parameters are valid
   #   TODO: check if dwei is valid
   # if(class(dwei)=="SOAPClientInterface") { return(FALSE) }
   
   require(SSOAP,quietly=TRUE)
   require(XMLSchema,quietly=TRUE)
   require(XML,quietly=TRUE)
   require(zoo,quietly=TRUE)
   require(xts,quietly=TRUE)
   
   #Create the request objects
   ud <-new("UserData", Username = paste("DS:",dwei$User ,sep=""), Password = dwei$Pass)
   
   ################
   # First create an array of request objects.  The format will depend on whether:
   #   1) Instrument is blank, 
   #   2) a datatype, 
   #   3) or an expression where XXXX is replaced with the mnemonic.  (This is a datastream expression but using XXXX instead of X)
   #
   #
   Instrument<-toupper(Instrument)
   #  Case: Instrument is blank
   if(verbose==TRUE){cat(paste("Instrument is",Instrument, "\n"))}
   
   if(Instrument == ""){
      if(verbose==TRUE){cat("Option: Instrument is blank", "\n")}  
      instrumentCode <- lapply(DSCodes,function(x) paste(toupper(x),
                                                         "~",format(startDate,format="%Y-%m-%d"),
                                                         "~:",format(endDate,format="%Y-%m-%d"), 
                                                         "~", frequency, 
                                                         "~AA~NA=NaN~#",dwei$User, sep=""))
   }
   else{
      #first replace the '~' character used in exchange rate conversions with '~~'
      Instrument<-gsub("~","~~",Instrument)
      
      if(grepl(pattern="XXXX", x=Instrument,fixed=TRUE) == FALSE){
         # Case: Instrument contains a series of datatypes
         #
         if(verbose==TRUE){cat("Option: instrument is datatypes", "\n")}
         instrumentCode <- lapply(DSCodes,function(x) paste(toupper(x),
                                                            "~=",Instrument,
                                                            "~",format(startDate,format="%Y-%m-%d"),
                                                            "~:",format(endDate,format="%Y-%m-%d"), 
                                                            "~", frequency, 
                                                            "~AA~NA=NaN~#",dwei$User, sep=""))    
      }
      else{  
         # Case: Get a list of strings that have replaced the 'XXXX' in Instrument with the Mnemonic of the stock
         if(verbose==TRUE){cat("Option: instrument is an expression", "\n")}
         codes <- lapply(DSCodes, function(x) gsub(pattern="XXXX",replacement=x,x=Instrument,fixed=TRUE))
         
         instrumentCode <- lapply(codes,function(x) paste(toupper(x),
                                                          "~",format(startDate,format="%Y-%m-%d"),
                                                          "~:",format(endDate,format="%Y-%m-%d"), 
                                                          "~", frequency, 
                                                          "~AA~NA=NaN~#",dwei$User, sep=""))
      }
   }
   
   # Take instrumentCode and create the request object as well as a mapping of codes to InstrumentCodes
   #  print(paste("InstrumentCode is",instrumentCode))
   rd <- lapply(instrumentCode,function(x) new("RequestData", 
                                               Source = "Datastream", 
                                               Instrument = x,
                                               Fields = as(c(""), "ArrayOfString")))
   
   instrumentCodeMap<-list(code=as.character(DSCodes),instruments=as.character(instrumentCode))
   
   if(verbose==TRUE){cat("Instrument code map\n")}
   if(verbose==TRUE){cat(format(instrumentCodeMap))}
   if(verbose==TRUE){cat("\n")}
   urrf <- new("User.Requests.RequestFlags", 
               User = ud, 
               Requests = new("ArrayOfRequestData",rd), 
               RequestFlags = 0L)
   
   
   # Now we need to make the request using the RequestRecordsAsXml function
   # Make the request from Datastream DWE
   if(verbose==TRUE){cat("Make request\n")}
   dweObject <- dwei$dwe
   
   #SSOAP 0.9.0 now takes named parameters
#    response <- dweObject@functions$RequestRecordsAsXml(User=ud,
#                                                        Request=rd,
#                                                        RequestFlags=0L,
#                                                        .convert = FALSE)
   
   response <- dweObject@functions$RequestRecordsAsXml(parameters = urrf, .convert = FALSE)
   if(verbose==TRUE){cat("Get message content\n")}
   resultsXML<-response$content
   rm(response)  # test if this helps with memory usage
   gc() # Garbage clear
   if(verbose==TRUE){cat("Process content\n")}
   
   
   ourBranches <- function(){
      # Need to setup the environment (ie workspace in which data is stored)
      seriesNames <- new.env() #environment for the names and codes of the series
      tS <- new.env()  #environment for the timeSeries
      tS[["count"]]<-0
      
      #########
      #
      # We define a function that will process each chunk of the response
      Record <- function(x, ...) {
         # In this function we are processing the contents of the Record node
         # First check if we the request was successful
         status <- xmlValue(x[["StatusCode"]])
         if(verbose==TRUE){cat("Status of response ", status)}
         
         # Status of Connected - so we have fields to process
         # Get the name and code  of the series
         
         instrument <- xmlValue(x[["Instrument"]])
         
         code <- getCodeFromInstrument(instrument,instrumentCodeMap)

         
         if(verbose==TRUE){cat(" and processing ", instrument, " with code ", code,"\n")}
         if(status==0){
            value <- xmlValue(x[["Fields"]][["DISPNAME"]])
            
            seriesNames[[code]] <- value
            
            
            rm(instrument)
            rm(value)
            # Now get the dates and values returned as xts timeseries
            datesXML <- x[["Fields"]][["DATE"]]
            tmpdts <-as.Date(xmlSApply(datesXML,getNodesValue))
            # Unfortunately DWE can return the value in a node that changes its name.   
            # My approach is to find the node that does not match any of the other possible nodes 
            # (ie "CCY","DATE","DISPNAME","FREQUENCY","SYMBOL")
            # and assume the other node contains the values
            
            # q is an vector that contains NA where this unmatched node is
            q <- match(names(xmlChildren(x[["Fields"]]))
                       ,c("CCY","DATE","DISPNAME","FREQUENCY","SYMBOL"))
            # get the index of the NA value
            valNode <- max(is.na(q)*seq(along=q))
            rm(q)
            pricesXML <-  x[["Fields"]][[valNode]]    
            rm(valNode)
            # create the timeseries
            tmpval<-as.numeric(xmlSApply(pricesXML,function(node) xmlSApply(node,xmlValue)))    
            t <- xts(tmpval,tmpdts)
            names(t) <- code
            if(verbose==TRUE){cat("Extracted timeseries\n")}
            if(verbose==TRUE){cat(paste0("Class ",class(t),"\n"))}
            
            if(verbose==TRUE){print(head(t))}
            if(verbose==TRUE){cat("\n")}            
            #now free up memory
            rm(tmpval)
            rm(tmpdts)
            rm(pricesXML)
            rm(datesXML)    
            
         }
         else
         {
            # This means the status code is a failure for some reason
            # So we want to fill in with 'dummy' columns
            
            seriesNames[[code]] <- code     
            rm(instrument)
            if(verbose==TRUE){cat("No data returned for ", code, "\n")}
            # Create an empty xts object with just the startDate.  The missing dates can be merged
            # in 
            t<-xts(NA,startDate)
            names(t) <- code
         }
         # put this xts timeseries into a list and store it      
         stockCount<- tS[["count"]]
         stockCount<-stockCount+1
         z<- tS[["ts"]]
         z[[stockCount]]<-t
         tS[["ts"]] <- z
         tS[["count"]] <- stockCount
         rm(code,t,z)
         rm(stockCount)
         if(verbose==TRUE){cat(" size ", object.size(tS[["ts"]]),"\n")}
         if(verbose==TRUE){cat("Memory size", memory.size(),"\n")}
         gc()
      }
      
      # These convience functions are used to return data from the environment
      getSeriesNames <- function() as.list(seriesNames)
      getTimeSeries <- function() {
         # We merge all the timeseries into one single timeseries at this point
         if(verbose==TRUE){cat("Getting timeseries array...")}
         m <- do.call(merge,tS[["ts"]])
         if(verbose==TRUE){cat("...done\n")}
         return(m)
      }
      getCount <- function() as.integer(tS[["count"]])
      getTS <- function() return(tS[["ts"]])
      # Final element is a list of functions to be used by Branches
      free <- function() {
         rm(list=ls(name=tS),envir=tS)
         rm(list=ls(name=seriesNames),envir=seriesNames)
         
      }
      list(Record=Record, 
           getSeriesNames=getSeriesNames, 
           getTimeSeries=getTimeSeries,
           getCount=getCount,
           getTS=getTS,
           free=free)
      
   }
   #
   #
   # End of function definition
   #
   ##########
   
   branches<- ourBranches()
   
   #  Now back to the main line of code
   #  Start processing the DWE response 
   if(verbose==TRUE){cat("Parsing response\n")}
   doc <- invisible(xmlEventParse(resultsXML, handlers=list(),
                                  branches=branches,
                                  useTagName=FALSE, addContext = FALSE,asText=TRUE))
   
   # Unused is a convenience function that returns the number of timeseries
   # branches$getCount()
   if(verbose==TRUE){cat("Returning timeseries\n")}
   eval.parent(substitute(sStockList <- branches$getSeriesNames()))
   eval.parent(substitute(aTimeSeries <- branches$getTimeSeries()))
   tS<-branches$getTS()

   # Now release objects
   if(verbose==TRUE){cat("Releasing objects\n")}
   branches$free()
   rm("branches") 
   rm(resultsXML)
   gc()
   return(instrumentCodeMap)
   
}

#'\code{getNodesValue} internal helper function
getNodesValue <-function(node) {
   if(! is.null(node)) {
      return(xmlSApply(node,xmlValue))
   }
   else
   {return(NA)}
}

#'\code{getCodeFromInstrument} internal helper function
getCodeFromInstrument <- function(instrument = "",key) {
   
   return(key$code[!is.na(match(key$instruments,instrument))*seq(along=key$instruments)])
}

