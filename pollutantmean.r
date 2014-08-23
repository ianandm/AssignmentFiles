formfilename <- function(id){
  flname <- NA
  counter <- 0
  for (i in id)
  {
    
    if (i < 10){
      fname <- paste("00",i,sep="")
    }
    else{
      if (i <100){
        fname <- paste("0",i,sep="")
      }
      else{
        fname <- toString(i)
      }
    }
    counter <- counter+1
    flname[counter] <- paste(fname,".csv",sep="")
  }
  
  return(flname)
}

pollutantmean <- function(directory, pollutant, id){
  
  if(missing(id)) 
    id<-1:332
  
  flnames <- formfilename(id)
  flnames <- flnames[!is.na(flnames)]
  finaldata <- NA
  polluData <- NA
  count <- 0
  
  
  file.names <- list.files(paste(directory,"/", sep=""), full.names=FALSE)

  
  for(i in 1:length(flnames)){
    for(file in file.names){
      
      if(flnames[i]==file) {

        file.data <- read.csv(paste(paste(directory,"/", sep=""),file,sep=""))
        polluData <- file.data[pollutant]
        polluData <- polluData[!is.na(polluData)]
        finaldata <- c(finaldata[!is.na(finaldata)], polluData) 
      }
    }
  }

  return(mean(finaldata)) 
}
  
  


