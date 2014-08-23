complete <- function(directory, id){
  
  flnames <- formfilename(id)
  
  
  for(i in 1:length(flnames)){
    file.data <- read.csv(paste(paste(directory,"/", sep=""),flnames[i],sep=""), header=TRUE)
    id <- as.numeric(substr(flnames[i],1,3))
    nobs <- nrow(na.omit(file.data))
    if(i==1) 
        m<-data.frame(id, nobs)
    else
        m <- rbind(m,data.frame(id, nobs))
    
  }
  return(m)
}

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