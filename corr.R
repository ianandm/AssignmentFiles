corr <- function(directory, threshold){
    #get the list of complete cases
    cVec <- numeric()
    completeCases <- complete(directory, 1:332)
    
    thCases <- subset(completeCases, nobs>threshold )
    
    if(nrow(thCases)==0){
        cVec <- numeric()
    }
    else{
        for(i in 1:nrow(thCases)){
            filename <- formfilename(thCases[i,1])
            file.data <- read.csv(paste(paste(directory,"/", sep=""),filename,sep=""), header=TRUE)
            
            file.datasubset <- subset(file.data, ((!is.na(nitrate))&(!is.na(sulfate))))
            
            sul <- file.datasubset["sulfate"]
            
            nit <- file.datasubset["nitrate"]
            
            corl <- cor(sul,nit,use="complete.obs")
            cVec <- c(cVec,corl)
            
        }
    }
    
    return(cVec)
    
}

formfilename <- function(id){
    flname <- NA
    counter <- 0
    for (i in id)
    {
        
        if (i < 10){
            fname <- paste("00",i,sep="")
        }
        else if(i <100){
            fname <- paste("0",i,sep="")
        }
        else if(i<1000){
            fname <- toString(i)
        }
        else if(i<10000){
            fname <- toString(i)  
            
        }
        counter <- counter+1
        flname[counter] <- paste(fname,".csv",sep="")
    }
    
    return(flname)
}