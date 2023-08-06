function(directory, threshold = 0){
  
  # Combine all .csv files in the directory into one data frame 
  
  files <- list.files(directory, pattern="*.csv", full.names = TRUE)
  
  # go through files and add to vector if exceeds threshold
  corvec <- numeric()
  for (i in 1:332) {
    data <- na.omit(read.csv(files[i]))
    nobs <- sum(complete.cases(data))
    
    
    if (nobs > threshold){
    
    csulfate <- (data[,2])
    cnitrate <- (data[,3])
    
    corvec <- c(corvec, cor(csulfate,cnitrate))
    }
  }
  return(corvec)
}