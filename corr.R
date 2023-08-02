corr <- function(directory, threshold=0){
  
  # Combine all .csv files in the directory into one data frame 
  # for selected monitor ids
  
  numfiles <- length(list.files(directory, pattern="*.csv"))
  full_names <- list.files(directory, full.names = TRUE)
  dat <- data.frame()
  for(i in numfiles){
    dat <- rbind(dat, read.csv(full_names[i]))
  }
  
  # subset dat to only include complete data rows
  
  completedata <- subset(dat, (!is.na(dat$sulfate)) & (!is.na(dat$nitrate)))
  
  # get vector of number of complete cases for each id
  nobs <- c()
  for(i in 1:length(numfiles)){
    nobs[i] <- length(completedata$ID[completedata$ID == numfiles[i]])
  }
  
  # make a logical vector for if nobs exceeds threshold
  sel <- nobs >= threshold
  monitors <- which(sel %in% TRUE)
}