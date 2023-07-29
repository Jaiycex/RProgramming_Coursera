complete <- function(directory, id = 1:332) {
  
  # Combine all .csv files in the directory into one data frame 
  # for selected monitor ids
  
  numfiles <- length(list.files(directory, pattern="*.csv"))
  full_names <- list.files(directory, full.names = TRUE)
  dat <- data.frame()
  for(i in id){
    dat <- rbind(dat, read.csv(full_names[i]))
    }
  
  # subset dat to only include complete data rows
 
  completedata <- subset(dat, (!is.na(dat$sulfate)) & (!is.na(dat$nitrate)))

  # get vector of number of complete cases for each id
  nobs <- c()
  for(i in 1:length(id)){
    nobs[i] <- length(completedata$ID[completedata$ID == id[i]])
  }
 
  # make new data frame with id and number of complete cases
  newdata <- as.data.frame(cbind(id,nobs))
  print(newdata)
}