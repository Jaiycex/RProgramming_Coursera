pollutantmean <- function(directory, pollutant, id = 1:332) {

# Combine all .csv files in the directory into one data frame 
# for selected monitor ids
  
numfiles <- length(list.files(directory, pattern="*.csv"))
full_names <- list.files(directory, full.names = TRUE)
dat <- data.frame()
for(i in id){
  dat <- rbind(dat, read.csv(full_names[i]))
}

# calculate the mean of either sulfate or nitrate pollution for specified monitors

mean(dat[,pollutant], na.rm = TRUE)

  }