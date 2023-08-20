# Gets the hospital with lowest mortality rate
# based on given U.S state and particular negative outcome

best <- function(state, cause) {
  
  #get state data and sort alphabetically
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ds <- outcome[outcome$State == state,]
  alphd <- ds[order(ds$Hospital.Name),]
  if(nrow(ds) == 0){stop("invalid state")}
  
  #based on outcome, select appropriate mortality rate column
  # return associated hospital
   if (cause == "heart attack"){
   hacol <-  as.numeric(alphd[,11])
   ind <- which.min(hacol)
   hname <- alphd[ind,2]
  }
  else if (cause == "heart failure"){
    hfcol <-  as.numeric(alphd[,17])
    ind <- which.min(hfcol)
    hname <- alphd[ind,2]
  }
  else if (cause == "pneumonia"){
    pcol <-  as.numeric(alphd[,23])
    ind <- which.min(pcol)
    hname <- alphd[ind,2]
  }
  else {stop("invalid outcome")}
  
  return(hname)
}