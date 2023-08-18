# Gets the hospital with lowest mortality rate
# based on given U.S state and particular negative outcome

best <- function(state, outcome) {
  
  #get state data and sort alphabetically
  ds <- outcome[outcome$State == state,]
  alphd <- ds[order(ds$Hospital.Name),]
  
  #based on outcome, select appropriate mortality rate column
  # return associated hospital
  if (outcome == "heart attack"){
   hacol <-  na.omit(as.numeric(alphd[,11]))
   ind <- which.min(hacol)
   hname <- alphd[ind,2]
  }
  else if (outcome == "heart failure"){
    hfcol <-  na.omit(as.numeric(alphd[,17]))
    ind <- which.min(hfcol)
    hname <- alphd[ind,2]
  }
  else if (outcome == "pneumonia"){
    pcol <-  na.omit(as.numeric(alphd[,23]))
    ind <- which.min(pcol)
    hname <- alphd[ind,2]
  }
  else { stop("invalid outcome")}
  
  return(hname)
}