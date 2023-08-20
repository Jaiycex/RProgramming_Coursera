# Gets the hospital with specified rank mortality rate
# based on given U.S state and particular negative outcome

rankhospital <- function(state, cause, num) {
  
  #get state data and sort alphabetically
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ds <- outcome[outcome$State == state,]
  if(nrow(ds) == 0){stop("invalid state")}
  
  #based on outcome, select appropriate mortality rate column
  # return associated hospital for given rank. Very unoptimized writing
  if (cause == "heart attack"){
    alphd <- ds[order(as.numeric(ds$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ds$Hospital.Name),]
    if (num == "best"){
      hname <-  head(alphd[,2], n=1)
    } else if (num == "worst"){
      hacol <-  as.numeric(alphd[,11])
      ind <- which.max(hacol)
      hname <- alphd[ind,2]
    } else hname <- alphd[num,2]
  } else if (cause == "heart failure"){
    alphd <- ds[order(as.numeric(ds$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ds$Hospital.Name),]
    if (num == "best"){
      hname <-  head(alphd[,2], n=1)
    } else if (num == "worst"){
      hfcol <-  as.numeric(alphd[,17])
      ind <- which.max(hfcol)
      hname <- alphd[ind,2]
    } else hname <- alphd[num,2]
  } else if (cause == "pneumonia"){
    alphd <- ds[order(as.numeric(ds$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ds$Hospital.Name),]
    if (num == "best"){
      hname <-  head(alphd[,2], n=1)
    } else if (num == "worst"){
      pcol <-  as.numeric(alphd[,23])
      ind <- which.max(pcol)
      hname <- alphd[ind,2]
    } else hname <- alphd[num,2]
  } else {stop("invalid outcome")}
 
   return(hname)
}