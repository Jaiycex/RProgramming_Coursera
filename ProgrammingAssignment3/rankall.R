
rankall <-  function(cause, num = "best"){
  
  # read outcome data and create column index
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  cvec <- c("11", "17", "23")
  names(cvec) <- c("heart attack", "heart failure", "pneumonia")
  
  # check that outcome is valid
  if((cause %in% names(cvec)) == FALSE) stop("invalid outcome")
 
  #order and subset data 
  index <- as.numeric(cvec[cause])
  subdt <- outcome[,c(2,7,index)]
  colnames(subdt) [3] <- "rate"
  orderdt <- subdt[order(subdt$State, as.numeric(subdt$rate), subdt$Hospital.Name),]
  statedata <-split(orderdt, orderdt[,"State"])
  
  #find hospital of given rank for given cause for each state
   if (num == "best"){
     hlist <- do.call(rbind, lapply(
      statedata, function(x) {
        x[which.min(x[,3]),]
      } 
     ))
     rownames(hlist) <- NULL
   }else if (num == "worst"){
    hlist <- do.call(rbind, lapply(
      statedata, function(x) {
        x[which.max(x[,3]),]
      } 
    ))
    rownames(hlist) <- NULL
  }else 
    hlist <- do.call(rbind, lapply(
      statedata, function(x){
      x[num,]
      }
    ))
  hlist[,c(1,2)]
}
  